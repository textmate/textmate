#import "ProjectChanges.h"
#import <document/OakDocument Private.h>
#include <xdiff.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <chrono>
#include <scm/drivers/api.h>

namespace {
constexpr NSUInteger fileLimit = 4 * 1024 * 1024;
static double Now() { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); }
static dispatch_time_t At(double when) { return dispatch_time(DISPATCH_TIME_NOW, (int64_t)(MAX(0, when-Now()) * NSEC_PER_SEC)); }
static NSDictionary* Error(NSString* message) { return @{ @"error": message }; }

// The lookup the gutter and SCM drivers use: TM_GIT, then PATH, skipping the /usr/bin/git
// shim when developer tools are missing so it cannot raise an install dialog.
static NSString* GitExecutable()
{
	static NSString* resolved;
	@synchronized(NSProcessInfo.processInfo) {
		if(!resolved) {
			std::string path = scm::find_executable("git", "TM_GIT");
			if(path != NULL_STR) resolved = @(path.c_str());
		}
		return resolved;
	}
}

static NSDictionary* Git(NSString* directory, NSArray<NSString*>* arguments)
{
	NSString* executable = GitExecutable();
	if(!executable) return Error(@"Git was not found. Install the macOS command line tools, or set TM_GIT in ~/.tm_properties.");
	NSTask* task = [[NSTask alloc] init];
	task.executableURL = [NSURL fileURLWithPath:executable];
	task.arguments = [@[@"--no-optional-locks", @"-c", @"core.fsmonitor=false", @"-C", directory] arrayByAddingObjectsFromArray:arguments];
	NSMutableDictionary* environment = [NSProcessInfo.processInfo.environment mutableCopy];
	// This view always reviews the requested directory, independently of any
	// Git overrides inherited when TextMate was launched from a shell.
	for(NSString* key in [environment.allKeys copy])
		if([key hasPrefix:@"GIT_"]) [environment removeObjectForKey:key];
	environment[@"GIT_OPTIONAL_LOCKS"] = @"0";
	environment[@"GIT_TERMINAL_PROMPT"] = @"0";
	task.environment = environment;
	NSPipe* pipe = [NSPipe pipe];
	task.standardOutput = pipe;
	task.standardError = [NSFileHandle fileHandleWithNullDevice];
	task.standardInput = [NSFileHandle fileHandleWithNullDevice];
	NSError* error;
	dispatch_semaphore_t exited = dispatch_semaphore_create(0);
	// Set before launch so an exit that beats the process monitor is still seen.
	task.terminationHandler = ^(NSTask* task){ dispatch_semaphore_signal(exited); };
	if(![task launchAndReturnError:&error]) return Error(@"Git could not be started. Install the macOS command line tools and try again.");
	[pipe.fileHandleForWriting closeFile];
	double deadline = Now()+10;

	// A readability source replaces polling: it is woken by the pipe instead of
	// sleeping in 10 ms steps while Git is quiet.
	int fd = pipe.fileHandleForReading.fileDescriptor;
	fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
	NSMutableData* output = [NSMutableData data];
	__block NSString* problem = nil;
	__block BOOL done = NO;
	dispatch_semaphore_t eof = dispatch_semaphore_create(0);
	dispatch_semaphore_t stopped = dispatch_semaphore_create(0);
	dispatch_source_t source = dispatch_source_create(DISPATCH_SOURCE_TYPE_READ, fd, 0, dispatch_queue_create("com.macromates.project-changes.git-pipe", DISPATCH_QUEUE_SERIAL));
	dispatch_source_set_event_handler(source, ^{
		if(done) return;
		char bytes[16384];
		ssize_t count = read(fd, bytes, sizeof(bytes));
		if(count > 0) {
			if(output.length + count > fileLimit) problem = @"This change is too large to preview (limit 4 MB).";
			else { [output appendBytes:bytes length:count]; return; }
		} else if(count < 0 && (errno == EAGAIN || errno == EINTR)) return;
		done = YES;
		dispatch_semaphore_signal(eof);
	});
	dispatch_source_set_cancel_handler(source, ^{ dispatch_semaphore_signal(stopped); });
	dispatch_resume(source);
	if(dispatch_semaphore_wait(eof, At(deadline))) problem = @"Git took too long to respond. Try refreshing the view.";
	// Cancelling and waiting keeps the descriptor open until no handler can read it.
	dispatch_source_cancel(source);
	dispatch_semaphore_wait(stopped, DISPATCH_TIME_FOREVER);
	// Past the size cap Git would block on the full pipe until the deadline, so stop it now.
	if(problem && task.running) kill(task.processIdentifier, SIGKILL);
	if(dispatch_semaphore_wait(exited, At(deadline))) {
		if(!problem) problem = @"Git took too long to respond. Try refreshing the view.";
		if(task.running) kill(task.processIdentifier, SIGKILL);
	}
	[task waitUntilExit];
	[pipe.fileHandleForReading closeFile];
	return problem ? Error(problem) : @{ @"data": [output copy], @"status": @(task.terminationStatus) };
}
static NSString* Status(NSString* flags, BOOL unsaved)
{
	NSString* status = [flags containsString:@"U"] || [@[@"AA", @"DD"] containsObject:flags] ? @"Conflict" :
		[flags containsString:@"D"] ? @"Deleted" : [flags containsString:@"?"] || [flags containsString:@"A"] ? @"New file" : @"Modified";
	return unsaved ? [status stringByAppendingString:@" · Unsaved"] : status;
}
static NSString* PathRelativeTo(NSString* path, NSString* root)
{
	if([root isEqual:@"/"])
		return path.length > 1 && [path hasPrefix:@"/"] ? [path substringFromIndex:1] : nil;
	if(![path hasPrefix:root])
		return nil;
	NSString* remainder = [path substringFromIndex:root.length];
	return remainder.length > 1 && [remainder hasPrefix:@"/"] ? [remainder substringFromIndex:1] : nil;
}
static NSString* BranchName(NSString* header)
{
	if(!header.length) return nil;
	// Both states used to come from a `symbolic-ref`/`rev-parse` that failed:
	// a detached HEAD showed as "Detached HEAD" and an unborn branch as its name.
	if([header isEqual:@"HEAD (no branch)"]) return @"Detached HEAD";
	for(NSString* prefix in @[@"No commits yet on ", @"Initial commit on "])
		if([header hasPrefix:prefix]) return [header substringFromIndex:prefix.length];
	NSRange upstream = [header rangeOfString:@"..."];
	return upstream.location == NSNotFound ? header : [header substringToIndex:upstream.location];
}
static NSData* ReadFile(NSString* path, NSString** error)
{
	struct stat info;
	if(lstat(path.fileSystemRepresentation, &info)) { *error = @"The file is no longer available. Refresh to update the list."; return nil; }
	if(S_ISLNK(info.st_mode)) {
		char target[PATH_MAX];
		ssize_t size = readlink(path.fileSystemRepresentation, target, sizeof(target));
		if(size >= 0) return [NSData dataWithBytes:target length:size];
		*error = @"The symbolic link could not be read."; return nil;
	}
	int fd = open(path.fileSystemRepresentation, O_RDONLY | O_NONBLOCK | O_NOFOLLOW);
	if(fd < 0) { *error = @"The file could not be read."; return nil; }
	if(fstat(fd, &info) || !S_ISREG(info.st_mode)) { close(fd); *error = @"This entry is a directory or submodule. Review its changes in that repository."; return nil; }
	if(info.st_size > fileLimit) { close(fd); *error = @"This file is too large to preview (limit 4 MB)."; return nil; }
	NSMutableData* data = [NSMutableData data];
	char bytes[16384];
	ssize_t size;
	while((size = read(fd, bytes, sizeof(bytes))) > 0) {
		[data appendBytes:bytes length:size];
		if(data.length > fileLimit) { *error = @"This file is too large to preview (limit 4 MB)."; break; }
	}
	if(size < 0) *error = @"The file could not be read.";
	close(fd);
	return *error ? nil : data;
}
}

@implementation ProjectChangesBuffer
{
	ng::detail::storage_t _storage;
	BOOL _tooLarge;
}
+ (instancetype)captureDocument:(OakDocument*)document
{
	ProjectChangesBuffer* result = [[self alloc] init];
	result->_tooLarge = [document buffer].size() > fileLimit;
	if(!result->_tooLarge) result->_storage = [document buffer].storage();
	return result;
}
- (NSData*)data
{
	if(_tooLarge) return nil;
	NSMutableData* data = [NSMutableData data];
	for(auto const& piece : _storage) [data appendBytes:piece.data() length:piece.size()];
	return data;
}
@end

NSDictionary* ProjectChangesSnapshot(NSString* directory, NSDictionary<NSString*, ProjectChangesBuffer*>* buffers)
{
	// One `rev-parse` yields the work tree root and HEAD together. The root is
	// still printed when HEAD is unborn, which is how a repository without
	// commits is recognised without a second call.
	NSDictionary* revision = Git(directory, @[@"rev-parse", @"--show-toplevel", @"--verify", @"HEAD"]);
	NSString* root = nil;
	NSString* head = nil;
	if(!revision[@"error"]) {
		NSArray<NSString*>* lines = [[[NSString alloc] initWithData:revision[@"data"] encoding:NSUTF8StringEncoding] componentsSeparatedByString:@"\n"];
		if(lines.count > 0 && lines[0].length) root = lines[0];
		if(lines.count > 1 && lines[1].length) head = lines[1];
	}
	if(!root.length) return Error(revision[@"error"] ?: @"Open a folder in a Git repository to review its changes.");
	// `--branch` puts the branch name in the same call as the status entries,
	// so the two can no longer disagree and cannot cost two deadlines.
	NSDictionary* status = Git(root, @[@"status", @"--porcelain=v1", @"-z", @"--branch", @"--untracked-files=all", @"--no-renames"]);
	if(status[@"error"] || [status[@"status"] intValue]) return Error(status[@"error"] ?: @"The repository's changed files could not be read.");
	NSData* data = status[@"data"];
	NSMutableDictionary<NSString*, NSString*>* paths = [NSMutableDictionary dictionary];
	NSString* branch = nil;
	char const* bytes = static_cast<char const*>(data.bytes);
	NSUInteger start = 0;
	for(NSUInteger i = 0; i < data.length; ++i) if(bytes[i] == '\0') {
		if(i-start > 3 && bytes[start] == '#' && bytes[start+1] == '#') {
			branch = BranchName([[NSString alloc] initWithBytes:bytes+start+3 length:i-start-3 encoding:NSUTF8StringEncoding]);
		} else if(i-start >= 4) {
			NSString* flags = [[NSString alloc] initWithBytes:bytes+start length:2 encoding:NSUTF8StringEncoding];
			NSString* path = [[NSString alloc] initWithBytes:bytes+start+3 length:i-start-3 encoding:NSUTF8StringEncoding];
			if(path && flags) paths[path] = flags;
		}
		start = i+1;
	}
	NSMutableDictionary* bufferPaths = [NSMutableDictionary dictionary];
	for(NSString* original in buffers) {
		char resolved[PATH_MAX];
		NSString* parent = [original stringByDeletingLastPathComponent];
		NSString* absolute = [(realpath(parent.fileSystemRepresentation, resolved) ? @(resolved) : parent) stringByAppendingPathComponent:original.lastPathComponent];
		// Comparing component-wise, rather than with a "root/" prefix, keeps a
		// repository checked out at "/" able to match its unsaved buffers.
		if(NSString* relative = PathRelativeTo(absolute, root)) {
			if(!paths[relative]) paths[relative] = @" M";
			bufferPaths[relative] = original;
		}
	}
	NSMutableArray* files = [NSMutableArray array];
	for(NSString* path in [paths.allKeys sortedArrayUsingSelector:@selector(localizedStandardCompare:)]) {
		NSString* absolute = [root stringByAppendingPathComponent:path];
		BOOL unsaved = bufferPaths[path] != nil;
		[files addObject:@{ @"path": path, @"absolutePath": absolute, @"flags": paths[path],
			@"status": Status(paths[path], unsaved), @"unsaved": @(unsaved), @"bufferPath": bufferPaths[path] ?: absolute }];
	}
	return @{ @"root": root, @"files": files, @"head": head ?: @"", @"branch": branch ?: @"Detached HEAD" };
}

NSDictionary* ProjectChangesDiff(NSDictionary* snapshot, NSDictionary* file, NSDictionary<NSString*, ProjectChangesBuffer*>* buffers)
{
	NSData* before = [NSData data];
	NSString* flags = file[@"flags"];
	if([snapshot[@"head"] length]) {
		NSDictionary* blob = Git(snapshot[@"root"], @[@"cat-file", @"blob", [NSString stringWithFormat:@"%@:%@", snapshot[@"head"], file[@"path"]]]);
		if(blob[@"error"]) return Error(blob[@"error"]);
		if([blob[@"status"] intValue]) {
			if(![flags containsString:@"A"] && ![flags containsString:@"?"])
				return Error(@"The committed version is not a text file or could not be read. Refresh and try again.");
		} else before = blob[@"data"];
	}
	NSString* problem = nil;
	NSData* after;
	if(ProjectChangesBuffer* buffer = buffers[file[@"bufferPath"]]) {
		after = buffer.data;
		if(!after) return Error(@"This unsaved buffer is too large to preview (limit 4 MB).");
	} else if([flags containsString:@"D"]) after = [NSData data];
	else after = ReadFile(file[@"absolutePath"], &problem);
	if(!after) return Error(problem ?: @"The current file could not be read.");
	if(memchr(before.bytes, 0, before.length) || memchr(after.bytes, 0, after.length))
		return Error(@"Binary file changed. A text diff is not available.");
	NSString* oldText = [[NSString alloc] initWithData:before encoding:NSUTF8StringEncoding];
	NSString* newText = [[NSString alloc] initWithData:after encoding:NSUTF8StringEncoding];
	if(!oldText || !newText) return Error(@"This file uses an encoding other than UTF-8. A text diff is not available.");
	// Editor buffers normalize newlines. Compare like-for-like for an unsaved
	// buffer so a CRLF file does not appear changed on every line.
	if([file[@"unsaved"] boolValue]) {
		before = [[oldText stringByReplacingOccurrencesOfString:@"\r\n" withString:@"\n"] dataUsingEncoding:NSUTF8StringEncoding];
		after = [[newText stringByReplacingOccurrencesOfString:@"\r\n" withString:@"\n"] dataUsingEncoding:NSUTF8StringEncoding];
	}
	mmfile_t a = { (char*)before.bytes, (long)before.length }, b = { (char*)after.bytes, (long)after.length };
	xpparam_t parameters = {};
	parameters.flags = XDF_HISTOGRAM_DIFF;
	xdemitconf_t config = {};
	config.ctxlen = 3;
	NSMutableData* output = [NSMutableData data];
	xdemitcb_t callback = {};
	callback.priv = (__bridge void*)output;
	callback.out_line = [](void* context, mmbuffer_t* chunks, int count) -> int {
		NSMutableData* data = (__bridge NSMutableData*)context;
		for(int i = 0; i < count; ++i) {
			if(data.length + chunks[i].size > 3*fileLimit) return -1;
			[data appendBytes:chunks[i].ptr length:chunks[i].size];
		}
		return 0;
	};
	if(xdl_diff(&a, &b, &parameters, &config, &callback) < 0) return Error(@"This diff is too large to display.");
	NSString* diff = [[NSString alloc] initWithData:output encoding:NSUTF8StringEncoding];
	NSMutableArray* hunks = [NSMutableArray array];
	NSRegularExpression* pattern = [NSRegularExpression regularExpressionWithPattern:@"^@@ -[0-9]+(?:,[0-9]+)? \\+([0-9]+)(?:,[0-9]+)? @@" options:NSRegularExpressionAnchorsMatchLines error:nil];
	NSArray<NSTextCheckingResult*>* matches = [pattern matchesInString:diff options:0 range:NSMakeRange(0, diff.length)];
	for(NSUInteger i = 0; i < matches.count; ++i) {
		NSRange range = matches[i].range;
		NSUInteger end = i+1 < matches.count ? matches[i+1].range.location : diff.length;
		[hunks addObject:@{ @"offset": @(range.location), @"length": @(end-range.location),
			@"line": @(MAX(1, [[diff substringWithRange:[matches[i] rangeAtIndex:1]] integerValue])) }];
	}
	NSUInteger added = 0, removed = 0;
	for(NSString* line in [diff componentsSeparatedByString:@"\n"]) {
		if([line hasPrefix:@"+"]) ++added;
		if([line hasPrefix:@"-"]) ++removed;
	}
	return @{ @"text": diff.length ? diff : @"No text differences against the last commit. File permissions or the staging area may differ.",
		@"hunks": hunks, @"added": @(added), @"removed": @(removed) };
}
