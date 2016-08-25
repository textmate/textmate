#import "HTMLOutput.h"
#import <OakSystem/process.h>
#import <command/runner.h>
#import <oak/debug.h>

NSString* const kCommandRunnerURLScheme = @"x-txmt-command";

namespace
{
	struct runners_t
	{
		NSInteger add_process (pid_t processId, NSFileHandle* fileHandle)
		{
			std::lock_guard<std::mutex> lock(_lock);
			return _records.emplace(++_next_key, (record_t){ processId, fileHandle }).first->first;
		}

		std::pair<pid_t, NSFileHandle*> remove_process (NSInteger key)
		{
			std::lock_guard<std::mutex> lock(_lock);
			auto iter = _records.find(key);
			if(iter == _records.end())
				return { };

			auto res = std::make_pair(iter->second.process_id, iter->second.file_handle);
			_records.erase(iter);
			return res;
		}

	private:
		struct record_t
		{
			pid_t process_id;
			NSFileHandle* file_handle;
		};

		NSInteger _next_key = 0;
		std::map<NSInteger, record_t> _records;
		std::mutex _lock;
	};

	runners_t& runners ()
	{
		static runners_t runners;
		return runners;
	}
}

@interface CommandRunnerURLProtocol : NSURLProtocol
@property (nonatomic) pid_t processId;
@property (nonatomic) NSFileHandle* fileHandle;
@end

@implementation CommandRunnerURLProtocol
+ (void)load
{
	[self registerClass:self];
	[WebView registerURLSchemeAsLocal:kCommandRunnerURLScheme];
}

+ (BOOL)canInitWithRequest:(NSURLRequest*)request                            { return [[[request URL] scheme] isEqualToString:kCommandRunnerURLScheme]; }
+ (NSURLRequest*)canonicalRequestForRequest:(NSURLRequest*)request           { return request; }
+ (BOOL)requestIsCacheEquivalent:(NSURLRequest*)a toRequest:(NSURLRequest*)b { return NO; }

- (id)initWithRequest:(NSURLRequest*)anURLRequest cachedResponse:(NSCachedURLResponse*)aCachedURLResponse client:(id <NSURLProtocolClient>)anId
{
	if(self = [super initWithRequest:anURLRequest cachedResponse:aCachedURLResponse client:anId])
	{
		NSInteger key;
		NSString* jobString = [[[anURLRequest URL] path] lastPathComponent];
		NSScanner* scanner = [NSScanner scannerWithString:jobString];
		if([scanner scanInteger:&key] && scanner.scanLocation == [jobString length])
			std::tie(_processId, _fileHandle) = runners().remove_process(key);
	}
	return self;
}

// =============================================
// = These methods might be called in a thread =
// =============================================

- (void)startLoading
{
	if(!_fileHandle)
	{
		NSURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:[[self request] URL] statusCode:404 HTTPVersion:@"HTTP/1.1" headerFields:nil];
		[[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];
		[[self client] URLProtocolDidFinishLoading:self];
		NSLog(@"No command output for ‘%@’", [[self request] URL]);
		return;
	}

	NSURLResponse* response = [[NSURLResponse alloc] initWithURL:[[self request] URL] MIMEType:@"text/html" expectedContentLength:-1 textEncodingName:@"utf-8"];
	[[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];

	// WebView seems to stall until it has received at least 1024 bytes
	static std::string const dummy("<!--" + std::string(1017, ' ') + "-->");
	[[self client] URLProtocol:self didLoadData:[NSData dataWithBytes:dummy.data() length:dummy.size()]];

	NSFileHandle* fh = _fileHandle;
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		char buf[8192];
		int len;
		while((len = read(fh.fileDescriptor, buf, sizeof(buf))) > 0)
		{
			NSData* data = [NSData dataWithBytesNoCopy:buf length:len freeWhenDone:NO];
			[[self client] URLProtocol:self didLoadData:data];
		}

		if(len == -1)
			perror("HTMLOutput: read");

		[fh closeFile];
		[[self client] URLProtocolDidFinishLoading:self];
	});
}

- (void)stopLoading
{
	[_fileHandle closeFile];
	oak::kill_process_group_in_background(_processId);
}
@end

// ==============
// = Public API =
// ==============

NSURLRequest* URLRequestForCommandRunner (command::runner_ptr aRunner)
{
	struct filehandle_callback_t : command::callback_t
	{
		filehandle_callback_t (command::runner_ptr runner, NSFileHandle* fileHandle) : _runner(runner), _fileHandle(fileHandle) { _runner->add_callback(this); }
		~filehandle_callback_t ()                                                                                               { _runner->remove_callback(this); }

		void output (command::runner_ptr runner, char const* data, size_t len)
		{
			NSFileHandle* fh = _fileHandle;
			__block std::string const buf(data, len);
			dispatch_async(queue(), ^{
				ssize_t bytesWritten = write(fh.fileDescriptor, buf.data(), buf.size());
				if(bytesWritten == -1)
					perror("HTMLOutput: write");
			});
		}

		void done (command::runner_ptr runner)
		{
			NSFileHandle* fh = _fileHandle;
			dispatch_async(queue(), ^{
				[fh closeFile];
			});
			delete this;
		}

	private:
		dispatch_queue_t queue ()
		{
			if(!_queue)
				_queue = dispatch_queue_create("org.textmate.command", DISPATCH_QUEUE_SERIAL);
			return _queue;
		}

		command::runner_ptr _runner;
		NSFileHandle* _fileHandle;
		dispatch_queue_t _queue = nullptr;
	};

	NSPipe* pipe = [NSPipe pipe];
	new filehandle_callback_t(aRunner, pipe.fileHandleForWriting);

	NSInteger key = runners().add_process(aRunner->process_id(), pipe.fileHandleForReading);
	return [NSURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@://job/%ld", kCommandRunnerURLScheme, key]] cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:6000]; // TODO add a description parameter to the URL (based on bundle item name)
}
