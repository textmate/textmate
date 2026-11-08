#import "../src/ProjectChanges.h"
#include <unistd.h>

static void changes_test_git(NSString* directory, NSArray<NSString*>* arguments)
{
	NSTask* task = [[NSTask alloc] init];
	task.executableURL = [NSURL fileURLWithPath:@"/usr/bin/git"];
	task.arguments = [@[@"-C", directory, @"-c", @"user.name=TextMate Tests", @"-c", @"user.email=tests@example.invalid", @"-c", @"commit.gpgsign=false", @"-c", @"core.hooksPath=/dev/null"] arrayByAddingObjectsFromArray:arguments];
	task.standardInput = [NSFileHandle fileHandleWithNullDevice];
	task.standardOutput = [NSFileHandle fileHandleWithNullDevice];
	task.standardError = [NSFileHandle fileHandleWithNullDevice];
	OAK_ASSERT([task launchAndReturnError:nil]);
	[task waitUntilExit];
	OAK_ASSERT(task.terminationStatus == 0);
}
static void changes_test_write(NSString* directory, NSString* path, NSString* text)
{
	OAK_ASSERT([text writeToFile:[directory stringByAppendingPathComponent:path] atomically:YES encoding:NSUTF8StringEncoding error:nil]);
}
static NSDictionary* changes_test_file(NSDictionary* snapshot, NSString* path)
{
	for(NSDictionary* file in snapshot[@"files"]) if([file[@"path"] isEqual:path]) return file;
	return nil;
}

void test_project_changes_git_baseline_and_special_files ()
{
	NSString* directory = [NSTemporaryDirectory() stringByAppendingPathComponent:[@"TextMate Changes " stringByAppendingString:NSUUID.UUID.UUIDString]];
	OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:directory withIntermediateDirectories:YES attributes:nil error:nil]);
	changes_test_git(directory, @[@"init", @"--quiet"]);
	NSMutableArray* lines = [NSMutableArray array];
	for(int i = 1; i <= 35; ++i) [lines addObject:[NSString stringWithFormat:@"line %d", i]];
	NSString* original = [[lines componentsJoinedByString:@"\n"] stringByAppendingString:@"\n"];
	NSString* special = @"--file with spaces 😀\nand newline.txt";
	changes_test_write(directory, @"modified.txt", original);
	changes_test_write(directory, @"deleted.txt", @"removed\n");
	changes_test_write(directory, @"binary.dat", @"original\n");
	changes_test_write(directory, special, @"original 😀\n");
	OAK_ASSERT(symlink("old-target", [directory stringByAppendingPathComponent:@"link"].fileSystemRepresentation) == 0);
	changes_test_git(directory, @[@"add", @"--all"]);
	changes_test_git(directory, @[@"commit", @"--quiet", @"-m", @"Fixture"]);
	lines[1] = @"staged edit";
	changes_test_write(directory, @"modified.txt", [[lines componentsJoinedByString:@"\n"] stringByAppendingString:@"\n"]);
	changes_test_git(directory, @[@"add", @"--", @"modified.txt"]);
	lines[30] = @"unstaged edit";
	changes_test_write(directory, @"modified.txt", [[lines componentsJoinedByString:@"\n"] stringByAppendingString:@"\n"]);
	OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:[directory stringByAppendingPathComponent:@"deleted.txt"] error:nil]);
	changes_test_write(directory, @"new.txt", @"brand new\n");
	changes_test_write(directory, special, @"updated 😀\n");
	OAK_ASSERT(unlink([directory stringByAppendingPathComponent:@"link"].fileSystemRepresentation) == 0);
	OAK_ASSERT(symlink("new-target", [directory stringByAppendingPathComponent:@"link"].fileSystemRepresentation) == 0);
	char binary[] = {'a', 0, 'b'};
	OAK_ASSERT([[NSData dataWithBytes:binary length:sizeof(binary)] writeToFile:[directory stringByAppendingPathComponent:@"binary.dat"] atomically:YES]);
	NSDictionary* snapshot = ProjectChangesSnapshot(directory, @{});
	OAK_ASSERT(!snapshot[@"error"] && [snapshot[@"files"] count] == 6);
	OAK_ASSERT([snapshot[@"head"] length] == 40);
	NSDictionary* diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"modified.txt"), @{});
	OAK_ASSERT(!diff[@"error"] && [diff[@"hunks"] count] == 2);
	OAK_ASSERT([diff[@"text"] containsString:@"+staged edit"] && [diff[@"text"] containsString:@"+unstaged edit"]);
	OAK_ASSERT([diff[@"added"] integerValue] == 2 && [diff[@"removed"] integerValue] == 2);
	for(NSDictionary* hunk in diff[@"hunks"]) {
		NSUInteger offset = [hunk[@"offset"] unsignedIntegerValue];
		OAK_ASSERT([[diff[@"text"] substringFromIndex:offset] hasPrefix:@"@@"]);
	}
	diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"new.txt"), @{});
	OAK_ASSERT([diff[@"text"] containsString:@"+brand new"]);
	diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"deleted.txt"), @{});
	OAK_ASSERT([diff[@"text"] containsString:@"-removed"] && [diff[@"added"] integerValue] == 0);
	diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"binary.dat"), @{});
	OAK_ASSERT([diff[@"error"] containsString:@"Binary"]);
	diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, special), @{});
	OAK_ASSERT([diff[@"text"] containsString:@"+updated 😀"]);
	diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"link"), @{});
	OAK_ASSERT([diff[@"text"] containsString:@"+new-target"] && [diff[@"text"] containsString:@"-old-target"]);
	// Repository reads do not change staging or rewrite the working tree.
	NSDictionary* again = ProjectChangesSnapshot(directory, @{});
	OAK_ASSERT([changes_test_file(again, @"modified.txt")[@"flags"] isEqual:@"MM"]);
	OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:directory error:nil]);
}

void test_project_changes_without_commit_and_outside_repository ()
{
	NSString* directory = [NSTemporaryDirectory() stringByAppendingPathComponent:NSUUID.UUID.UUIDString];
	OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:directory withIntermediateDirectories:YES attributes:nil error:nil]);
	OAK_ASSERT((ProjectChangesSnapshot(directory, @{})[@"error"] != nil));
	changes_test_git(directory, @[@"init", @"--quiet"]);
	changes_test_write(directory, @"first.txt", @"first change\n");
	NSDictionary* snapshot = ProjectChangesSnapshot(directory, @{});
	OAK_ASSERT(!snapshot[@"error"] && [snapshot[@"head"] length] == 0);
	NSDictionary* diff = ProjectChangesDiff(snapshot, changes_test_file(snapshot, @"first.txt"), @{});
	OAK_ASSERT([diff[@"text"] containsString:@"+first change"]);
	OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:directory error:nil]);
}
