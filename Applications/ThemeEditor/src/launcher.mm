#import <AppKit/AppKit.h>
#import <mach-o/dyld.h>

int main (int argc, char const* argv[])
{
	@autoreleasepool
	{
		if(argc > 1)
		{
			fprintf(stdout, "Usage: tm-theme-editor\nOpens the native TextMate theme editor.\n");
			return strcmp(argv[1], "--help") == 0 ? 0 : 1;
		}
		uint32_t length = 0;
		_NSGetExecutablePath(nullptr, &length);
		std::vector<char> buffer(length);
		_NSGetExecutablePath(buffer.data(), &length);
		NSString* executable = [[NSString stringWithUTF8String:buffer.data()] stringByResolvingSymlinksInPath];
		NSString* contents = executable.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent;
		NSURL* editor = [NSURL fileURLWithPath:[contents stringByAppendingPathComponent:@"Helpers/ThemeEditor.app"] isDirectory:YES];
		NSWorkspaceOpenConfiguration* configuration = [NSWorkspaceOpenConfiguration configuration];
		configuration.activates = YES;
		configuration.createsNewApplicationInstance = NO;
		dispatch_semaphore_t finished = dispatch_semaphore_create(0);
		__block NSError* launchError = nil;
		[NSWorkspace.sharedWorkspace openApplicationAtURL:editor configuration:configuration completionHandler:^(NSRunningApplication* app, NSError* error) {
			launchError = error;
			dispatch_semaphore_signal(finished);
		}];
		if(dispatch_semaphore_wait(finished, dispatch_time(DISPATCH_TIME_NOW, 30 * NSEC_PER_SEC)) != 0)
		{
			fprintf(stderr, "tm-theme-editor: timed out launching %s\n", editor.fileSystemRepresentation);
			return 1;
		}
		if(launchError)
		{
			fprintf(stderr, "tm-theme-editor: could not launch %s: %s\n", editor.fileSystemRepresentation, launchError.localizedDescription.UTF8String);
			return 1;
		}
		return 0;
	}
}
