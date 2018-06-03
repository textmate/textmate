#include <os/log.h>

int main (int argc, char const* argv[])
{
	for(NSUInteger i = 0; i < argc; ++i)
		os_log_info(OS_LOG_DEFAULT, "Genie Launcher: argv[%lu] = %{public}s", i, argv[i]);

	NSError* error;
	NSString* geniePath = [[NSBundle.mainBundle.bundlePath stringByAppendingPathComponent:@"../../../../../Genie.app"] stringByStandardizingPath];
	if(![NSWorkspace.sharedWorkspace launchApplicationAtURL:[NSURL fileURLWithPath:geniePath] options:NSWorkspaceLaunchWithoutActivation|NSWorkspaceLaunchWithoutAddingToRecents configuration:@{ } error:&error])
		os_log_error(OS_LOG_DEFAULT, "Failed to launch Genie.app: %{public}@", error.localizedDescription);

	return EX_OK;
}
