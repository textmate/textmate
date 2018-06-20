#include <os/log.h>

int main (int argc, char const* argv[])
{
	NSString* geniePath = [[NSBundle.mainBundle.bundlePath stringByAppendingPathComponent:@"../../../.."] stringByStandardizingPath];
	if(NSBundle* genieBundle = [NSBundle bundleWithPath:geniePath])
	{
		if(NSString* identifier = genieBundle.bundleIdentifier)
		{
			NSArray* runningApps = [NSRunningApplication runningApplicationsWithBundleIdentifier:identifier];
			if(!runningApps.count)
			{
				NSError* error;
				if(![NSWorkspace.sharedWorkspace launchApplicationAtURL:genieBundle.bundleURL options:NSWorkspaceLaunchWithoutActivation|NSWorkspaceLaunchWithoutAddingToRecents configuration:@{ } error:&error])
					os_log_error(OS_LOG_DEFAULT, "Failed to launch Genie.app: %{public}@", error.localizedDescription);
			}
			else
			{
				os_log_info(OS_LOG_DEFAULT, "Genie is already running");
			}
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "No bundle identifier in bundle: %{public}@", genieBundle);
		}
	}
	else
	{
		os_log_error(OS_LOG_DEFAULT, "Unable to load bundle at path: %{public}@", geniePath);
	}
	return EX_OK;
}
