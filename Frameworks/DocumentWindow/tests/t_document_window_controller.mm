#import <Cocoa/Cocoa.h>
#import "../src/DocumentWindowController.h"
#import "../src/TerminalPanelViewController.h"

void test_window_without_documents_is_kept_for_file_browser_or_terminal ()
{
	dispatch_sync(dispatch_get_main_queue(), ^{
		@autoreleasepool
		{
			[NSApplication sharedApplication];
			[DocumentWindowController disableSessionSave];
			DocumentWindowController* controller = [[DocumentWindowController alloc] init];
			OAK_ASSERT(!controller.keepsWindowWithoutDocuments);

			controller.fileBrowserVisible = YES;
			OAK_ASSERT(controller.keepsWindowWithoutDocuments);
			// Enabling the file browser in a window that was never laid out aborted when the project layout was pinned to a frame.
			OAK_ASSERT(![[controller valueForKey:@"layoutView"] translatesAutoresizingMaskIntoConstraints]);
			controller.fileBrowserVisible = NO;
			OAK_ASSERT(!controller.keepsWindowWithoutDocuments);

			TerminalPanelViewController* panel = [controller valueForKey:@"terminalPanelController"];
			panel.terminalShellPath      = @"/bin/sh";
			panel.terminalShellArguments = @[ ];
			[panel startTerminal];
			if(panel.hasTerminal) // built without the Swift terminal component
			{
				OAK_ASSERT(controller.keepsWindowWithoutDocuments);
				[panel stopTerminal];
				OAK_ASSERT(!controller.keepsWindowWithoutDocuments);
			}
			[DocumentWindowController enableSessionSave];
		}
	});
}
