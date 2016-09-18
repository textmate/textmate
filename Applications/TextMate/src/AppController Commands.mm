#import "AppController.h"
#import <DocumentWindow/DocumentWindowController.h>
#import <bundles/bundles.h>
#import <command/parser.h>
#import <command/runner.h>
#import <document/collection.h>
#import <ns/ns.h>
#import <settings/settings.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakCommand/OakCommand.h>
#import <plist/uuid.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>

OAK_DEBUG_VAR(AppController_Commands);

@implementation AppController (Commands)
- (void)performBundleItemWithUUIDString:(NSString*)uuidString
{
	if(bundles::item_ptr item = bundles::lookup(to_s(uuidString)))
	{
		NSWindow* mainWindow = [NSApp mainWindow];

		id delegate = [([mainWindow attachedSheet] ?: mainWindow) delegate];

		if(![delegate respondsToSelector:@selector(performBundleItem:)])
			delegate = [NSApp targetForAction:@selector(performBundleItem:)];
		if(delegate)
			return [delegate performBundleItem:item];

		switch(item->kind())
		{
			case bundles::kItemTypeSnippet:
			{
				document::document_ptr doc = document::create();
				// TODO set language according to snippet’s scope selector
				doc->sync_load();
				document::show(doc); // If we call show() with a document that isn’t open then it will be loaded in the background, and show() will return before this has completed, meaning the next line may not target our new document.
				[[DocumentWindowController controllerForDocument:doc] performBundleItem:item];
				doc->close();
				// TODO mark document as “not modified”
			}
			break;

			case bundles::kItemTypeCommand:
			{
				OakCommand* command = [[OakCommand alloc] initWithBundleCommand:parse_command(item)];
				command.firstResponder = NSApp;
				[command executeWithInput:nil variables:item->bundle_variables() completionHandler:nil];
			}
			break;

			case bundles::kItemTypeGrammar:
			{
				document::show(document::from_content("", item->value_for_field(bundles::kFieldGrammarScope)));
			}
			break;
		}
	}
}
@end
