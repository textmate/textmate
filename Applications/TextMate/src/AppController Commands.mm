#import "AppController.h"
#import <DocumentWindow/src/DocumentWindowController.h>
#import <bundles/src/bundles.h>
#import <command/src/parser.h>
#import <command/src/runner.h>
#import <document/src/OakDocument.h>
#import <document/src/OakDocumentController.h>
#import <ns/src/ns.h>
#import <settings/src/settings.h>
#import <OakAppKit/src/NSAlert Additions.h>
#import <OakAppKit/src/OakToolTip.h>
#import <OakFoundation/src/NSString Additions.h>
#import <OakCommand/src/OakCommand.h>
#import <plist/src/uuid.h>
#import <HTMLOutputWindow/src/HTMLOutputWindow.h>

@implementation AppController (Commands)
- (void)performBundleItemWithUUIDStringFrom:(id)anArgument
{
	NSString* uuidString = [anArgument valueForKey:@"representedObject"];
	if(bundles::item_ptr item = bundles::lookup(to_s(uuidString)))
	{
		if(id delegate = [NSApp.keyWindow.delegate respondsToSelector:@selector(performBundleItem:)] ? NSApp.keyWindow.delegate : [NSApp targetForAction:@selector(performBundleItem:)])
			[delegate performBundleItem:item];
	}
}

- (void)performBundleItem:(bundles::item_ptr)item
{
	switch(item->kind())
	{
		case bundles::kItemTypeSnippet:
		{
			// TODO set language according to snippet’s scope selector

			OakDocument* doc = [OakDocumentController.sharedInstance untitledDocument];
			[doc loadModalForWindow:nil completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
				[OakDocumentController.sharedInstance showDocument:doc];
				if(DocumentWindowController* controller = [DocumentWindowController controllerForDocument:doc])
					[controller performBundleItem:item];
				[doc markDocumentSaved];
				[doc close];
			}];
		}
		break;

		case bundles::kItemTypeCommand:
		{
			OakCommand* command = [[OakCommand alloc] initWithBundleCommand:parse_command(item)];
			command.firstResponder = NSApp;
			[command executeWithInput:nil variables:item->bundle_variables() outputHandler:nil];
		}
		break;

		case bundles::kItemTypeGrammar:
		{
			OakDocument* doc = [OakDocumentController.sharedInstance untitledDocument];
			doc.fileType = to_ns(item->value_for_field(bundles::kFieldGrammarScope));
			[OakDocumentController.sharedInstance showDocument:doc];
		}
		break;
	}
}
@end
