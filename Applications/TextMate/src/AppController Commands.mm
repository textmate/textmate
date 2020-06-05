#import "AppController.h"
#import <DocumentWindow/DocumentWindowController.h>
#import <bundles/bundles.h>
#import <command/parser.h>
#import <command/runner.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <ns/ns.h>
#import <settings/settings.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakCommand/OakCommand.h>
#import <plist/uuid.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>

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
