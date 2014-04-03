#import "AppController.h"
#import <DocumentWindow/DocumentController.h>
#import <bundles/bundles.h>
#import <command/parser.h>
#import <command/runner.h>
#import <document/collection.h>
#import <editor/editor.h>
#import <ns/ns.h>
#import <settings/settings.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakToolTip.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <plist/uuid.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>

OAK_DEBUG_VAR(AppController_Commands);

@implementation AppController (Commands)
- (void)performBundleItemWithUUIDString:(NSString*)uuidString
{
	if(bundles::item_ptr item = bundles::lookup(to_s(uuidString)))
	{
		id delegate = [[NSApp mainWindow] delegate];
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
				doc->open();
				document::show(doc); // If we call show() with a document that isn’t open then it will be loaded in the background, and show() will return before this has completed, meaning the next line may not target our new document.
				[[DocumentController controllerForDocument:doc] performBundleItem:item];
				doc->close();
				// TODO mark document as “not modified”
			}
			break;

			case bundles::kItemTypeCommand:
			{
				std::map<std::string, std::string> map = oak::basic_environment();
				map << item->bundle_variables();
				map = bundles::scope_variables(map);
				map = variables_for_path(map);
				document::run(parse_command(item), ng::buffer_t(), ng::ranges_t(), document::document_ptr(), map);
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
