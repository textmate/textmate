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
#import <oak/CocoaSTL.h>

OAK_DEBUG_VAR(AppController_Commands);

static CGPoint MenuPosition ()
{
	NSPoint pos = [NSEvent mouseLocation];
	pos.y -= 16;

	return NSPointToCGPoint(pos);
}

@implementation AppController (Commands)
- (void)performBundleItemWithUUIDString:(NSString*)uuidString
{
	if(bundles::item_ptr item = bundles::lookup(to_s(uuidString)))
	{
		DocumentController* delegate = (DocumentController*)[[NSApp mainWindow] delegate];
		if([delegate respondsToSelector:@selector(performBundleItem:)])
			return [delegate performBundleItem:item];

		switch(item->kind())
		{
			case bundles::kItemTypeSnippet:
			{
				// TODO set language according to snippet’s scope selector
				// TODO mark document as “not modified”
				document::document_ptr doc = document::create();
				doc->open();
				ng::editor_ptr editor = ng::editor_for_document(doc);
				editor->snippet_dispatch(item->plist(), editor->variables(item->environment()));
				document::show(doc);
				doc->close();
			}
			break;

			case bundles::kItemTypeCommand:
			{
				document::run(parse_command(item), ng::buffer_t(), ng::ranges_t(), document::document_ptr());
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

- (BOOL)canHandleMenuKeyEquivalent:(NSEvent*)anEvent
{
	if([[[NSApp keyWindow] delegate] isKindOfClass:[DocumentController class]])
		return NO;
	return !bundles::query(bundles::kFieldKeyEquivalent, to_s(anEvent), "").empty();
}

- (void)handleMenuKeyEquivalent:(id)sender
{
	if(bundles::item_ptr item = bundles::show_menu_for_items(bundles::query(bundles::kFieldKeyEquivalent, to_s([NSApp currentEvent]), ""), MenuPosition()))
		[self performBundleItemWithUUIDString:[NSString stringWithCxxString:item->uuid()]];
}
@end
