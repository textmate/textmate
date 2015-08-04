#include <document/document.h>
#include <bundles/bundles.h>

@class OakDocumentView;
@class PropertiesViewController;

namespace be { struct entry_t; typedef std::shared_ptr<entry_t> entry_ptr; } /* be */

PUBLIC @interface BundleEditor : NSWindowController <NSBrowserDelegate>
{
	IBOutlet NSBrowser* browser;
	IBOutlet OakDocumentView* documentView;
	NSDrawer* drawer;

	be::entry_ptr bundles;
	std::map<bundles::item_ptr, plist::dictionary_t> changes;

	BOOL propertiesChanged;

	bundles::item_ptr bundleItem;
	document::document_ptr bundleItemContent;

	document::document_t::callback_t* documentCallback;
}
+ (BundleEditor*)sharedInstance;
- (void)revealBundleItem:(bundles::item_ptr const&)anItem;
- (IBAction)browserSelectionDidChange:(id)sender;
@end
