#include <bundles/bundles.h>

PUBLIC @interface BundleEditor : NSWindowController <NSBrowserDelegate>
+ (instancetype)sharedInstance;
- (void)revealBundleItem:(bundles::item_ptr const&)anItem;
- (IBAction)browserSelectionDidChange:(id)sender;
@end
