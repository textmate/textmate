#import <bundles/bundles.h>
#import <oak/debug.h>

@interface BundleMenuDelegate : NSObject <NSMenuDelegate>
{
	OBJC_WATCH_LEAKS(BundleMenuDelegate);
	bundles::item_ptr umbrellaItem;
}
- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem;
@end
