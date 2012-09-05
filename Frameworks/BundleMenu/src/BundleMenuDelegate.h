#import <bundles/bundles.h>
#import <oak/debug.h>

PUBLIC @interface BundleMenuDelegate : NSObject <NSMenuDelegate>
{
	OBJC_WATCH_LEAKS(BundleMenuDelegate);
	bundles::item_ptr umbrellaItem;
	NSMutableArray* subdelegates;
}
- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem;
@end
