#ifndef BUNDLEMENU_H_BI4UDOAR
#define BUNDLEMENU_H_BI4UDOAR

#import <bundles/bundles.h>
#import <oak/misc.h>

PUBLIC @interface BundleMenuDelegate : NSObject <NSMenuDelegate>
{
	OBJC_WATCH_LEAKS(BundleMenuDelegate);
	bundles::item_ptr umbrellaItem;
	NSMutableArray* subdelegates;
}
- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem;
@end

PUBLIC bundles::item_ptr OakShowMenuForBundleItems (std::vector<bundles::item_ptr> const& items, CGPoint const& pos, bool hasSelection = false);

#endif /* end of include guard: BUNDLEMENU_H_BI4UDOAR */
