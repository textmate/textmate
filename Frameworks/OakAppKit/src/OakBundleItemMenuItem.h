#import <bundles/bundles.h>

struct PUBLIC OakBundleItemMenuItemAlignment
{
	OakBundleItemMenuItemAlignment () : maxAlignmentWidth(0), maxRightWidth(0) {}
	CGFloat maxAlignmentWidth;
	CGFloat maxRightWidth;
};

@interface OakBundleItemMenuItem : NSMenuItem
{
	BOOL hasRightPart;
}
+ (OakBundleItemMenuItem*)menuItemWithBundleItem:(bundles::item_ptr const&)bundleItem alignmentData:(OakBundleItemMenuItemAlignment&)alignment;
- (void)updateAlignment:(OakBundleItemMenuItemAlignment&)alignment;
@end
