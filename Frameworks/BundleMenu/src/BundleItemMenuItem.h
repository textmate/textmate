struct BundleItemMenuItemAlignment
{
	BundleItemMenuItemAlignment () : maxAlignmentWidth(0), maxRightWidth(0) {}
	CGFloat maxAlignmentWidth;
	CGFloat maxRightWidth;
};

@interface BundleItemMenuItem : NSMenuItem
{
	BOOL hasRightPart;
}
+ (BundleItemMenuItem*)menuItemWithName:(std::string const&)name
                          keyEquivalent:(std::string const&)keyEquiv
                             tabTrigger:(std::string const&)tabTrigger
                                 action:(SEL)action
                          alignmentData:(BundleItemMenuItemAlignment&)alignment;
- (void)updateAlignment:(BundleItemMenuItemAlignment&)alignment;
@end
