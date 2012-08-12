#import <cf/cf.h>
#import <oak/debug.h>
#import <bundles/bundles.h>

extern PUBLIC NSString* const BundlesManagerBundlesDidChangeNotification;

@interface BundleMenuDelegate : NSObject <NSMenuDelegate> {
	OBJC_WATCH_LEAKS(BundleMenuDelegate);
	bundles::item_ptr umbrellaItem;
}

- (id)initWithBundleItem:(bundles::item_ptr const&)aBundleItem;
- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction;
- (void)menuNeedsUpdate:(NSMenu*)aMenu;
- (void)menuWillOpen:(NSMenu*)aMenu;
- (void)menuDidClose:(NSMenu*)aMenu;
- (void)zapMenu:(NSMenu*)aMenu;

@end
