#ifndef BUNDLEMENU_H_BI4UDOAR
#define BUNDLEMENU_H_BI4UDOAR

#import <bundles/src/bundles.h>

@interface BundleMenuDelegate : NSObject <NSMenuDelegate>
@property (class, readonly) BundleMenuDelegate* sharedInstance;
@end

bundles::item_ptr OakShowMenuForBundleItems (std::vector<bundles::item_ptr> const& items, NSView* view, NSPoint pos);

#endif /* end of include guard: BUNDLEMENU_H_BI4UDOAR */
