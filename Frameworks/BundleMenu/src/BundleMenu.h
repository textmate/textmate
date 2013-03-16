#ifndef BUNDLEMENU_H_BI4UDOAR
#define BUNDLEMENU_H_BI4UDOAR

#import <bundles/bundles.h>
#import <oak/misc.h>

PUBLIC @interface BundleMenuDelegate : NSObject <NSMenuDelegate>
+ (BundleMenuDelegate*)sharedInstance;
@end

PUBLIC bundles::item_ptr OakShowMenuForBundleItems (std::vector<bundles::item_ptr> const& items, CGPoint const& pos);

#endif /* end of include guard: BUNDLEMENU_H_BI4UDOAR */
