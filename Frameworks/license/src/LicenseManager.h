#import <oak/misc.h>

PUBLIC @interface LicenseManager : NSObject
+ (instancetype)sharedInstance;
- (void)showAddLicenseWindow:(id)sender;
@property (nonatomic, readonly) NSString* owner;
@end
