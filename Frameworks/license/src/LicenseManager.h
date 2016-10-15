#import <oak/misc.h>

PUBLIC @interface LicenseManager : NSObject
+ (instancetype)sharedInstance;
- (void)showAddLicenseWindow:(id)sender;
- (void)decorateWindow:(NSWindow*)window;
@property (nonatomic, readonly) NSString* owner;
@end
