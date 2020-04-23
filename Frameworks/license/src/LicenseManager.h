#import <oak/misc.h>

PUBLIC @interface LicenseManager : NSObject
@property (class, readonly) LicenseManager* sharedInstance;
- (void)showAddLicenseWindow:(id)sender;
- (void)decorateWindow:(NSWindow*)window;
@property (nonatomic, readonly) NSString* owner;
@end
