@interface LicenseManager : NSObject
@property (class, readonly) LicenseManager* sharedInstance;
- (void)showAddLicenseWindow:(id)sender;
@property (nonatomic, readonly) NSString* owner;
@end
