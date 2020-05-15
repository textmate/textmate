@interface FFFolderMenu : NSObject <NSMenuDelegate>
@property (class, readonly) FFFolderMenu* sharedInstance;
+ (void)addSubmenuForDirectoryAtPath:(NSString*)path toMenuItem:(NSMenuItem*)aMenuItem;
@end
