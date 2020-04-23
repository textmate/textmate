@interface FFFolderMenu : NSObject <NSMenuDelegate>
@property (class, readonly) FFFolderMenu* sharedInstance;
+ (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
- (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
@end
