@interface FFFolderMenu : NSObject <NSMenuDelegate>
+ (FFFolderMenu*)sharedInstance;
+ (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
- (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
@end
