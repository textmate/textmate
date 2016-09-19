@interface FFFolderMenu : NSObject <NSMenuDelegate>
+ (instancetype)sharedInstance;
+ (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
- (void)addFolderSubmenuToMenuItem:(NSMenuItem*)aMenuItem;
@end
