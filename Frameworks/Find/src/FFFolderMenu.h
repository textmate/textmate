#import <oak/debug.h>

@interface FFFolderMenu : NSObject <NSMenuDelegate>
+ (void)addFolderMenuAtPath:(NSString*)path toMenuItem:(NSMenuItem*)item withOwner:(id)owner;
@end

@protocol FolderMenuDelegate
- (void)userDidSelectFolder:(NSString*)folder inMenu:(NSMenu*)menu;
@end
