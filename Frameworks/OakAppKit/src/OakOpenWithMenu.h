#import <oak/misc.h>

PUBLIC @interface OakOpenWithMenu : NSObject
+ (void)addOpenWithMenuForPaths:(NSSet*)paths toMenuItem:(NSMenuItem*)aMenuItem;
@end
