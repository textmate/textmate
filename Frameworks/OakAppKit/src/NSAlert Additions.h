#import <oak/misc.h>

@interface NSAlert (Other)
+ (NSAlert*)tmAlertWithMessageText:(NSString*)messageText informativeText:(NSString*)informativeText buttons:(NSString*)firstTitle, ...;
- (void)addButtons:(NSString*)firstTitle, ...;
@end
