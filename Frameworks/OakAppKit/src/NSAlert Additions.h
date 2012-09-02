@interface NSAlert (Other)
- (void)addButtons:(NSString*)firstTitle, ...;

+ (NSAlert *) tmAlertWithMessageText:(NSString *) messageTitle informativeText:(NSString *) informativeText buttons:(NSString *) firstTitle, ...;
@end
