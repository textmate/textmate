@interface MBMenuDelegate : NSObject <NSMenuDelegate>
+ (id <NSMenuDelegate>)delegateUsingSelector:(SEL)selector;
@end
