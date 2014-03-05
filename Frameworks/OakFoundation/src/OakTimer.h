#import <oak/debug.h>

// Unline NSTimer this class does not retain ‘target’.
// Additionally if you release an instance from this class, it will invalidate the timer.

PUBLIC @interface OakTimer : NSObject
+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector repeats:(BOOL)repeats;
+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector userInfo:(id)userInfo repeats:(BOOL)repeats;
@property (nonatomic) id userInfo;
- (void)invalidate;
@end
