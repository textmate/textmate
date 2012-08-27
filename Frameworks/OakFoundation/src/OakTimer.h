#import <oak/debug.h>

@class OakTimerHelper;

PUBLIC @interface OakTimer : NSObject
{
	OBJC_WATCH_LEAKS(OakTimer);

	OakTimerHelper* helper;
	NSTimer* timer;
	id target;
	SEL selector;
	id userInfo;
}
+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector repeats:(BOOL)repeats;
+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector userInfo:(id)userInfo repeats:(BOOL)repeats;
- (void)fire;
@property (nonatomic, assign) id target;
@property (nonatomic, assign) SEL selector;
@property (nonatomic, retain) id userInfo;
@end
