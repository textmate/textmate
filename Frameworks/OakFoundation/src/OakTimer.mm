#import "OakTimer.h"

OAK_DEBUG_VAR(OakTimer);

@interface OakTimer ()
@property (nonatomic, retain) NSTimer* timer;
- (void)timerDidFire:(NSTimer*)timer;
@end

@interface OakTimerHelper : NSObject
{
	OBJC_WATCH_LEAKS(OakTimerHelper);
	OakTimer* timerProxy; // non-retained
}
- (id)initWithOakTimer:(OakTimer*)aTimer;
@end

@implementation OakTimerHelper
- (id)initWithOakTimer:(OakTimer*)aTimer
{
	if((self = [super init]))
		timerProxy = aTimer;
	return self;
}

- (void)timerDidFire:(NSTimer*)timer
{
	[timerProxy timerDidFire:timer];
}
@end

@implementation OakTimer
@synthesize timer, target, selector, userInfo;

- (id)initWithTimeInterval:(NSTimeInterval)seconds repeats:(BOOL)repeats
{
	if(self = [super init])
	{
		helper     = [[OakTimerHelper alloc] initWithOakTimer:self];
		self.timer = [NSTimer scheduledTimerWithTimeInterval:seconds target:helper selector:@selector(timerDidFire:) userInfo:nil repeats:repeats];
	}
	return self;
}

+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector userInfo:(id)userInfo repeats:(BOOL)repeats
{
	D(DBF_OakTimer, bug("%f %s (%p), %s\n", seconds, [[target description] UTF8String], target, (char const*)aSelector););
	id timer = [[[self alloc] initWithTimeInterval:seconds repeats:repeats] autorelease];
	[timer setTarget:target];
	[timer setSelector:aSelector];
	[timer setUserInfo:userInfo];
	return timer;
}

+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector repeats:(BOOL)repeats
{
	D(DBF_OakTimer, bug("%f %s (%p), %s\n", seconds, [[target description] UTF8String], target, (char const*)aSelector););
	id timer = [[[self alloc] initWithTimeInterval:seconds repeats:repeats] autorelease];
	[timer setTarget:target];
	[timer setSelector:aSelector];
	return timer;
}

- (void)dealloc
{
	D(DBF_OakTimer, bug("\n"););
	[timer invalidate];
	self.timer = nil;
	[helper release];
	[userInfo release];
	[super dealloc];
}

- (void)timerDidFire:(NSTimer*)timer
{
	D(DBF_OakTimer, bug("target: %p, action: %s\n", target, (char const*)selector););
	[target performSelector:selector withObject:self];
}

- (void)fire
{
	[self.timer fire];
}
@end
