#import "OakTimer.h"

OAK_DEBUG_VAR(OakTimer);

@interface OakTimer ()
- (void)timerDidFire:(NSTimer*)timer;
@end

@interface OakRetainedTimerTarget : NSObject
@property (nonatomic, weak) OakTimer* owner;
- (void)timerDidFire:(NSTimer*)timer;
@end

@implementation OakRetainedTimerTarget
- (void)timerDidFire:(NSTimer*)timer
{
	[self.owner timerDidFire:timer];
}
@end

@interface OakTimer ()
@property (nonatomic, weak) id target;
@property (nonatomic) SEL selector;

@property (nonatomic) OakRetainedTimerTarget* timerTarget;
@property (nonatomic) NSTimer* timer;
@end

@implementation OakTimer
- (id)initWithTimeInterval:(NSTimeInterval)seconds repeats:(BOOL)repeats
{
	if(self = [super init])
	{
		self.timerTarget = [OakRetainedTimerTarget new];
		self.timerTarget.owner = self;
		self.timer = [NSTimer scheduledTimerWithTimeInterval:seconds target:self.timerTarget selector:@selector(timerDidFire:) userInfo:nil repeats:repeats];
	}
	return self;
}

+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector userInfo:(id)userInfo repeats:(BOOL)repeats
{
	D(DBF_OakTimer, bug("%f %s (%p), %s\n", seconds, [[target description] UTF8String], target, sel_getName(aSelector)););
	OakTimer* timer = [[self alloc] initWithTimeInterval:seconds repeats:repeats];
	timer.target   = target;
	timer.selector = aSelector;
	timer.userInfo = userInfo;
	return timer;
}

+ (id)scheduledTimerWithTimeInterval:(NSTimeInterval)seconds target:(id)target selector:(SEL)aSelector repeats:(BOOL)repeats
{
	D(DBF_OakTimer, bug("%f %s (%p), %s\n", seconds, [[target description] UTF8String], target, sel_getName(aSelector)););
	return [self scheduledTimerWithTimeInterval:seconds target:target selector:aSelector userInfo:NULL repeats:repeats];
}

- (void)dealloc
{
	D(DBF_OakTimer, bug("\n"););
	[self invalidate];
}

- (void)invalidate
{
	[self.timer invalidate];
	self.timer = nil;
}

- (void)fire
{
	[NSApp sendAction:self.selector to:self.target from:self];
}

- (void)timerDidFire:(NSTimer*)timer
{
	D(DBF_OakTimer, bug("target: %p, action: %s\n", self.target, sel_getName(self.selector)););
	if(self.target)
			[self fire];
	else	[self invalidate];
}
@end
