#import <oak/debug.h>

@interface OakZoomingIcon : NSWindow
{
	OBJC_WATCH_LEAKS(OakZoomingIcon);
@private
	NSRect startFrame;
	NSDate* startTime;
	NSTimeInterval duration;
	NSTimer* animationTimer;
}
+ (void)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect;
@end
