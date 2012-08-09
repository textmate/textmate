#import <oak/misc.h>
#import <oak/debug.h>

@interface OakToolTip : NSWindow
{
	OBJC_WATCH_LEAKS(OakToolTip);

	NSTextField* field;
	NSTimer* animationTimer;
	NSDate* animationStart;

	NSDate* didOpenAtDate; // ignore mouse moves for the next second
	NSPoint mousePositionWhenOpened;
	BOOL enforceMouseThreshold;
}
- (void)setEnforceMouseThreshold:(BOOL)flag;
- (void)setFont:(NSFont*)aFont;
- (void)setStringValue:(NSString*)aString;

- (void)showAtLocation:(NSPoint)aPoint forScreen:(NSScreen*)aScreen;
@end

PUBLIC void OakShowToolTip (NSString* msg, NSPoint location);
