#import <oak/debug.h>

@protocol OakWindowFrameHelperDelegate <NSObject>
- (NSRect)savableWindowFrame;
@end

PUBLIC @interface OakWindowFrameHelper : NSObject
{
	OBJC_WATCH_LEAKS(OakWindowFrameHelper);

	NSWindow* window;
	NSString* autosaveName;
	Class windowDelegateClass;
}
+ (OakWindowFrameHelper*)windowFrameHelperWithWindow:(NSWindow*)aWindow;
@end
