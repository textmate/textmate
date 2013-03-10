#import <oak/misc.h>
#import <oak/debug.h>

@interface OakToolTip : NSWindow
- (void)setEnforceMouseThreshold:(BOOL)flag;
- (void)setFont:(NSFont*)aFont;
- (void)setStringValue:(NSString*)aString;

- (void)showAtLocation:(NSPoint)aPoint forScreen:(NSScreen*)aScreen;
@end

PUBLIC void OakShowToolTip (NSString* msg, NSPoint location);
