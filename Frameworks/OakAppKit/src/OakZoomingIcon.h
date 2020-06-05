#import <oak/debug.h>

@interface OakZoomingIcon : NSWindow
+ (OakZoomingIcon*)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect;
@end
