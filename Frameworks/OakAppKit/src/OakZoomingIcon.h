#import <oak/debug.h>

PUBLIC @interface OakZoomingIcon : NSWindow
+ (OakZoomingIcon*)zoomIcon:(NSImage*)icon fromRect:(NSRect)aRect;
@end
