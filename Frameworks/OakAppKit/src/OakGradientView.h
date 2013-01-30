#import <oak/misc.h>

PUBLIC @interface OakGradientView : NSView
- (id)initWithGradient:(NSGradient*)activeGradient inactiveGradient:(NSGradient*)inactiveGradient;
@end
