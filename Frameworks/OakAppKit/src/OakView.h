#import <oak/misc.h>

PUBLIC extern NSUInteger const OakViewApplicationIsActiveMask;
PUBLIC extern NSUInteger const OakViewWindowIsMainMask;
PUBLIC extern NSUInteger const OakViewWindowIsKeyMask;
PUBLIC extern NSUInteger const OakViewViewIsFirstResponderMask;

PUBLIC @interface OakView : NSView
@property (nonatomic) NSUInteger keyState;
@end
