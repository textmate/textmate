extern NSUInteger const OakViewApplicationIsActiveMask;
extern NSUInteger const OakViewWindowIsMainMask;
extern NSUInteger const OakViewWindowIsKeyMask;
extern NSUInteger const OakViewViewIsFirstResponderMask;

@interface OakView : NSView
@property (nonatomic) NSUInteger keyState;
@end
