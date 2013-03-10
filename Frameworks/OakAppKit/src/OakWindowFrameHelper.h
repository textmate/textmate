#import <oak/debug.h>

@protocol OakWindowFrameHelperDelegate <NSObject>
- (NSRect)savableWindowFrame;
@end

PUBLIC @interface OakWindowFrameHelper : NSObject
+ (OakWindowFrameHelper*)windowFrameHelperWithWindow:(NSWindow*)aWindow;
@end
