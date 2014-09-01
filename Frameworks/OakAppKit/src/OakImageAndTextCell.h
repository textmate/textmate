#import <oak/misc.h>

enum {
	OakImageAndTextCellHitImage = (1 << 10),
	OakImageAndTextCellHitText  = (1 << 11),
};

PUBLIC @interface OakImageAndTextCell : NSTextFieldCell
- (NSRect)imageFrameWithFrame:(NSRect)aRect inControlView:(NSView*)aView;
- (NSRect)textFrameWithFrame:(NSRect)aRect inControlView:(NSView*)aView;
@end
