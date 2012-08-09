#import <OakAppKit/OakImageAndTextCell.h>
#import <oak/debug.h>

@class OakTimer;

enum {
	OFBPathInfoCellHitCloseButton = (12 << 1),
};

@interface OFBPathInfoCell : OakImageAndTextCell
{
	OBJC_WATCH_LEAKS(OFBPathInfoCell);
	NSUInteger labelIndex;
	BOOL mouseDownInCloseButton;
	BOOL isOpen;
	BOOL isVisible;
	BOOL isLoading;
	double spinnerValue;
	OakTimer* spinTimer;
}
@property (nonatomic, assign) NSUInteger labelIndex;

@property (nonatomic, assign) BOOL isOpen;
@property (nonatomic, assign) BOOL isVisible;
@property (nonatomic, assign) BOOL isLoading;
@property (nonatomic, assign) BOOL mouseDownInCloseButton;

- (NSRect)closeButtonRectInFrame:(NSRect)cellFrame;
- (BOOL)isMouseInCloseButtonInFrame:(NSRect)cellFrame controlView:(NSView*)controlView;
@end
