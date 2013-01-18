#import <OakAppKit/OakImageAndTextCell.h>

enum {
	OFBPathInfoCellHitCloseButton = (12 << 1),
};

@interface OFBPathInfoCell : OakImageAndTextCell
@property (nonatomic, assign) NSUInteger labelIndex;

@property (nonatomic, assign) BOOL isOpen;
@property (nonatomic, assign) BOOL isVisible;
@property (nonatomic, assign) BOOL isLoading;

- (NSRect)closeButtonRectInFrame:(NSRect)cellFrame;
@end
