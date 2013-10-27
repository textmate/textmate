#import <OakAppKit/OakImageAndTextCell.h>
#import <oak/misc.h>

enum {
	OFBPathInfoCellHitCloseButton = (1 << 12),
	OFBPathInfoCellHitOpenItem    = (1 << 13),
	OFBPathInfoCellHitRevealItem  = (1 << 14),
};

PUBLIC @interface OFBPathInfoCell : OakImageAndTextCell
@property (nonatomic, assign) NSUInteger labelIndex;

@property (nonatomic, assign) BOOL isOpen;
@property (nonatomic, assign) BOOL isVisible;
@property (nonatomic, assign) BOOL isLoading;

- (NSRect)closeButtonRectInFrame:(NSRect)cellFrame;
@end
