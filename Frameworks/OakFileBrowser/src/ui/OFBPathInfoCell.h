#import <OakAppKit/OakImageAndTextCell.h>
#import <oak/misc.h>

enum {
	OFBPathInfoCellHitCloseButton = (1 << 12),
	OFBPathInfoCellHitOpenItem    = (1 << 13),
	OFBPathInfoCellHitRevealItem  = (1 << 14),
};

PUBLIC @interface OFBPathInfoCell : OakImageAndTextCell
@property (nonatomic) NSUInteger labelIndex;

@property (nonatomic) BOOL isOpen;
@property (nonatomic) BOOL isVisible;
@property (nonatomic) BOOL isLoading;

- (NSRect)closeButtonRectInFrame:(NSRect)cellFrame;
@end
