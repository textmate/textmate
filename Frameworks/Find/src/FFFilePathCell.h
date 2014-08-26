@interface FFFilePathCell : NSCell
@property (nonatomic) NSImage* icon;
@property (nonatomic) NSString* path;
@property (nonatomic) NSAttributedString* displayPath;
@property (nonatomic) NSUInteger count;
@property (nonatomic) NSString* charset;

- (NSRect)iconFrameInCellFrame:(NSRect)cellFrame;
@end
