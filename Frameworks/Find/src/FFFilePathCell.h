@interface FFFilePathCell : NSCell
@property (nonatomic, retain) NSImage* icon;
@property (nonatomic, retain) NSString* path;
@property (nonatomic, retain) NSString* base;
@property (nonatomic, assign) NSUInteger count;

- (NSRect)iconFrameInCellFrame:(NSRect)cellFrame;
@end
