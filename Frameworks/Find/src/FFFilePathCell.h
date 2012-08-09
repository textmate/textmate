@interface FFFilePathCell : NSCell
{
	NSImage* icon;
	NSString* path;
	NSString* base;
	NSUInteger count;
	BOOL mouseDownInIcon;
}
@property (nonatomic, retain) NSImage* icon;
@property (nonatomic, retain) NSString* path;
@property (nonatomic, retain) NSString* base;
@property (nonatomic, assign) NSUInteger count;

- (NSRect)iconFrameInCellFrame:(NSRect)cellFrame;
@end
