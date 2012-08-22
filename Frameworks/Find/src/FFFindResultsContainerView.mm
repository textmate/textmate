@interface FFFindResultsContainerView : NSView
{
	NSView* header;
	NSView* list;
}
@end

@implementation FFFindResultsContainerView
- (void)setFrame:(NSRect)frame
{
	[super setFrame:frame];
	[header setFrame:NSMakeRect(0, frame.size.height - header.frame.size.height, frame.size.width, header.frame.size.height)];
	[list setFrame:NSMakeRect(-1, 0, frame.size.width+2, frame.size.height - header.frame.size.height+1)];
}
@end
