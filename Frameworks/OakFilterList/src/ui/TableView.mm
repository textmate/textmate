#import "TableView.h"

@interface OakInactiveTableRowView ()
{
	BOOL _effectiveDrawAsHighlighted;
}
@end

@implementation OakInactiveTableRowView
- (void)updateDrawAsHighlighted
{
	BOOL flag = self.selected && _drawAsHighlighted && self.window.isKeyWindow;
	if(_effectiveDrawAsHighlighted == flag)
		return;
	_effectiveDrawAsHighlighted = flag;

	for(NSView* view in [self subviews])
	{
		if([view respondsToSelector:@selector(setBackgroundStyle:)])
			[(NSTableCellView*)view setBackgroundStyle:self.interiorBackgroundStyle];
		if([view respondsToSelector:@selector(cell)])
			[[(NSControl*)view cell] setBackgroundStyle:self.interiorBackgroundStyle];
	}

	[self setNeedsDisplay:YES];
}

- (void)setDrawAsHighlighted:(BOOL)flag
{
	_drawAsHighlighted = flag;
	[self updateDrawAsHighlighted];
}

- (void)setSelected:(BOOL)flag
{
	[super setSelected:flag];
	[self updateDrawAsHighlighted];
}

- (void)setEmphasized:(BOOL)flag
{
	[super setEmphasized:flag];
	[self updateDrawAsHighlighted];
}

- (NSBackgroundStyle)interiorBackgroundStyle
{
	return _effectiveDrawAsHighlighted ? NSBackgroundStyleDark : [super interiorBackgroundStyle];
}

- (void)drawSelectionInRect:(NSRect)dirtyRect
{
	if(!_effectiveDrawAsHighlighted)
		return [super drawSelectionInRect:dirtyRect];

	[[NSColor alternateSelectedControlColor] set];
	NSRectFill(NSIntersectionRect(NSOffsetRect(NSInsetRect(self.bounds, 0, 0.5), 0, -0.5), dirtyRect));
}
@end
