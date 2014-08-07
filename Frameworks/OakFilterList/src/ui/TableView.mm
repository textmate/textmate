#import "TableView.h"
#import <OakAppKit/OakAppKit.h> // Accessibility API missing from 10.7/10.8 SDK.

@implementation OakInactiveTableView
- (NSCell*)preparedCellAtColumn:(NSInteger)column row:(NSInteger)row
{
	NSCell* res = [super preparedCellAtColumn:column row:row];
	if(res.isHighlighted && [self.window isKeyWindow] && self.drawAsHighlighted)
	{
		res.backgroundStyle = NSBackgroundStyleDark;
		res.highlighted     = NO;
	}
	return res;
}

- (void)highlightSelectionInClipRect:(NSRect)clipRect
{
	if(![self.window isKeyWindow] || !self.drawAsHighlighted)
		return [super highlightSelectionInClipRect:clipRect];

	[[NSColor alternateSelectedControlColor] set];
	[[self selectedRowIndexes] enumerateRangesInRange:[self rowsInRect:clipRect] options:0 usingBlock:^(NSRange range, BOOL* stop){
		for(NSUInteger row = range.location; row < NSMaxRange(range); ++row)
		{
			NSRect rect = [self rectOfRow:row];
			rect.size.height -= 1;
			NSRectFill(rect);
		}
	}];
}

- (void)setDrawAsHighlighted:(BOOL)flag
{
	if(_drawAsHighlighted == flag)
		return;

	_drawAsHighlighted = flag;

	[[self selectedRowIndexes] enumerateRangesInRange:[self rowsInRect:[self visibleRect]] options:0 usingBlock:^(NSRange range, BOOL* stop){
		for(NSUInteger row = range.location; row < NSMaxRange(range); ++row)
		{
			NSRect rect = [self rectOfRow:row];
			rect.size.height -= 1;
			[self setNeedsDisplayInRect:rect];
		}
	}];
}

- (void)selectRowIndexes:(NSIndexSet*)indexes byExtendingSelection:(BOOL)extend
{
	BOOL announce = _drawAsHighlighted && [indexes count] && self.window.firstResponder != self && ![indexes isEqualToIndexSet:[self selectedRowIndexes]];
	[super selectRowIndexes:indexes byExtendingSelection:extend];
	if(announce && (nil == &NSAccessibilitySharedFocusElementsAttribute))
	{
		NSInteger selectedRow = [indexes lastIndex];

		NSMutableArray* descriptionBits = [NSMutableArray arrayWithCapacity:[self tableColumns].count];
		[[self tableColumns] enumerateObjectsUsingBlock:^(NSTableColumn* column, NSUInteger index, BOOL* stop) {
			NSCell* cell = [self preparedCellAtColumn:index row:selectedRow];
			NSString* description = (NSString*)[[cell accessibilityAttributeValue:NSAccessibilityValueAttribute] description];
			[descriptionBits addObject:description];
		}];
		NSString* description = [descriptionBits componentsJoinedByString:@", "];

		id element = self.window.firstResponder;

		// Is first responder the field editor? Then we must get the actual view.
		if([element respondsToSelector:@selector(delegate)] && [[element performSelector:@selector(delegate)] isKindOfClass:[NSControl class]])
			element = [element performSelector:@selector(delegate)];

		// Is the view an NSControl? Then we must use its cell for accessibility notifications.
		element = [element isKindOfClass:[NSControl class]] ? [element cell] : element;

		if([element respondsToSelector:@selector(accessibilityIsIgnored)] && ![element accessibilityIsIgnored])
			NSAccessibilityPostNotificationWithUserInfo(element, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey : description });
	}
}
@end
