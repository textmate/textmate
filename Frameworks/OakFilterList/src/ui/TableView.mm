#import "TableView.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h> // Accessibility API missing from 10.7/10.8 SDK.
#import <oak/algorithm.h>

// ========================================
// = Forward NSTextField Movement Actions =
// ========================================

@interface OakTextFieldMovementDelegate : NSObject <NSTextFieldDelegate>
@property (nonatomic, weak) NSTableView* tableView;
@end

static NSString* const kUserDefaultsEnableLoopFilterList = @"enableLoopFilterList";

@implementation OakTextFieldMovementDelegate
- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend sender:(id)sender
{
	if([_tableView numberOfRows])
	{
		if(_tableView.allowsMultipleSelection == NO)
			extend = NO;

		NSInteger row = [_tableView selectedRow] + anOffset;
		NSInteger numberOfRows = [_tableView numberOfRows];
		if(abs(anOffset) == 1 && numberOfRows && [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsEnableLoopFilterList])
				row = (row + numberOfRows) % numberOfRows;
		else	row = oak::cap((NSInteger)0, row, numberOfRows - 1);

		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend];
		[_tableView scrollRowToVisible:row];
	}
}

- (int)visibleRows                                      { return (int)floor(NSHeight([_tableView visibleRect]) / ([_tableView rowHeight]+[_tableView intercellSpacing].height)) - 1; }

- (void)moveUp:(id)sender                               { [self moveSelectedRowByOffset:-1 extendingSelection:NO sender:sender];  }
- (void)moveDown:(id)sender                             { [self moveSelectedRowByOffset:+1 extendingSelection:NO sender:sender];  }
- (void)moveUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-1 extendingSelection:YES sender:sender]; }
- (void)moveDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+1 extendingSelection:YES sender:sender]; }
- (void)movePageUp:(id)sender                           { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:NO sender:sender]; }
- (void)movePageDown:(id)sender                         { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:NO sender:sender]; }
- (void)moveToBeginningOfDocument:(id)sender            { [self moveSelectedRowByOffset:-(INT_MAX >> 1) extendingSelection:NO sender:sender]; }
- (void)moveToEndOfDocument:(id)sender                  { [self moveSelectedRowByOffset:+(INT_MAX >> 1) extendingSelection:NO sender:sender]; }

- (void)pageUp:(id)sender                               { [self movePageUp:sender]; }
- (void)pageDown:(id)sender                             { [self movePageDown:sender]; }
- (void)scrollPageUp:(id)sender                         { [self movePageUp:sender]; }
- (void)scrollPageDown:(id)sender                       { [self movePageDown:sender]; }
- (void)scrollToBeginningOfDocument:(id)sender          { [self moveToBeginningOfDocument:sender]; }
- (void)scrollToEndOfDocument:(id)sender                { [self moveToEndOfDocument:sender]; }

- (IBAction)insertNewline:(id)sender                    { [NSApp sendAction:@selector(accept:) to:nil from:sender]; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { [NSApp sendAction:@selector(accept:) to:nil from:sender]; }
- (IBAction)cancelOperation:(id)sender                  { [NSApp sendAction:@selector(cancel:) to:nil from:sender]; }

- (BOOL)control:(NSControl*)aControl textView:(NSTextView*)aTextView doCommandBySelector:(SEL)aCommand
{
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(movePageUp:), @selector(movePageDown:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(scrollToBeginningOfDocument:), @selector(scrollToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(aCommand == @selector(deleteToBeginningOfLine:) && [aControl.window tryToPerform:@selector(delete:) with:aControl])
		return YES;
	else if(forward->find(aCommand) != forward->end() && [self respondsToSelector:aCommand])
		return [NSApp sendAction:aCommand to:self from:aControl];
	return NO;
}
@end

// ========================================

@interface OakInactiveTableView ()
@property (nonatomic) OakTextFieldMovementDelegate* textFieldMovementDelegate;
@end

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

- (void)setLinkedTextField:(NSTextField*)aTextField
{
	_textFieldMovementDelegate = [OakTextFieldMovementDelegate new];
	_textFieldMovementDelegate.tableView = self;

	_linkedTextField.delegate = nil;
	_linkedTextField = aTextField;
	_linkedTextField.delegate = _textFieldMovementDelegate;
}

- (void)dealloc
{
	_linkedTextField.delegate = nil;
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
