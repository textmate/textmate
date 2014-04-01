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

@implementation OakTextFieldMovementDelegate
- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend sender:(id)sender
{
	if([_tableView numberOfRows])
	{
		if(_tableView.allowsMultipleSelection == NO)
			extend = NO;
		NSInteger row = oak::cap((NSInteger)0, [_tableView selectedRow] + anOffset, [_tableView numberOfRows] - 1);
		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend];
		[_tableView scrollRowToVisible:row];

		if([sender isKindOfClass:[NSSearchField class]])
		{
			NSMutableArray* descriptionBits = [NSMutableArray arrayWithCapacity:[_tableView tableColumns].count];
			[[_tableView tableColumns] enumerateObjectsUsingBlock:^(NSTableColumn* column, NSUInteger index, BOOL* stop) {
				NSCell* cell = [_tableView preparedCellAtColumn:index row:row];
				NSString* description = [cell stringValue];
				[descriptionBits addObject:description];
			}];
			NSString* description = [descriptionBits componentsJoinedByString:@", "];

			id element = [sender isKindOfClass:[NSControl class]] ? [sender cell] : sender;
			NSAccessibilityPostNotificationWithUserInfo(element, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey : description });
		}
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
	if(res.isHighlighted && [self.window isKeyWindow] && [self renderAsKeyViewWithFirstResponder:[self.window firstResponder]])
	{
		res.backgroundStyle = NSBackgroundStyleDark;
		res.highlighted     = NO;
	}
	return res;
}

- (void)highlightSelectionInClipRect:(NSRect)clipRect
{
	if(![self.window isKeyWindow] || ![self renderAsKeyViewWithFirstResponder:[self.window firstResponder]])
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

// =================================================
// = Redraw when linked text field gain/lose focus =
// =================================================

static void* kFirstResponderBinding = &kFirstResponderBinding;

- (void)viewWillMoveToWindow:(NSWindow*)aWindow
{
	if(aWindow)
			[aWindow addObserver:self forKeyPath:@"firstResponder" options:NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld context:kFirstResponderBinding];
	else	[self.window removeObserver:self forKeyPath:@"firstResponder" context:kFirstResponderBinding];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kFirstResponderBinding)
	{
		if([self renderAsKeyViewWithFirstResponder:change[NSKeyValueChangeNewKey]] || [self renderAsKeyViewWithFirstResponder:change[NSKeyValueChangeOldKey]])
		{
			[[self selectedRowIndexes] enumerateRangesInRange:[self rowsInRect:[self visibleRect]] options:0 usingBlock:^(NSRange range, BOOL* stop){
				for(NSUInteger row = range.location; row < NSMaxRange(range); ++row)
				{
					NSRect rect = [self rectOfRow:row];
					rect.size.height -= 1;
					[self setNeedsDisplayInRect:rect];
				}
			}];
		}
	}
}

- (BOOL)renderAsKeyViewWithFirstResponder:(NSResponder*)aView
{
	return aView == self.linkedTextField || ([aView isKindOfClass:[NSText class]] && [(NSText*)aView delegate] == (id)self.linkedTextField);
}
@end
