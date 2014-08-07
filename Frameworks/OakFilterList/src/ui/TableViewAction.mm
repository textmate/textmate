#import "TableViewAction.h"
#import <oak/algorithm.h>

static NSString* const kUserDefaultsEnableLoopFilterList = @"enableLoopFilterList";

@interface OakTableViewActionHelper : NSResponder
@property (nonatomic) NSTableView* tableView;
@end

@implementation OakTableViewActionHelper
+ (instancetype)tableViewActionHelperWithTableView:(NSTableView*)aTableView
{
	OakTableViewActionHelper* helper = [[self alloc] init];
	helper.tableView = aTableView;
	return helper;
}

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

- (BOOL)doCommandBySelector:(SEL)aSelector withSender:(id)aSender
{
	static auto const forward = new std::set<SEL>{ @selector(moveUp:), @selector(moveDown:), @selector(moveUpAndModifySelection:), @selector(moveDownAndModifySelection:), @selector(pageUp:), @selector(pageDown:), @selector(movePageUp:), @selector(movePageDown:), @selector(scrollPageUp:), @selector(scrollPageDown:), @selector(moveToBeginningOfDocument:), @selector(moveToEndOfDocument:), @selector(scrollToBeginningOfDocument:), @selector(scrollToEndOfDocument:), @selector(insertNewline:), @selector(insertNewlineIgnoringFieldEditor:), @selector(cancelOperation:) };
	if(forward->find(aSelector) != forward->end() && [self respondsToSelector:aSelector])
		return [NSApp sendAction:aSelector to:self from:aSender];
	return NO;
}
@end

void OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event)
{
	[[OakTableViewActionHelper tableViewActionHelperWithTableView:tableView] interpretKeyEvents:@[ event ]];
}

BOOL OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector, NSTextView* textView)
{
	if(selector == @selector(deleteToBeginningOfLine:) && [textView.window tryToPerform:@selector(delete:) with:textView])
		return YES;
	return [[OakTableViewActionHelper tableViewActionHelperWithTableView:tableView] doCommandBySelector:selector withSender:textView];
}
