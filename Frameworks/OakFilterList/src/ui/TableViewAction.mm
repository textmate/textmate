#import "TableViewAction.h"
#import <oak/algorithm.h>

static NSString* const kUserDefaultsEnableLoopFilterList = @"enableLoopFilterList";

NSUInteger const OakMoveMoveReturn     = 0;
NSUInteger const OakMoveAcceptReturn   = 1;
NSUInteger const OakMoveCancelReturn   = 2;
NSUInteger const OakMoveNoActionReturn = 3;

@interface OakTableViewActionHelper : NSResponder
@property (nonatomic) NSTableView* tableView;
@property (nonatomic) NSUInteger returnCode;
@end

@implementation OakTableViewActionHelper
+ (instancetype)tableViewActionHelperWithTableView:(NSTableView*)aTableView
{
	OakTableViewActionHelper* helper = [[self alloc] init];
	helper.tableView  = aTableView;
	helper.returnCode = OakMoveNoActionReturn;
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

		self.returnCode = OakMoveMoveReturn;
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

- (IBAction)insertNewline:(id)sender                    { self.returnCode = OakMoveAcceptReturn; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { self.returnCode = OakMoveAcceptReturn; }
- (IBAction)cancelOperation:(id)sender                  { self.returnCode = OakMoveCancelReturn; }
@end

NSUInteger OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event)
{
	OakTableViewActionHelper* helper = [OakTableViewActionHelper tableViewActionHelperWithTableView:tableView];
	[helper interpretKeyEvents:@[ event ]];
	return helper.returnCode;
}

NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector, NSTextView* textView)
{
	OakTableViewActionHelper* helper = [OakTableViewActionHelper tableViewActionHelperWithTableView:tableView];
	[helper doCommandBySelector:selector];
	return helper.returnCode;
}
