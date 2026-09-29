#import "OakAppKit.h"
#import <oak/algorithm.h>
#import <crash/src/info.h>
#import <ns/src/ns.h>
#import <oak/debug.h>

NSNotificationName const OakCursorDidHideNotification = @"OakCursorDidHideNotification";

void OakRunIOAlertPanel (char const* format, ...)
{
	va_list ap;
	va_start(ap, format);
	char* buf = NULL;
	vasprintf(&buf, format, ap);
	va_end(ap);

	NSAlert* alert        = [[NSAlert alloc] init];
	alert.messageText     = @(buf);
	alert.informativeText = [NSString stringWithFormat:@"Error: %s", strerror(errno)];
	[alert addButtonWithTitle:@"OK"];
	[alert runModal];

	free(buf);
}

BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags, NSEvent* anEvent)
{
	return ([anEvent type] == NSEventTypeLeftMouseUp || [anEvent type] == NSEventTypeOtherMouseUp || [anEvent type] == NSEventTypeKeyDown) && (([anEvent modifierFlags] & flags) == flags);
}

// ======================
// = TableView Movement =
// ======================

static NSString* const kUserDefaultsEnableLoopFilterList = @"enableLoopFilterList";

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

- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend
{
	if([_tableView numberOfRows])
	{
		NSInteger row = [_tableView selectedRow] + anOffset;
		NSInteger numberOfRows = [_tableView numberOfRows];
		if(std::abs(anOffset) == 1 && numberOfRows && [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsEnableLoopFilterList])
				row = (row + numberOfRows) % numberOfRows;
		else	row = std::clamp(row, (NSInteger)0, numberOfRows - 1);

		if([_tableView.delegate respondsToSelector:@selector(tableView:shouldSelectRow:)])
		{
			while(0 <= row && row < numberOfRows)
			{
				if([_tableView.delegate tableView:_tableView shouldSelectRow:row])
					break;
				row += anOffset > 0 ? +1 : -1;
			}

			if(row < 0 || row >= numberOfRows)
				row = [_tableView selectedRow];
		}

		NSIndexSet* indexes = [NSIndexSet indexSetWithIndex:row];
		if(std::abs(anOffset) > 1 && extend && _tableView.allowsMultipleSelection)
		{
			NSIndexSet* selected = [_tableView selectedRowIndexes];
			NSInteger selectedRow = anOffset < 0 ? [selected firstIndex] : [selected lastIndex];

			NSInteger min = MIN(selectedRow, row);
			NSInteger max = MAX(selectedRow, row);
			indexes = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(min, max - min + 1)];

			if([_tableView.delegate respondsToSelector:@selector(tableView:shouldSelectRow:)])
			{
				indexes = [indexes indexesPassingTest:^BOOL(NSUInteger index, BOOL* stop){
					return [_tableView.delegate tableView:_tableView shouldSelectRow:index];
				}];
			}
		}

		[_tableView selectRowIndexes:indexes byExtendingSelection:extend && _tableView.allowsMultipleSelection];
		[_tableView scrollRowToVisible:row];

		self.returnCode = OakMoveMoveReturn;
	}
}

- (NSInteger)visibleRows                                { return (NSInteger)floor(NSHeight([_tableView visibleRect]) / ([_tableView rowHeight]+[_tableView intercellSpacing].height)) - 1; }

- (void)moveUp:(id)sender                               { [self moveSelectedRowByOffset:-1 extendingSelection:NO];  }
- (void)moveDown:(id)sender                             { [self moveSelectedRowByOffset:+1 extendingSelection:NO];  }
- (void)moveUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-1 extendingSelection:YES]; }
- (void)moveDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+1 extendingSelection:YES]; }
- (void)pageUp:(id)sender                               { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:NO]; }
- (void)pageDown:(id)sender                             { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:NO]; }
- (void)pageUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:YES]; }
- (void)pageDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:YES]; }
- (void)moveToBeginningOfDocument:(id)sender            { [self moveSelectedRowByOffset:-(INT_MAX >> 1) extendingSelection:NO]; }
- (void)moveToEndOfDocument:(id)sender                  { [self moveSelectedRowByOffset:+(INT_MAX >> 1) extendingSelection:NO]; }

- (void)scrollPageUp:(id)sender                         { [self pageUp:sender]; }
- (void)scrollPageDown:(id)sender                       { [self pageDown:sender]; }
- (void)scrollToBeginningOfDocument:(id)sender          { [self moveToBeginningOfDocument:sender]; }
- (void)scrollToEndOfDocument:(id)sender                { [self moveToEndOfDocument:sender]; }

- (IBAction)insertNewline:(id)sender                    { self.returnCode = OakMoveAcceptReturn; }
- (IBAction)insertNewlineIgnoringFieldEditor:(id)sender { self.returnCode = OakMoveAcceptReturn; }
- (IBAction)cancelOperation:(id)sender                  { self.returnCode = OakMoveCancelReturn; }

- (void)doCommandBySelector:(SEL)aSelector
{
	[self tryToPerform:aSelector with:self];
}
@end

NSUInteger OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event)
{
	OakTableViewActionHelper* helper = [OakTableViewActionHelper tableViewActionHelperWithTableView:tableView];
	[helper interpretKeyEvents:@[ event ]];
	return helper.returnCode;
}

NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector)
{
	OakTableViewActionHelper* helper = [OakTableViewActionHelper tableViewActionHelperWithTableView:tableView];
	[helper doCommandBySelector:selector];
	return helper.returnCode;
}
