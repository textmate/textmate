#import "OakAppKit.h"
#import <oak/algorithm.h>
#import <crash/info.h>
#import <ns/ns.h>
#import <oak/debug.h>

NSString* const OakCursorDidHideNotification = @"OakCursorDidHideNotification";

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
	return ([anEvent type] == NSLeftMouseUp || [anEvent type] == NSOtherMouseUp || [anEvent type] == NSKeyDown) && (([anEvent modifierFlags] & flags) == flags);
}

// ======================
// = TableView Movement =
// ======================

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

- (void)moveSelectedRowByOffset:(NSInteger)anOffset extendingSelection:(BOOL)extend
{
	if([_tableView numberOfRows])
	{
		NSInteger row = [_tableView selectedRow] + anOffset;
		NSInteger numberOfRows = [_tableView numberOfRows];
		if(std::abs(anOffset) == 1 && numberOfRows && [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsEnableLoopFilterList])
				row = (row + numberOfRows) % numberOfRows;
		else	row = oak::cap((NSInteger)0, row, numberOfRows - 1);

		[_tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:extend && _tableView.allowsMultipleSelection];
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
