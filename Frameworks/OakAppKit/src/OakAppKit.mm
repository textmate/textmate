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
	NSRunAlertPanel(@(buf), @"Error: %s", @"OK", nil, nil, strerror(errno));
	free(buf);
}

BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags, NSEvent* anEvent)
{
	return ([anEvent type] == NSLeftMouseUp || [anEvent type] == NSKeyDown) && (([anEvent modifierFlags] & flags) == flags);
}

@interface OakSheetCallbackDelegate : NSObject
@property (nonatomic, copy) void(^callback)(NSInteger);
@property (nonatomic)       id retainedSelf;
@end

@implementation OakSheetCallbackDelegate
- (id)initWithBlock:(void(^)(NSInteger))aBlock
{
	if(self = [super init])
	{
		self.callback = aBlock;
		self.retainedSelf = self;
	}
	return self;
}

- (void)sheetDidEnd:(id)sheetOrAlert returnCode:(NSInteger)returnCode contextInfo:(void*)unused
{
	self.callback(returnCode);
	self.retainedSelf = nil;
}
@end

void OakShowSheetForWindow (NSWindow* sheet, NSWindow* window, void(^callback)(NSInteger))
{
	crash_reporter_info_t info(to_s([NSString stringWithFormat:@"sheet %@, window %@: title ‘%@’, is visible %s, is sheet %s, has sheet %s, delegate %@", sheet, window, window.title, BSTR(window.isVisible), BSTR(window.isSheet), BSTR(window.attachedSheet), window.delegate]));
	OakSheetCallbackDelegate* delegate = [[OakSheetCallbackDelegate alloc] initWithBlock:callback];
	[NSApp beginSheet:sheet modalForWindow:window modalDelegate:delegate didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
}

void OakShowAlertForWindow (NSAlert* alert, NSWindow* window, void(^callback)(NSInteger))
{
	OakSheetCallbackDelegate* delegate = [[OakSheetCallbackDelegate alloc] initWithBlock:callback];
	if(window)
			[alert beginSheetModalForWindow:window modalDelegate:delegate didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	else	[delegate sheetDidEnd:alert returnCode:[alert runModal] contextInfo:NULL];
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

- (void)moveUp:(id)sender                               { [self moveSelectedRowByOffset:-1 extendingSelection:NO];  }
- (void)moveDown:(id)sender                             { [self moveSelectedRowByOffset:+1 extendingSelection:NO];  }
- (void)moveUpAndModifySelection:(id)sender             { [self moveSelectedRowByOffset:-1 extendingSelection:YES]; }
- (void)moveDownAndModifySelection:(id)sender           { [self moveSelectedRowByOffset:+1 extendingSelection:YES]; }
- (void)movePageUp:(id)sender                           { [self moveSelectedRowByOffset:-[self visibleRows] extendingSelection:NO]; }
- (void)movePageDown:(id)sender                         { [self moveSelectedRowByOffset:+[self visibleRows] extendingSelection:NO]; }
- (void)moveToBeginningOfDocument:(id)sender            { [self moveSelectedRowByOffset:-(INT_MAX >> 1) extendingSelection:NO]; }
- (void)moveToEndOfDocument:(id)sender                  { [self moveSelectedRowByOffset:+(INT_MAX >> 1) extendingSelection:NO]; }

- (void)pageUp:(id)sender                               { [self movePageUp:sender]; }
- (void)pageDown:(id)sender                             { [self movePageDown:sender]; }
- (void)scrollPageUp:(id)sender                         { [self movePageUp:sender]; }
- (void)scrollPageDown:(id)sender                       { [self movePageDown:sender]; }
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

NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector, NSTextView* textView)
{
	OakTableViewActionHelper* helper = [OakTableViewActionHelper tableViewActionHelperWithTableView:tableView];
	[helper doCommandBySelector:selector];
	return helper.returnCode;
}

// ======================

#if !defined(MAC_OS_X_VERSION_10_10) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_10)
// 10.9 and 10.10 SDKs don't define NSAppKitVersionNumber10_9 (nor *_10)
// literal value taken of 1265 from https://developer.apple.com/library/prerelease/mac/releasenotes/AppKit/RN-AppKit/index.html
#ifndef NSAppKitVersionNumber10_9
#define NSAppKitVersionNumber10_9 1265
#endif
NSString *const _NSAccessibilitySharedFocusElementsAttribute = @"AXSharedFocusElements";
NSString *const *const pNSAccessibilitySharedFocusElementsAttribute = (floor(NSAppKitVersionNumber) <= NSAppKitVersionNumber10_9) ? nil : &_NSAccessibilitySharedFocusElementsAttribute;
#endif