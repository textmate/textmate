#include "IOAlertPanel.h"
extern NSNotificationName const OakCursorDidHideNotification;

BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSEventModifierFlagOption, NSEvent* anEvent = [NSApp currentEvent]);

typedef NS_ENUM(NSUInteger, OakPerformTableViewActionResult) {
	OakMoveMoveReturn,
	OakMoveAcceptReturn,
	OakMoveCancelReturn,
	OakMoveNoActionReturn,
};

NSUInteger OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event);
NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector);
