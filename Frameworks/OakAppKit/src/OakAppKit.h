#include "IOAlertPanel.h"
extern NSNotificationName const OakCursorDidHideNotification;

BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSEventModifierFlagOption, NSEvent* anEvent = [NSApp currentEvent]);

extern NSUInteger const OakMoveMoveReturn;
extern NSUInteger const OakMoveAcceptReturn;
extern NSUInteger const OakMoveCancelReturn;
extern NSUInteger const OakMoveNoActionReturn;

NSUInteger OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event);
NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector);
