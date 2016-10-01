#include "IOAlertPanel.h"
#include <oak/misc.h>

PUBLIC extern NSString* const OakCursorDidHideNotification;

PUBLIC BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSAlternateKeyMask, NSEvent* anEvent = [NSApp currentEvent]);
PUBLIC void OakShowSheetForWindow (NSWindow* sheet, NSWindow* window, void(^callback)(NSInteger));
PUBLIC void OakShowAlertForWindow (NSAlert* alert, NSWindow* window, void(^callback)(NSInteger));

PUBLIC extern NSUInteger const OakMoveMoveReturn;
PUBLIC extern NSUInteger const OakMoveAcceptReturn;
PUBLIC extern NSUInteger const OakMoveCancelReturn;
PUBLIC extern NSUInteger const OakMoveNoActionReturn;

PUBLIC NSUInteger OakPerformTableViewActionFromKeyEvent (NSTableView* tableView, NSEvent* event);
PUBLIC NSUInteger OakPerformTableViewActionFromSelector (NSTableView* tableView, SEL selector);
