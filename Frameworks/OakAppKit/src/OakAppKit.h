#include "IOAlertPanel.h"
#include <oak/misc.h>

PUBLIC extern NSString* const OakCursorDidHideNotification;

PUBLIC BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSAlternateKeyMask, NSEvent* anEvent = [NSApp currentEvent]);
PUBLIC void OakShowSheetForWindow (NSWindow* sheet, NSWindow* window, void(^callback)(NSInteger));
PUBLIC void OakShowAlertForWindow (NSAlert* alert, NSWindow* window, void(^callback)(NSInteger));

#if !defined(MAC_OS_X_VERSION_10_9) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_9)
APPKIT_EXTERN void NSAccessibilityPostNotificationWithUserInfo(id element, NSString *notification, NSDictionary *userInfo) NS_AVAILABLE_MAC(10_7);
APPKIT_EXTERN NSString *const NSAccessibilityAnnouncementRequestedNotification NS_AVAILABLE_MAC(10_7);
APPKIT_EXTERN NSString *const NSAccessibilityAnnouncementKey            NS_AVAILABLE_MAC(10_7);
#endif
