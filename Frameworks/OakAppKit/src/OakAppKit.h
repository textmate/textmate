#include "IOAlertPanel.h"
#include <oak/misc.h>

PUBLIC extern NSString* const OakCursorDidHideNotification;

PUBLIC NSBox* OakCreateViewWithColor (NSColor* color = nil, NSColor* secondaryColor = nil);
PUBLIC NSBox* OakCreateVerticalLine (NSColor* primaryColor, NSColor* secondaryColor = nil);
PUBLIC NSBox* OakCreateHorizontalLine (NSColor* primaryColor, NSColor* secondaryColor = nil);

PUBLIC BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSAlternateKeyMask, NSEvent* anEvent = [NSApp currentEvent]);
PUBLIC void OakShowSheetForWindow (NSWindow* sheet, NSWindow* window, void(^callback)(NSInteger));
PUBLIC void OakShowAlertForWindow (NSAlert* alert, NSWindow* window, void(^callback)(NSInteger));
