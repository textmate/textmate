#include "IOAlertPanel.h"
#include <oak/misc.h>

PUBLIC extern NSString* const OakCursorDidHideNotification;

PUBLIC BOOL OakIsAlternateKeyOrMouseEvent (NSUInteger flags = NSAlternateKeyMask, NSEvent* anEvent = [NSApp currentEvent]);

#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7)
enum {
	NSScrollerKnobStyleDefault  = 0,
	NSScrollerKnobStyleDark     = 1,
	NSScrollerKnobStyleLight    = 2
};
typedef NSInteger NSScrollerKnobStyle;
#endif

PUBLIC void SetLionScrollerKnobStyle (NSScrollView* scrollView, NSScrollerKnobStyle style);
