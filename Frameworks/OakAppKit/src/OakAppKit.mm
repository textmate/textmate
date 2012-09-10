#import "OakAppKit.h"

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
