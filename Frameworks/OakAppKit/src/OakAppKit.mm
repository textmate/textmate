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

#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7)
@interface NSScrollView (Lion)
- (void)setScrollerKnobStyle:(NSScrollerKnobStyle)newKnobStyle;
@end
#endif

void SetLionScrollerKnobStyle (NSScrollView* scrollView, NSScrollerKnobStyle style)
{
	if([scrollView respondsToSelector:@selector(setScrollerKnobStyle:)])
		[scrollView setScrollerKnobStyle:style];
}
