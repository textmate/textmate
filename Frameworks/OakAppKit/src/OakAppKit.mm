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

#if !defined(MAC_OS_X_VERSION_10_10) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_10)
// 10.9 and 10.10 SDKs don't define NSAppKitVersionNumber10_9 (nor *_10)
// literal value taken of 1265 from https://developer.apple.com/library/prerelease/mac/releasenotes/AppKit/RN-AppKit/index.html
#ifndef NSAppKitVersionNumber10_9
#define NSAppKitVersionNumber10_9 1265
#endif
NSString *const NSAccessibilitySharedFocusElementsAttribute = (floor(NSAppKitVersionNumber) <= NSAppKitVersionNumber10_9) ? nil : @"AXSharedFocusElements";
#endif
