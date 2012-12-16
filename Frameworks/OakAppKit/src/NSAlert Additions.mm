#import "NSAlert Additions.h"

@interface OakAlertCallbackDelegate : NSObject
@property (nonatomic, retain) NSAlert* alert;
@property (nonatomic, copy)   void(^callback)(NSAlert*, NSInteger);
@end

@implementation OakAlertCallbackDelegate
- (void)alertSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(void*)info
{
	self.callback(self.alert, returnCode);
	[self release];
}

- (void)dealloc
{
	self.callback = nil;
	self.alert    = nil;
	[super dealloc];
}
@end

void OakShowAlert (NSAlert* alert, NSWindow* window, void(^callback)(NSAlert*, NSInteger))
{
	OakAlertCallbackDelegate* delegate = [OakAlertCallbackDelegate new];
	delegate.callback = callback;
	delegate.alert    = alert;

	if(window)
			[alert beginSheetModalForWindow:window modalDelegate:delegate didEndSelector:@selector(alertSheetDidEnd:returnCode:contextInfo:) contextInfo:NULL];
	else	[delegate alertSheetDidEnd:alert returnCode:[alert runModal] contextInfo:NULL];
}

@implementation NSAlert (Other)
- (void)vAddButtons:(NSString*)firstTitle fromList:(va_list)list
{
	do {
		[self addButtonWithTitle:firstTitle];
	} while(firstTitle = va_arg(list, NSString*));
	va_end(list);
}

- (void)addButtons:(NSString*)firstTitle, ...
{
	va_list list;
	va_start(list, firstTitle);
	[self vAddButtons:firstTitle fromList:list];
}

+ (NSAlert*)tmAlertWithMessageText:(NSString*)messageText informativeText:(NSString*)informativeText buttons:(NSString*)firstTitle, ...
{
	NSAlert* alert = [[[NSAlert alloc] init] autorelease];

	alert.messageText     = messageText;
	alert.informativeText = informativeText;

	va_list list;
	va_start(list, firstTitle);
	[alert vAddButtons:firstTitle fromList:list];

	return alert;
}
@end
