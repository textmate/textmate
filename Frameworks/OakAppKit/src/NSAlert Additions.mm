#import "NSAlert Additions.h"

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
	NSAlert* alert = [NSAlert new];

	alert.messageText     = messageText;
	alert.informativeText = informativeText;

	va_list list;
	va_start(list, firstTitle);
	[alert vAddButtons:firstTitle fromList:list];

	return alert;
}
@end
