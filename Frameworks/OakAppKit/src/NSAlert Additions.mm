#import "NSAlert Additions.h"

@implementation NSAlert (Other)
- (void)vaddButtons:(NSString*)firstTitle fromList:(va_list) list
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
	[self vaddButtons:firstTitle fromList:list];
}

+ (NSAlert *) tmAlertWithMessageText:(NSString *) messageTitle informativeText:(NSString *) informativeText buttons:(NSString *) firstTitle, ...
{
	NSAlert *alert = [[NSAlert alloc] init];

	alert.messageText = messageTitle;
	alert.informativeText = informativeText;

	va_list list;
	va_start(list, firstTitle);
	[alert vaddButtons:firstTitle fromList:list];

	return alert;
}
@end
