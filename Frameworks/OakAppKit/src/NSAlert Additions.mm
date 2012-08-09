#import "NSAlert Additions.h"

@implementation NSAlert (Other)
- (void)addButtons:(NSString*)firstTitle, ...
{
	va_list ap;
	va_start(ap, firstTitle);
	do {
		[self addButtonWithTitle:firstTitle];
	} while(firstTitle = va_arg(ap, NSString*));
	va_end(ap);
}
@end
