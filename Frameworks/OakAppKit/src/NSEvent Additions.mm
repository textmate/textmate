#import "NSEvent Additions.h"
#import <oak/oak.h>

@implementation NSEvent (SnowLeopardCompatibilityWrappers)
+ (NSTimeInterval)caretBlinkInterval
{
	return [([NSUserDefaults.standardUserDefaults objectForKey:@"NSTextInsertionPointBlinkPeriod"] ?: @567) doubleValue] / 1000.0;
}
@end
