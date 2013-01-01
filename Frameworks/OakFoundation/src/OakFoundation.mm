#import "OakFoundation.h"

BOOL NSIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL NSNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}
