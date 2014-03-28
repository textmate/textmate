#import "OakFoundation.h"

BOOL OakIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL OakNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}
