#import "OakFoundation.h"

NSNumber* const YES_obj = [[NSNumber alloc] initWithBool:YES];
NSNumber* const NO_obj  = [[NSNumber alloc] initWithBool:NO];

BOOL NSIsEmptyString (NSString* str)
{
	return !str || [str isEqualToString:@""];
}

BOOL NSNotEmptyString (NSString* str)
{
	return str && ![str isEqualToString:@""];
}
