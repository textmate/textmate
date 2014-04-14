#import "CWItem.h"

@implementation CWItem
- (CWItem*)initWithPath:(NSString*)aPath andSCMStatus:(NSString*)aStatus commit:(BOOL)state
{
	if((self = [super init]))
	{
		_path  = [aPath stringByStandardizingPath];
		_scmStatus = aStatus;
		_commit = state;
	}
	return self;
}

+ (CWItem*)itemWithPath:(NSString*)aPath andSCMStatus:(NSString*)aStatus commit:(BOOL)state
{
	return [[CWItem alloc] initWithPath:aPath andSCMStatus:aStatus commit:state];
}

- (id)copyWithZone:(NSZone*)zone
{
	CWItem* newItem = [[CWItem allocWithZone:zone] initWithPath:_path andSCMStatus:_scmStatus commit:_commit];
	return newItem;
}

- (NSComparisonResult)compare:(CWItem*)item
{
	return [[self path] compare:[item path]];
}
@end
