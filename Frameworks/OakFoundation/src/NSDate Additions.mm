#import "NSDate Additions.h"

@implementation NSDate (HumanDates)
- (NSString*)humanReadableTimeElapsed
{
	NSTimeInterval const minute =  60;
	NSTimeInterval const hour   =  60*minute;
	NSTimeInterval const day    =  24*hour;
	NSTimeInterval const week   =   7*day;
	NSTimeInterval const month  =  31*day;
	NSTimeInterval const year   = 365*day;

	NSTimeInterval t = -[self timeIntervalSinceNow];
	if(t < 1)
		return @"Just now";
	if(t < minute)
		return @"Less than a minute ago";
	if(t < 2 * minute)
		return @"1 minute ago";
	if(t < hour)
		return [NSString stringWithFormat:@"%.0f minutes ago", t / minute];
	if(t < 2 * hour)
		return @"1 hour ago";
	if(t < day)
		return [NSString stringWithFormat:@"%.0f hours ago", t / hour];
	if(t < 2*day)
		return @"Yesterday";
	if(t < week)
		return[NSString stringWithFormat:@"%.0f days ago", t / day];
	if(t < 2*week)
		return @"Last week";
	if(t < month)
		return [NSString stringWithFormat:@"%.0f weeks ago", t / week];
	if(t < 2*month)
		return @"Last month";
	if(t < year)
		return [NSString stringWithFormat:@"%.0f months ago", t / month];
	if(t < 2*year)
		return @"Last year";

	return [NSString stringWithFormat:@"%.0f years ago", t / year];
}
@end
