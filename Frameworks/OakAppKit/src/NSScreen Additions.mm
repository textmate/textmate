#import "NSScreen Additions.h"

@implementation NSScreen (RestrainFrames)
+ (NSScreen*)screenWithFrame:(NSRect)rect
{
	std::multimap<CGFloat, NSScreen*> rank;
	for(NSScreen* scr in [NSScreen screens])
	{
		NSRect r = NSIntersectionRect([scr visibleFrame], rect);
		rank.emplace(- NSWidth(r) * NSHeight(r), scr);
	}

	if(rank.empty() || rank.begin()->first == 0.0)
			return [NSScreen mainScreen];
	else	return rank.begin()->second;
}

- (NSRect)restrainFrameToVisibleScreen:(NSRect)aFrame
{
	if([[NSUserDefaults standardUserDefaults] integerForKey:@"OakScreenDisableWindowRestraining"])
		return aFrame;

	NSRect r = [self visibleFrame];
	CGFloat scrX0 = NSMinX(r), scrX1 = NSMaxX(r);
	CGFloat scrY0 = NSMinY(r), scrY1 = NSMaxY(r);

	CGFloat x0 = std::max(NSMinX(aFrame), scrX0);
	CGFloat x1 = std::min(NSMaxX(aFrame), scrX1);
	CGFloat y0 = std::max(NSMinY(aFrame), scrY0);
	CGFloat y1 = std::min(NSMaxY(aFrame), scrY1);

	CGFloat extra_width = NSWidth(aFrame) - (x1 - x0);
	CGFloat extra_height = NSHeight(aFrame) - (y1 - y0);

	x0 = std::max(x0 - extra_width, scrX0);
	x1 = std::min(x1 + extra_width, scrX1);
	y0 = std::max(y0 - extra_height, scrY0);
	y1 = std::min(y1 + extra_height, scrY1);

	return NSIntegralRect(NSMakeRect(x0, y0, std::max((CGFloat)200, x1 - x0), std::max((CGFloat)200, y1 - y0)));
}
@end
