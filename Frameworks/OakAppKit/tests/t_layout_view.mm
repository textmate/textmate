#import <OakAppKit/OakLayoutView.h>

@interface MyLayoutView : OakLayoutView
@end

@implementation MyLayoutView
- (NSTextField*)createView:(NSString*)label
{
	NSTextField* res = [[[NSTextField alloc] initWithFrame:NSMakeRect(0, 0, 200, 200)] autorelease];
	[res setEditable:NO];
	[res setSelectable:NO];
	// [res setBezeled:NO];
	// [res setBordered:NO];
	// [res setDrawsBackground:NO];
	// [res setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
	[res setStringValue:label];
	return res;
}

- (NSRectEdge)edgeOfView:(NSView*)aView containingPoint:(NSPoint)aPoint
{
	NSSize size     = [aView frame].size;
	NSRectEdge edge = NSMinYEdge;

	if(aPoint.y < size.height/4)
		edge = NSMinYEdge;
	else if(aPoint.y > 3*size.height/4)
		edge = NSMaxYEdge;
	else if(aPoint.x < size.width/4)
		edge = NSMinXEdge;
	else if(aPoint.x > 3*size.width/4)
		edge = NSMaxXEdge;

	return edge;
}

- (void)mouseDown:(NSEvent*)anEvent
{
	static NSInteger i = 0;
	NSPoint click = [anEvent locationInWindow];

	NSUInteger flags = [anEvent modifierFlags];
	if(flags & NSShiftKeyMask)
	{
		NSView* newView = [self createView:[NSString stringWithFormat:@"View %ld", ++i]];
		NSRectEdge edge = [self edgeOfView:self containingPoint:[self convertPoint:click fromView:nil]];
		[self addView:newView atEdge:edge ofView:nil];
		return;
	}

	NSView* clickedView = [self hitTest:[[self superview] convertPoint:click fromView:nil]];
	if(clickedView)
	{
		NSView* newView = [self createView:[NSString stringWithFormat:@"View %ld", ++i]];
		NSRectEdge edge = [self edgeOfView:clickedView containingPoint:[clickedView convertPoint:click fromView:nil]];
		[self addView:newView atEdge:edge ofView:clickedView];
	}
}
@end

class LayoutViewTests : public CxxTest::TestSuite
{
public:
	void test_layout_view ()
	{
		NSAutoreleasePool* pool = [NSAutoreleasePool new];
		MyLayoutView* layoutView = [[[MyLayoutView alloc] initWithFrame:NSMakeRect(0, 0, 400, 60)] autorelease];
		[layoutView addView:[layoutView createView:@"Root"]];
		OakSetupApplicationWithView(layoutView, "layout_view");
		[pool drain];
	}
};
