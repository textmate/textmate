#import <OakAppKit/OakPopOutAnimation.h>
#import <test/cocoa.h>
#import <oak/oak.h>

@interface MyView : NSView
{
}
@end

@implementation MyView
- (BOOL)acceptsFirstResponder { return YES; }

- (void)drawRect:(NSRect)aRect
{
	NSEraseRect(aRect);
}

- (void)mouseDown:(NSEvent*)anEvent
{
	NSRect p = [[self window] convertRectToScreen:(NSRect){ [anEvent locationInWindow], NSMakeSize(48, 48) }];
	OakShowPopOutAnimation(nil, p, [NSImage imageNamed:NSImageNameComputer]);
}
@end

class PopOutTests : public CxxTest::TestSuite
{
public:
	void test_layout ()
	{
		OakSetupApplicationWithView([[MyView alloc] initWithFrame:NSMakeRect(0, 0, 200, 50)], "pop_out");
	}
};
