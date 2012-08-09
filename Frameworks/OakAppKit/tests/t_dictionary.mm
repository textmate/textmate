#import <test/cocoa.h>

@interface MyTextView : NSView <NSTextInput>
{
	NSString* myTextStorage;
}
@end

@implementation MyTextView
- (id)initWithFrame:(NSRect)aRect
{
	if((self = [super initWithFrame:aRect]))
	{
		myTextStorage = [@"This is a test. Try press ⌃⌘D on one of these words." retain];
	}
	return self;
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (NSDictionary*)stringAttributes
{
	static NSDictionary* attrs = [[NSDictionary dictionaryWithObjectsAndKeys:
		[NSColor blackColor],                 NSForegroundColorAttributeName,
		[NSFont userFixedPitchFontOfSize:12], NSFontAttributeName,
		nil] retain];
	return attrs;
}

- (void)keyDown:(NSEvent*)anEvent
{
	NSLog(@"%s %@", SELNAME(_cmd), anEvent);
	[self interpretKeyEvents:@[ anEvent ]];
}

- (void)drawRect:(NSRect)aRect
{
	NSEraseRect(aRect);
	[myTextStorage drawAtPoint:NSZeroPoint withAttributes:[self stringAttributes]];
}

#if 0
- (BOOL)respondsToSelector:(SEL)aSelector
{
	SEL ignore[] = { @selector(isEditable), @selector(inputContext), @selector(_acceptsMarkedText) };
	if(std::find(ignore, ignore + 3, aSelector) == ignore + 3)
		NSLog(@"%s %@", SELNAME(_cmd), NSStringFromSelector(aSelector));
	return [super respondsToSelector:aSelector];
}
#endif

- (NSTextStorage *)textStorage
{
	return [[[NSTextStorage alloc] initWithString:myTextStorage attributes:[self stringAttributes]] autorelease];
}

// ===============
// = NSTextInput =
// ===============

- (void)insertText:(id)aString
{
	NSLog(@"%s %@", SELNAME(_cmd), aString);
}

- (void)doCommandBySelector:(SEL)aSelector
{
	NSLog(@"%s %@", SELNAME(_cmd), NSStringFromSelector(aSelector));
	[self tryToPerform:aSelector with:self];
}

- (void)setMarkedText:(id)aString selectedRange:(NSRange)selRange
{
	NSLog(@"%s %@ %@", SELNAME(_cmd), aString, NSStringFromRange(selRange));
}

- (void)unmarkText
{
	NSLog(@"%s", SELNAME(_cmd));
}

- (BOOL)hasMarkedText
{
	NSLog(@"%s", SELNAME(_cmd));
	return NO;
}

- (NSInteger)conversationIdentifier
{
	return (NSInteger)self;
}

- (NSAttributedString*)attributedSubstringFromRange:(NSRange)theRange
{
	NSAttributedString* res = [[[NSAttributedString alloc] initWithString:[myTextStorage substringWithRange:theRange] attributes:[self stringAttributes]] autorelease];
	NSLog(@"%s %@ %@", SELNAME(_cmd), NSStringFromRange(theRange), res);
	return res;
}

- (NSRange)markedRange
{
	NSLog(@"%s", SELNAME(_cmd));
	return NSMakeRange(NSNotFound, 0);
}

- (NSRange)selectedRange
{
	NSLog(@"%s", SELNAME(_cmd));
	return NSMakeRange(0, 0);
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange
{
	NSRect rect = [self convertRect:NSMakeRect(7.0f * theRange.location, 0, 7.0f * theRange.length, 20.0f) toView:nil];
	rect.origin = [[self window] convertBaseToScreen:rect.origin];
	NSLog(@"%s %@ (%@)", SELNAME(_cmd), NSStringFromRange(theRange), NSStringFromRect(rect));
	return rect;
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
	NSUInteger index = floorf([self convertPoint:[[self window] convertScreenToBase:thePoint] fromView:nil].x / 7.0f);
	NSLog(@"%s %@ (%lu)", SELNAME(_cmd), NSStringFromPoint(thePoint), index);
	return index;
}

- (NSArray*)validAttributesForMarkedText
{
	NSLog(@"%s", SELNAME(_cmd));
	return [[self stringAttributes] allKeys];
}
@end

class DictionaryTests : public CxxTest::TestSuite
{
public:
	void test_dictionary ()
	{
		NSAutoreleasePool* pool = [NSAutoreleasePool new];
		OakSetupApplicationWithView([[[MyTextView alloc] initWithFrame:NSMakeRect(0, 0, 400, 60)] autorelease], "dictionary");
		[pool drain];
	}
};
