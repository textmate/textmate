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
		myTextStorage = @"This is a test. Try press ⌃⌘D on one of these words.";
	}
	return self;
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (NSDictionary*)stringAttributes
{
	static NSDictionary* attrs = @{
		NSForegroundColorAttributeName: [NSColor blackColor],
		NSFontAttributeName:            [NSFont userFixedPitchFontOfSize:12]
	};
	return attrs;
}

- (void)keyDown:(NSEvent*)anEvent
{
	NSLog(@"%s %@", sel_getName(_cmd), anEvent);
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
		NSLog(@"%s %@", sel_getName(_cmd), NSStringFromSelector(aSelector));
	return [super respondsToSelector:aSelector];
}
#endif

- (NSTextStorage *)textStorage
{
	return [[NSTextStorage alloc] initWithString:myTextStorage attributes:[self stringAttributes]];
}

// ===============
// = NSTextInput =
// ===============

- (void)insertText:(id)aString
{
	NSLog(@"%s %@", sel_getName(_cmd), aString);
}

- (void)doCommandBySelector:(SEL)aSelector
{
	NSLog(@"%s %@", sel_getName(_cmd), NSStringFromSelector(aSelector));
	[self tryToPerform:aSelector with:self];
}

- (void)setMarkedText:(id)aString selectedRange:(NSRange)selRange
{
	NSLog(@"%s %@ %@", sel_getName(_cmd), aString, NSStringFromRange(selRange));
}

- (void)unmarkText
{
	NSLog(@"%s", sel_getName(_cmd));
}

- (BOOL)hasMarkedText
{
	NSLog(@"%s", sel_getName(_cmd));
	return NO;
}

- (NSInteger)conversationIdentifier
{
	return (NSInteger)self;
}

- (NSAttributedString*)attributedSubstringFromRange:(NSRange)theRange
{
	NSAttributedString* res = [[NSAttributedString alloc] initWithString:[myTextStorage substringWithRange:theRange] attributes:[self stringAttributes]];
	NSLog(@"%s %@ %@", sel_getName(_cmd), NSStringFromRange(theRange), res);
	return res;
}

- (NSRange)markedRange
{
	NSLog(@"%s", sel_getName(_cmd));
	return NSMakeRange(NSNotFound, 0);
}

- (NSRange)selectedRange
{
	NSLog(@"%s", sel_getName(_cmd));
	return NSMakeRange(0, 0);
}

- (NSRect)firstRectForCharacterRange:(NSRange)theRange
{
	NSRect rect = [self convertRect:NSMakeRect(7.0f * theRange.location, 0, 7.0f * theRange.length, 20.0f) toView:nil];
	rect = [[self window] convertRectToScreen:rect];
	NSLog(@"%s %@ (%@)", sel_getName(_cmd), NSStringFromRange(theRange), NSStringFromRect(rect));
	return rect;
}

- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
	NSUInteger index = floor([self convertPoint:[[self window] convertRectFromScreen:(NSRect){ thePoint, NSZeroSize }].origin fromView:nil].x / 7.0f);
	NSLog(@"%s %@ (%lu)", sel_getName(_cmd), NSStringFromPoint(thePoint), index);
	return index;
}

- (NSArray*)validAttributesForMarkedText
{
	NSLog(@"%s", sel_getName(_cmd));
	return [[self stringAttributes] allKeys];
}
@end

#if 0
class DictionaryTests : public CxxTest::TestSuite
{
public:
	void test_dictionary ()
	{
		@autoreleasepool {
			OakSetupApplicationWithView([[MyTextView alloc] initWithFrame:NSMakeRect(0, 0, 400, 60)], "dictionary");
		}
	}
};
#endif
