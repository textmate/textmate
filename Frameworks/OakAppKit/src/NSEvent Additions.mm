#import "NSEvent Additions.h"
#import <oak/oak.h>

#if !defined(MAC_OS_X_VERSION_10_6) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_6)
@interface NSEvent (SnowLeopard)
+ (NSTimeInterval)doubleClickInterval;
+ (NSUInteger)pressedMouseButtons;
+ (NSUInteger)modifierFlags;
@end
#endif

static struct conversion_t { UInt32 oldValue; NSUInteger newValue; } const ConversionMap[] =
{
	{ shiftKey,    NSShiftKeyMask      },
	{ controlKey,  NSControlKeyMask    },
	{ optionKey,   NSAlternateKeyMask  },
	{ cmdKey,      NSCommandKeyMask    }
};

@implementation NSEvent (SnowLeopardCompatibilityWrappers)
+ (NSTimeInterval)slDoubleClickInterval
{
	if(![self respondsToSelector:@selector(doubleClickInterval)])
		return GetDblTime() / 60.0;
	return [self doubleClickInterval];
}

+ (NSTimeInterval)caretBlinkInterval
{
	return GetCaretTime() / 60.0; // There is no modern replacement for this one <rdar://7260524>
}

+ (NSUInteger)slPressedMouseButtons
{
	if(![self respondsToSelector:@selector(pressedMouseButtons)])
		return GetCurrentEventButtonState();
	return [self pressedMouseButtons];
}

+ (NSUInteger)slModifierFlags
{
	if(![self respondsToSelector:@selector(modifierFlags)])
	{
		NSUInteger res = 0;
		UInt32 legacy = GetCurrentEventKeyModifiers();
		iterate(modifier, ConversionMap)
		{
			if(legacy & modifier->oldValue)
				res |= modifier->newValue;
		}
		return res;
	}
	return [self modifierFlags];
}
@end
