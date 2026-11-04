#ifndef THEME_EDITOR_COLORS_H
#define THEME_EDITOR_COLORS_H

#import <Cocoa/Cocoa.h>
#include <algorithm>
#include <cmath>

namespace theme_editor
{
	inline NSColorSpace* color_space (NSDictionary* theme)
	{
		// Keep the same legacy default as theme_t and OakTheme.
		return [theme[@"colorSpaceName"] isEqualToString:@"sRGB"] ? NSColorSpace.sRGBColorSpace : NSColorSpace.genericRGBColorSpace;
	}

	inline NSColor* parse_color (NSString* value, NSColorSpace* space)
	{
		if(![value hasPrefix:@"#"] || (value.length != 4 && value.length != 5 && value.length != 7 && value.length != 9))
			return nil;
		NSString* digits = [value substringFromIndex:1];
		if([digits rangeOfCharacterFromSet:[NSCharacterSet characterSetWithCharactersInString:@"0123456789abcdefABCDEF"].invertedSet].location != NSNotFound)
			return nil;
		unsigned long rgba = strtoul(digits.UTF8String, nullptr, 16);
		if(digits.length == 3 || digits.length == 4)
		{
			unsigned long expanded = 0;
			for(NSUInteger index = 0; index < digits.length; ++index)
				expanded = (expanded << 8) | (((rgba >> (4 * (digits.length - index - 1))) & 15) * 17);
			rgba = expanded;
		}
		if(digits.length == 3 || digits.length == 6)
			rgba = (rgba << 8) | 255;
		CGFloat components[] = { ((rgba >> 24) & 255)/255.0, ((rgba >> 16) & 255)/255.0, ((rgba >> 8) & 255)/255.0, (rgba & 255)/255.0 };
		return [NSColor colorWithColorSpace:space components:components count:4];
	}

	inline NSString* color_string (NSColor* value, NSColorSpace* space)
	{
		NSColor* color = [value colorUsingColorSpace:space];
		if(!color)
			return nil;
		// Wide-gamut picker colors may exceed the theme's representable RGB range.
		auto byte = [](CGFloat component) { return lround(std::clamp(component, CGFloat(0), CGFloat(1)) * 255); };
		NSString* rgb = [NSString stringWithFormat:@"#%02lX%02lX%02lX", byte(color.redComponent), byte(color.greenComponent), byte(color.blueComponent)];
		long alpha = byte(color.alphaComponent);
		return alpha == 255 ? rgb : [rgb stringByAppendingFormat:@"%02lX", alpha];
	}
}

#endif
