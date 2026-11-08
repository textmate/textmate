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
	inline NSString* inverted_color (NSString* value, NSColorSpace* space)
	{
		NSColor* color = [value isKindOfClass:NSString.class] ? parse_color(value, space) : nil;
		if(!color)
			return value;
		// HSL lightness L -> 1-L, retaining hue, saturation, and alpha.
		CGFloat shift = 1 - std::max({ color.redComponent, color.greenComponent, color.blueComponent })
		                      - std::min({ color.redComponent, color.greenComponent, color.blueComponent });
		CGFloat components[] = { color.redComponent + shift, color.greenComponent + shift, color.blueComponent + shift, color.alphaComponent };
		return color_string([NSColor colorWithColorSpace:space components:components count:4], space);
	}

	inline void invert_theme_colors (NSMutableDictionary* theme, NSString* foreground, NSString* background, NSString* caret = @"#000000")
	{
		NSColorSpace* space = color_space(theme);
		NSMutableArray* rules = theme[@"settings"];
		NSMutableDictionary* global = nil;
		BOOL hasForeground = NO, hasBackground = NO, hasCaret = NO;
		for(NSMutableDictionary* rule in rules)
		{
			if(![rule[@"scope"] length])
			{
				if(!global) global = rule[@"settings"];
				hasForeground |= [rule[@"settings"][@"foreground"] length] > 0;
				hasBackground |= [rule[@"settings"][@"background"] length] > 0;
				hasCaret |= [rule[@"settings"][@"caret"] length] > 0;
			}
		}
		if(!global)
		{
			global = [NSMutableDictionary dictionary];
			[rules insertObject:[@{ @"settings": global } mutableCopy] atIndex:0];
		}
		// Explicitly invert renderer defaults, which otherwise stay light/dark.
		if(!hasForeground) global[@"foreground"] = foreground;
		if(!hasBackground) global[@"background"] = background;
		if(!hasCaret) global[@"caret"] = caret;
		for(NSMutableDictionary* rule in rules)
		{
			NSMutableDictionary* settings = rule[@"settings"];
			for(NSString* key in @[ @"foreground", @"background", @"caret", @"selection", @"invisibles" ])
				if(settings[key]) settings[key] = inverted_color(settings[key], space);
		}
		NSMutableDictionary* gutter = theme[@"gutterSettings"];
		for(NSString* key in @[ @"divider", @"selectionBorder", @"foreground", @"background", @"icons", @"iconsHover", @"iconsPressed", @"selectionForeground", @"selectionBackground", @"selectionIcons", @"selectionIconsHover", @"selectionIconsPressed" ])
			if(gutter[key]) gutter[key] = inverted_color(gutter[key], space);
	}
}

#endif
