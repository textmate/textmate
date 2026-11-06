#ifndef THEME_EDITOR_DTCG_H
#define THEME_EDITOR_DTCG_H

#import "colors.h"
#include <stdexcept>
#include <string>

namespace theme_editor
{
	// Read only token data. Descriptions and extension metadata are not tokens.
	struct dtcg_reader
	{
		NSDictionary* root;
		NSMutableDictionary* tokens = [NSMutableDictionary dictionary];
		NSMutableDictionary* types = [NSMutableDictionary dictionary];

		void collect (NSDictionary* group, NSString* path = @"", NSString* inherited = nil, NSUInteger depth = 0)
		{
			if(depth > 128) throw std::runtime_error("The token file is nested too deeply.");
			if(group[@"$extends"]) throw std::runtime_error("DTCG group extensions are not supported. Export resolved tokens before importing.");
			id type = group[@"$type"] ?: inherited;
			if(type && ![type isKindOfClass:NSString.class]) throw std::runtime_error("A token type must be a string.");
			if(group[@"$value"])
			{
				if(tokens[path]) throw std::runtime_error("The token file contains ambiguous token paths.");
				tokens[path] = group;
				if(type) types[path] = type;
				return;
			}
			for(NSString* key in [group.allKeys sortedArrayUsingSelector:@selector(localizedStandardCompare:)])
			{
				if(([key hasPrefix:@"$"] && ![key isEqual:@"$root"]) || ![group[key] isKindOfClass:NSDictionary.class]) continue;
				collect(group[key], path.length ? [path stringByAppendingFormat:@".%@", key] : key, type, depth + 1);
			}
		}

		id resolve (id value, NSUInteger depth = 0)
		{
			if(depth > 128) throw std::runtime_error("The token file contains a circular or excessively deep reference.");
			if([value isKindOfClass:NSString.class] && [value hasPrefix:@"{"] && [value hasSuffix:@"}"])
			{
				NSString* path = [value substringWithRange:NSMakeRange(1, [value length] - 2)];
				id token = tokens[path] ?: tokens[[path stringByAppendingString:@".$root"]];
				if(!token) throw std::runtime_error("A color token refers to a token that does not exist.");
				return resolve(token[@"$value"], depth + 1);
			}
			if([value isKindOfClass:NSDictionary.class])
			{
				if(id reference = value[@"$ref"])
				{
					if(![reference isKindOfClass:NSString.class] || ![reference hasPrefix:@"#/"])
						throw std::runtime_error("Only references within the imported token file are supported.");
					id target = root;
					for(NSString* part in [[reference substringFromIndex:2] componentsSeparatedByString:@"/"])
					{
						NSString* key = [[part stringByReplacingOccurrencesOfString:@"~1" withString:@"/"] stringByReplacingOccurrencesOfString:@"~0" withString:@"~"];
						if([target isKindOfClass:NSDictionary.class]) target = target[key];
						else if([target isKindOfClass:NSArray.class] && key.length && [key rangeOfCharacterFromSet:NSCharacterSet.decimalDigitCharacterSet.invertedSet].location == NSNotFound && key.longLongValue < [target count]) target = target[key.integerValue];
						else target = nil;
						if(!target) throw std::runtime_error("A color token contains an unresolved JSON pointer.");
					}
					if([target isKindOfClass:NSDictionary.class] && target[@"$value"]) target = target[@"$value"];
					return resolve(target, depth + 1);
				}
				NSMutableDictionary* result = [NSMutableDictionary dictionary];
				for(NSString* key in value) result[key] = resolve(value[key], depth + 1);
				return result;
			}
			if([value isKindOfClass:NSArray.class])
			{
				NSMutableArray* result = [NSMutableArray array];
				for(id item in value) [result addObject:resolve(item, depth + 1)];
				return result;
			}
			return value;
		}

		NSString* type (NSString* path, NSUInteger depth = 0)
		{
			if(depth > 128) throw std::runtime_error("The token file contains a circular type reference.");
			if(!tokens[path] && tokens[[path stringByAppendingString:@".$root"]]) path = [path stringByAppendingString:@".$root"];
			if(!tokens[path]) throw std::runtime_error("A color token refers to a token that does not exist.");
			if(types[path]) return types[path];
			id value = tokens[path][@"$value"];
			if([value isKindOfClass:NSString.class] && [value hasPrefix:@"{"] && [value hasSuffix:@"}"])
				return type([value substringWithRange:NSMakeRange(1, [value length] - 2)], depth + 1);
			return nil;
		}
	};

	inline double dtcg_component (id value)
	{
		if([value isEqual:@"none"]) return 0;
		if(![value isKindOfClass:NSNumber.class] || CFGetTypeID((__bridge CFTypeRef)value) == CFBooleanGetTypeID() || !std::isfinite([value doubleValue]))
			throw std::runtime_error("A color token contains an invalid numeric component.");
		return [value doubleValue];
	}

	inline NSString* dtcg_color (id value)
	{
		if([value isKindOfClass:NSString.class])
		{
			if(NSColor* color = parse_color(value, NSColorSpace.sRGBColorSpace)) return color_string(color, NSColorSpace.sRGBColorSpace);
			throw std::runtime_error("A color token contains an invalid hex color.");
		}
		if(![value isKindOfClass:NSDictionary.class] || ![value[@"components"] isKindOfClass:NSArray.class] || [value[@"components"] count] != 3)
			throw std::runtime_error("A DTCG color must contain a colorSpace and three components.");
		id space = value[@"colorSpace"];
		if(![space isEqual:@"srgb"] && ![space isEqual:@"srgb-linear"] && ![space isEqual:@"display-p3"])
			throw std::runtime_error("This DTCG color space is not supported. Export sRGB, linear sRGB, or Display P3 colors.");
		CGFloat components[4];
		for(NSUInteger i = 0; i < 3; ++i) components[i] = dtcg_component(value[@"components"][i]);
		components[3] = value[@"alpha"] ? dtcg_component(value[@"alpha"]) : 1;
		if(components[3] < 0 || components[3] > 1) throw std::runtime_error("A color token's alpha must be between zero and one.");
		static NSColorSpace* linearSpace = [] {
			CGColorSpaceRef cgSpace = CGColorSpaceCreateWithName(kCGColorSpaceExtendedLinearSRGB);
			NSColorSpace* result = [[NSColorSpace alloc] initWithCGColorSpace:cgSpace];
			CGColorSpaceRelease(cgSpace);
			return result;
		}();
		NSColorSpace* colorSpace = [space isEqual:@"display-p3"] ? NSColorSpace.displayP3ColorSpace : [space isEqual:@"srgb-linear"] ? linearSpace : NSColorSpace.sRGBColorSpace;
		return color_string([NSColor colorWithColorSpace:colorSpace components:components count:4], NSColorSpace.sRGBColorSpace);
	}

	inline NSDictionary* read_dtcg (NSData* data, bool dark = false)
	{
		NSError* error = nil;
		id root = [NSJSONSerialization JSONObjectWithData:data options:0 error:&error];
		if(![root isKindOfClass:NSDictionary.class]) throw std::runtime_error("Choose a DTCG JSON object containing color tokens.");
		dtcg_reader reader{ root };
		reader.collect(root);
		NSMutableArray* colors = [NSMutableArray array];
		NSMutableDictionary* byPath = [NSMutableDictionary dictionary];
		for(NSString* path in [reader.tokens.allKeys sortedArrayUsingSelector:@selector(localizedStandardCompare:)])
		{
			if(![reader.type(path) isEqual:@"color"]) continue;
			NSString* hex;
			try { hex = dtcg_color(reader.resolve(reader.tokens[path][@"$value"])); }
			catch(std::exception const& error) { throw std::runtime_error(std::string("Color token ") + path.UTF8String + ": " + error.what()); }
			NSDictionary* swatch = @{ @"name": path, @"color": hex };
			[colors addObject:swatch];
			byPath[path] = swatch;
		}
		if(!colors.count) throw std::runtime_error("The DTCG file does not contain any color tokens.");
		// Recognize semantic sets in project exports without mixing light/dark roles.
		NSMutableArray* syntax = [NSMutableArray array];
		for(NSString* family in @[ @"primary", @"secondary", @"tertiary", @"info", @"neutral" ])
		{
			NSString* preferred = [NSString stringWithFormat:@"%@_tokens.json.%@.%@", dark ? @"Dark" : @"Light", family, family];
			if(byPath[preferred]) [syntax addObject:byPath[preferred]];
		}
		return @{ @"name": @"", @"colors": colors, @"syntaxColors": syntax.count == 5 ? syntax : colors };
	}
}
#endif
