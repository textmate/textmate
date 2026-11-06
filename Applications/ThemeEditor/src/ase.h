#ifndef THEME_EDITOR_ASE_H
#define THEME_EDITOR_ASE_H

#import "colors.h"
#include <cstring>
#include <stdexcept>

namespace theme_editor
{
	// Every read is bounded by its block, including UTF-16 strings and floats.
	struct ase_reader
	{
		uint8_t const* bytes;
		size_t size, offset = 0;
		void require (size_t count) const { if(count > size - offset) throw std::runtime_error("The swatch file is incomplete or contains an invalid block length."); }
		uint32_t integer (size_t count)
		{
			require(count);
			uint32_t value = 0;
			while(count--) value = (value << 8) | bytes[offset++];
			return value;
		}
		float component (float minimum = 0, float maximum = 1)
		{
			uint32_t bits = integer(4);
			float value;
			memcpy(&value, &bits, sizeof(value));
			if(!std::isfinite(value) || value < minimum || value > maximum)
				throw std::runtime_error("The swatch file contains an invalid color component.");
			return value;
		}
		NSString* name ()
		{
			size_t count = integer(2);
			if(!count) throw std::runtime_error("The swatch file contains an invalid color name.");
			require(count * 2);
			NSString* result = [[NSString alloc] initWithBytes:bytes + offset length:(count - 1) * 2 encoding:NSUTF16BigEndianStringEncoding];
			offset += (count - 1) * 2;
			if(integer(2) || !result) throw std::runtime_error("The swatch file contains an invalid color name.");
			return result;
		}
	};

	inline NSDictionary* read_ase (NSData* data)
	{
		ase_reader reader{ static_cast<uint8_t const*>(data.bytes), data.length };
		if(reader.integer(4) != 0x41534546) throw std::runtime_error("This is not an Adobe Swatch Exchange (ASE) file.");
		if(reader.integer(2) != 1 || reader.integer(2) != 0) throw std::runtime_error("This ASE file version is not supported.");
		uint32_t count = reader.integer(4);
		NSMutableArray* colors = [NSMutableArray array];
		NSString* title = @"";
		size_t groups = 0;
		for(uint32_t index = 0; index < count; ++index)
		{
			uint32_t type = reader.integer(2), length = reader.integer(4);
			reader.require(length);
			ase_reader block{ reader.bytes + reader.offset, length };
			reader.offset += length;
			if(type == 0xC001)
			{
				NSString* group = block.name();
				if(!title.length) title = group;
				++groups;
			}
			else if(type == 0xC002)
			{
				if(!groups) throw std::runtime_error("The swatch file contains an unmatched group ending.");
				--groups;
			}
			else if(type == 1)
			{
				NSString* name = block.name();
				uint32_t model = block.integer(4);
				NSColor* color = nil;
				if(model == 0x52474220) // RGB
				{
					CGFloat r = block.component(), g = block.component(), b = block.component();
					color = [NSColor colorWithSRGBRed:r green:g blue:b alpha:1];
				}
				else if(model == 0x434D594B) // CMYK (ASE has no embedded profile)
				{
					CGFloat c = block.component(), m = block.component(), y = block.component(), k = block.component();
					color = [NSColor colorWithDeviceCyan:c magenta:m yellow:y black:k alpha:1];
				}
				else if(model == 0x47726179) // Gray
				{
					CGFloat gray = block.component();
					color = [NSColor colorWithSRGBRed:gray green:gray blue:gray alpha:1];
				}
				else if(model == 0x4C414220) // Lab, D50 white point; L is stored in 0…1.
				{
					CGFloat components[] = { block.component() * 100, block.component(-128, 127), block.component(-128, 127), 1 };
					CGFloat white[] = { 0.96422, 1, 0.82521 };
					CGColorSpaceRef space = CGColorSpaceCreateLab(white, nullptr, nullptr);
					CGColorRef lab = CGColorCreate(space, components);
					color = [NSColor colorWithCGColor:lab];
					CGColorRelease(lab);
					CGColorSpaceRelease(space);
				}
				else throw std::runtime_error("The swatch file uses an unsupported color model. Use RGB, CMYK, Lab, or Gray swatches.");
				if(block.integer(2) > 2) throw std::runtime_error("The swatch file contains an invalid color type.");
				NSString* hex = color_string(color, NSColorSpace.sRGBColorSpace);
				if(!hex) throw std::runtime_error("A swatch could not be converted to sRGB.");
				[colors addObject:@{ @"name": name.length ? name : [NSString stringWithFormat:@"Color %lu", colors.count + 1], @"color": hex }];
			}
			else continue; // Extension blocks can be skipped using their declared length.
			if(block.offset != block.size) throw std::runtime_error("The swatch file contains an invalid block length.");
		}
		if(groups || reader.offset != reader.size) throw std::runtime_error("The swatch file contains incomplete groups or unexpected trailing data.");
		if(!colors.count) throw std::runtime_error("The ASE file does not contain any color swatches.");
		return @{ @"name": title, @"colors": colors };
	}

	inline NSDictionary* theme_from_palette (NSDictionary* palette, NSString* fallbackName, bool dark = false)
	{
		NSArray* colors = palette[@"colors"];
		NSArray* syntaxColors = palette[@"syntaxColors"] ?: colors;
		NSString* background = dark ? @"#000000" : @"#FFFFFF";
		NSString* foreground = dark ? @"#FFFFFF" : @"#000000";
		NSMutableArray* rules = [NSMutableArray arrayWithObject:@{ @"settings": @{ @"background": background, @"foreground": foreground, @"caret": foreground, @"selection": [[syntaxColors[0][@"color"] substringToIndex:7] stringByAppendingString:@"60"] } }];
		NSArray* scopes = @[ @"string", @"keyword, storage", @"constant", @"entity.name, support", @"comment" ];
		for(NSUInteger index = 0; index < scopes.count; ++index)
		{
			NSDictionary* swatch = syntaxColors[index % syntaxColors.count];
			[rules addObject:@{ @"name": swatch[@"name"], @"scope": scopes[index], @"settings": @{ @"foreground": swatch[@"color"] } }];
		}
		return @{ @"name": [palette[@"name"] length] ? palette[@"name"] : fallbackName, @"colorSpaceName": @"sRGB", @"semanticClass": dark ? @"theme.dark.custom" : @"theme.light.custom", @"settings": rules, @"themeEditorPalette": colors };
	}
}
#endif
