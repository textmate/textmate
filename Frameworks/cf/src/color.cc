#include "color.h"
#include <oak/debug.h>

namespace cf
{
	color_t::color_t (std::string const& str) : string_value(str)
	{
		if(str != NULL_STR)
			value = parse(str) ?: parse("#FFFFFF");
	}

	CGFloat color_t::alpha () const
	{
		return CGColorGetAlpha(value.get());
	}

	void color_t::set_alpha (CGFloat newAlpha)
	{
		ASSERT(value.get());
		value.reset(CGColorCreateCopyWithAlpha(value.get(), newAlpha), CFRelease);
	}

	color_t::CGColorPtr color_t::parse (std::string const& color)
	{
		CGColorPtr res;
		unsigned int red = 0, green = 0, blue = 0, alpha = 0xFF;
		if(sscanf(color.c_str(), "#%02x%02x%02x%02x", &red, &green, &blue, &alpha) < 3)
			return res;

		CGFloat components[4] = { red/255.0f, green/255.0f, blue/255.0f, alpha/255.0f };
		CGColorSpaceRef colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
		res.reset(CGColorCreate(colorspace, components), CGColorRelease);
		CGColorSpaceRelease(colorspace);
		return res;
	}

	std::string to_s (color_t const& c)
	{
		return c.string_value == NULL_STR ? "(undefined)" : c.string_value;
	}

	bool color_is_dark (CGColorRef color)
	{
		size_t componentsCount = CGColorGetNumberOfComponents(color);
		CGFloat intensity = 1.0;
		if(componentsCount == 4)
		{
			CGFloat const* components = CGColorGetComponents(color);

			CGFloat const& red   = components[0];
			CGFloat const& green = components[1];
			CGFloat const& blue  = components[2];

			intensity = 0.30*SQ(red) + 0.59*SQ(green) + 0.11*SQ(blue);
		}
		return intensity < 0.5;
	}

} /* cf */
