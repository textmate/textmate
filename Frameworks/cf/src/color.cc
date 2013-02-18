#include "color.h"
#include <oak/debug.h>
#include <text/format.h>

namespace cf
{
	color_t::color_t (std::string const& str)
	{
		unsigned int r = 0, g = 0, b = 0, a = 0xFF;
		if(str == NULL_STR || sscanf(str.c_str(), "#%02x%02x%02x%02x", &r, &g, &b, &a) < 3)
			*this = color_t(0, 0, 0, 1);
		
		*this = color_t(r/255.0, g/255.0, b/255.0, a/255.0);
	}

	color_t::operator CGColorRef () const
	{
		if(!cachedValue)
		{
			CGFloat components[4] = { _red, _green, _blue, _alpha };
			CGColorSpaceRef colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
			cachedValue.reset(CGColorCreate(colorspace, components), CGColorRelease);
			CGColorSpaceRelease(colorspace);
		}
		return cachedValue.get();
	}
	
	bool color_t::operator== (color_t const& rhs) const { return _red == rhs._red && _blue == rhs._blue && _green == rhs._green && _alpha == rhs._alpha; }
	bool color_t::operator!= (color_t const& rhs) const { return _red != rhs._red || _blue != rhs._blue || _green != rhs._green || _alpha != rhs._alpha; }

	std::string to_s (color_t const& c)
	{
		return text::format("#%02lX%02lX%02lX%02lX", lround(c._red*0xFF), lround(c._green*0xFF), lround(c._blue*0xFF), lround(c._alpha*0xFF));
	}

	std::string to_s (CGColorRef color)
	{
		size_t componentsCount = CGColorGetNumberOfComponents(color);
		if(componentsCount == 4)
		{
			CGFloat const* rgba = CGColorGetComponents(color);
			return text::format("#%02lX%02lX%02lX%02lX", lround(0xFF*rgba[0]), lround(0xFF*rgba[1]), lround(0xFF*rgba[2]), lround(0xFF*rgba[3]));
		}
		return NULL_STR;
	}

	bool color_is_dark (color_t const& color)
	{
		return 0.30*color._red + 0.59*color._green + 0.11*color._blue < 0.5;
	}

	bool color_is_dark (CGColorRef const color)
	{
		size_t componentsCount = CGColorGetNumberOfComponents(color);
		if(componentsCount == 4)
		{
			CGFloat const* components = CGColorGetComponents(color);

			CGFloat const& red   = components[0];
			CGFloat const& green = components[1];
			CGFloat const& blue  = components[2];

			return 0.30*red + 0.59*green + 0.11*blue < 0.5;
		}
		return false;
	}

} /* cf */
