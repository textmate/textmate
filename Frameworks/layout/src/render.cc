#include "render.h"
#include <oak/oak.h>
#include <cf/cf.h>

namespace render
{
	void fill_rect (CGContextRef context, CGColorRef color, CGRect const& rect)
	{
		ASSERT(color);
		CGContextSetFillColorWithColor(context, color);
		CGContextFillRect(context, rect);
	}

	void draw_spelling_dot (CGContextRef context, CGRect const& rect)
	{
		static cf::image_t image("SpellingDot.tiff", "com.macromates.TextMate.OakAppKit");
		if(!image)
			return;

		for(CGFloat x = rect.origin.x; x < rect.origin.x + rect.size.width - 0.5; x += 4)
			CGContextDrawImage(context, CGRectMake(x, rect.origin.y, 4, 3), image);
	}

} /* render */
