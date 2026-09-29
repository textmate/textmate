#include "render.h"
#include <oak/oak.h>
#include <cf/src/cf.h>

namespace render
{
	void fill_rect (CGContextRef context, CGColorRef color, CGRect const& rect)
	{
		ASSERT(color);
		CGContextSetFillColorWithColor(context, color);
		CGContextFillRect(context, rect);
	}

} /* render */
