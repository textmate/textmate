#ifndef CGCOLORREF_TO_STRING_H_5BXZF34O
#define CGCOLORREF_TO_STRING_H_5BXZF34O

#include <text/src/format.h>

inline std::string to_s (CGColorRef color)
{
	size_t componentsCount = CGColorGetNumberOfComponents(color);
	if(componentsCount == 4)
	{
		CGFloat const* rgba = CGColorGetComponents(color);
		return text::format("#%02lX%02lX%02lX%02lX", lround(0xFF*rgba[0]), lround(0xFF*rgba[1]), lround(0xFF*rgba[2]), lround(0xFF*rgba[3]));
	}
	return NULL_STR;
}

#endif /* end of include guard: CGCOLORREF_TO_STRING_H_5BXZF34O */
