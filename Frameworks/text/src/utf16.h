#ifndef UTF16_H_VKRVH0HR
#define UTF16_H_VKRVH0HR

#include "utf8.h"
#include <oak/oak.h>

namespace utf16
{
	template <typename _Iter>
	_Iter advance (_Iter const& first, size_t distance)
	{
		utf8::iterator_t<char const*> it(first);
		for(; distance; ++it)
			distance -= (*it > 0xFFFF) ? 2 : 1;
		return &it;
	}

	template <typename _Iter>
	_Iter advance (_Iter const& first, size_t distance, _Iter const& last)
	{
		utf8::iterator_t<char const*> it(first);
		for(; distance && &it != last; ++it)
			distance -= (*it > 0xFFFF) ? 2 : 1;
		return &it;
	}

	template <typename _Iter>
	size_t distance (_Iter const& first, _Iter const& last)
	{
		size_t res = 0;
		foreach(it, utf8::make(first), utf8::make(last))
			res += (*it > 0xFFFF) ? 2 : 1;
		return res;
	}

} /* utf16 */ 

#endif /* end of include guard: UTF16_H_VKRVH0HR */
