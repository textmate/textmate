#ifndef UTF16_H_VKRVH0HR
#define UTF16_H_VKRVH0HR

#include "utf8.h"
#include <oak/oak.h>
#ifndef NDEBUG
#include "hexdump.h"
#endif

namespace utf16
{
	template <typename _Iter>
	_Iter advance (_Iter const& first, size_t distance)
	{
		utf8::iterator_t<_Iter> it(first);
		for(; distance; ++it)
			distance -= (*it > 0xFFFF) ? 2 : 1;
		return &it;
	}

	template <typename _Iter>
	_Iter advance (_Iter const& first, size_t distance, _Iter const& last)
	{
		ASSERT(last == utf8::find_safe_end(first, last));
		utf8::iterator_t<_Iter> it(first);
		for(; distance && &it != last; ++it)
			distance -= (*it > 0xFFFF) ? 2 : 1;
		return &it;
	}

	template <typename _Iter>
	size_t distance (_Iter const& first, _Iter const& last)
	{
		auto const safeLast = utf8::find_safe_end(first, last);
#ifndef NDEBUG
		if(last != safeLast)
		{
			auto const byteLength = std::distance(first, last);
			auto const safeLength = std::distance(first, safeLast);
			auto context = first;
			std::advance(context, std::max<decltype(byteLength)>(0, byteLength - 32));
			auto const contextOffset = std::distance(first, context);
			std::string const dump = text::to_hex(context, last);
			ASSERTF(last == safeLast,
			    "UTF-8 range ends inside a multibyte character.\n"
			    "Requested end: %td bytes after first.\n"
			    "Last safe end: %td bytes after first (%td incomplete byte%s).\n"
			    "Trailing context from byte %td (up to 32 bytes):\n%s",
			    byteLength, safeLength, byteLength - safeLength,
			    byteLength - safeLength == 1 ? "" : "s", contextOffset, dump.c_str());
		}
#endif
		size_t res = 0;
		foreach(it, utf8::make(first), utf8::make(safeLast))
			res += (*it > 0xFFFF) ? 2 : 1;
		return res;
	}

} /* utf16 */

#endif /* end of include guard: UTF16_H_VKRVH0HR */
