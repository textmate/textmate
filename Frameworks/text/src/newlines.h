#ifndef TEXT_NEWLINES_H_CYET9RUW
#define TEXT_NEWLINES_H_CYET9RUW

#include <oak/misc.h>

PUBLIC extern std::string const kLF;
PUBLIC extern std::string const kCR;
PUBLIC extern std::string const kCRLF;
PUBLIC extern std::string const kMIX;

namespace text
{
	// =====================
	// = Line Feed Support =
	// =====================

	template <typename _InputIter>
	std::string estimate_line_endings (_InputIter const& first, _InputIter const& last)
	{
		size_t cr_count = std::count(first, last, '\r');
		size_t lf_count = std::count(first, last, '\n');

		if(cr_count == 0)
			return kLF;
		else if(lf_count == 0)
			return kCR;
		else if(lf_count == cr_count)
			return kCRLF;
		else
			return kLF;
	}

	template <typename _InputIter>
	_InputIter convert_line_endings (_InputIter first, _InputIter last, std::string const& lineFeeds)
	{
		_InputIter out = first;
		while(first != last)
		{
			bool isCR = *first == '\r';
			if(out != first || isCR)
				*out = isCR ? '\n' : *first;
			if(++first != last && isCR && *first == '\n')
				++first;
			++out;
		}
		return out;
	}

} /* text */

#endif /* end of include guard: TEXT_NEWLINES_H_CYET9RUW */
