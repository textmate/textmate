#ifndef TEXT_NEWLINES_H_CYET9RUW
#define TEXT_NEWLINES_H_CYET9RUW

extern std::string const kLF;
extern std::string const kCR;
extern std::string const kCRLF;

namespace text
{
	// =====================
	// = Line Feed Support =
	// =====================

	template <typename _InputIter>
	std::string estimate_line_endings (_InputIter const& first, _InputIter const& last, std::string const& fallback = kLF)
	{
		size_t const kEnoughSamples = 50;

		size_t lf_count = 0, cr_count = 0, crlf_count = 0;
		bool prevWasCR = false;

		for(auto it = first; it != last; ++it)
		{
			if(*it == '\n')
			{
				if(prevWasCR)
				{
					if(++crlf_count == kEnoughSamples)
						break;
					prevWasCR = false;
				}
				else
				{
					if(++lf_count == kEnoughSamples)
						break;
				}
			}
			else
			{
				if(prevWasCR && (++cr_count == kEnoughSamples))
					break;
				prevWasCR = *it == '\r';
			}
		}

		if(lf_count == 0 && cr_count == 0 && crlf_count > 0)
			return kCRLF;
		else if(lf_count == 0 && crlf_count == 0 && cr_count > 0)
			return kCR;
		else if(lf_count != 0)
			return kLF;
		return fallback;
	}

	template <typename _InputIter>
	_InputIter convert_line_endings (_InputIter first, _InputIter last, std::string const& lineFeeds)
	{
		_InputIter out = first;
		while(first != last)
		{
			auto it = std::search(first, last, lineFeeds.begin(), lineFeeds.end());
			out = std::copy(first, it, out);
			first = it;
			if(first != last)
			{
				*out++ = '\n';
				std::advance(first, lineFeeds.size());
			}
		}
		return out;
	}

} /* text */

#endif /* end of include guard: TEXT_NEWLINES_H_CYET9RUW */
