#ifndef TEXT_FORMAT_H_ERKOWIHN
#define TEXT_FORMAT_H_ERKOWIHN

#include <oak/oak.h>

namespace text
{
	inline std::string format (char const* format, ...) __attribute__ ((format (printf, 1, 2)));

	inline std::string format (char const* format, ...)
	{
		char* tmp = NULL;

		va_list ap;
		va_start(ap, format);
		vasprintf(&tmp, format, ap);
		va_end(ap);

		std::string res(tmp);
		free(tmp);
		return res;
	}

	inline std::string pad (size_t number, size_t minDigits)
	{
		size_t actualDigits = 1 + size_t(floor(log10(number)));
		std::string res = "";
		for(size_t i = 0; i < std::max(minDigits, actualDigits) - actualDigits; ++i)
			res += "\u2007"; // Figure Space
		return res + std::to_string(number);
	}

	template <typename T>
	std::string join (T const& items, std::string const& sep = ", ")
	{
		std::string res = "";
		bool first = true;
		for(auto const& item : items)
		{
			if(first)
					first = false;
			else	res += sep;
			res += std::string(item);
		}
		return res;
	}

	PUBLIC std::string format_size (size_t inBytes);
}

#endif /* end of include guard: TEXT_FORMAT_H_ERKOWIHN */
