#include "trim.h"

namespace text
{
	std::string trim (std::string const& str, std::string const& trimChars)
	{
		std::string::size_type first = str.find_first_not_of(trimChars);
		std::string::size_type last  = str.find_last_not_of(trimChars);
		if(first == std::string::npos)
			first = str.size();
		if(last == std::string::npos)
				last = str.size();
		else	++last;
		return first <= last ? str.substr(first, last - first) : "";
	}

} /* text */
