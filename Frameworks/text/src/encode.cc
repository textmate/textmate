#include "encode.h"
#include "format.h"

namespace encode
{
	std::string url_part (std::string const& src, std::string const& excl)
	{
		std::string res = "";
		for(auto const& ch : src)
		{
			if(strchr("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.!~*'()", ch) || excl.find(ch) != std::string::npos)
					res += ch;
			else	res += text::format("%%%02X", ch);
		}
		return res;
	}

} /* encode */