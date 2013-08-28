#include "parser_base.h"

parser_base_t::parser_base_t (std::string const& str)
{
	first = it = str.data();
	last = first + str.size();
}

bool parser_base_t::parse_char (char const* ch)
{
	return it != last && strchr(ch, *it) ? (++it, true) : false;
}

bool parser_base_t::parse_chars (char const* chars, std::string& res)
{
	res.clear();
	while(it != last && strchr(chars, *it))
		res += *it++;
	return !res.empty();
}

bool parser_base_t::parse_int (size_t& res)
{
	if(it == last || !isdigit(*it))
		return false;
	res = 0;
	for(; it != last && isdigit(*it); ++it)
		res = (res * 10) + (*it - '0');
	return true;
}

bool parser_base_t::parse_until (char const* stopChars, std::string& res)
{
	res.clear();
	char const* backtrack = it;
	for(; it != last && !strchr(stopChars, *it); ++it)
	{
		if(*it == '\\' && it+1 != last && (it[1] == '\\' || strchr(stopChars, it[1])))
			++it;
		res += *it;
	}
	return parse_char(stopChars) ? true : (it = backtrack, false);
}
