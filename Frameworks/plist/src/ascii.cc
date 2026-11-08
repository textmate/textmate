#include "ascii.h"
#include <text/format.h>
#include <oak/debug.h>

static bool backtrack (char const*& p, char const* bt, plist::any_t& res)
{
	return (res = plist::any_t()), (p = bt), false;
}

static bool parse_ws (char const*& p, char const* pe)
{
	while(p < pe)
	{
		if(*p == ' ' || *p == '\t' || *p == '\n')
		{
			++p;
		}
		else if(pe - p >= 2 && p[0] == '/' && p[1] == '/')
		{
			p += 2;
			while(p < pe && *p != '\n')
				++p;
			if(p < pe)
				++p;
		}
		else if(pe - p >= 2 && p[0] == '/' && p[1] == '*')
		{
			p += 2;
			while(pe - p >= 2 && !(p[0] == '*' && p[1] == '/'))
				++p;
			if(pe - p >= 2)
				p += 2;
		}
		else
		{
			break;
		}
	}
	return true;
}

static bool parse_char (char const*& p, char const*& pe, char ch)
{
	return parse_ws(p, pe) && p != pe && *p == ch ? (++p, true) : false;
}

static bool parse_string (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	parse_ws(p, pe);
	if(p >= pe)
		return backtrack(p, bt, res);

	std::string strBuf;

	if(*p == '"')
	{
		++p;
		while(p < pe && *p != '"')
		{
			if(*p == '\\')
			{
				++p;
				if(p < pe)
				{
					if(*p == '\\' || *p == '"')
						strBuf.push_back(*p++);
					else
					{
						strBuf.push_back('\\');
						strBuf.push_back(*p++);
					}
				}
			}
			else
			{
				strBuf.push_back(*p++);
			}
		}
		if(p < pe && *p == '"')
		{
			++p;
			res = strBuf;
			return true;
		}
		return backtrack(p, bt, res);
	}
	else if(*p == '\'')
	{
		++p;
		while(p < pe)
		{
			if(*p == '\'')
			{
				if(pe - p >= 2 && p[1] == '\'')
				{
					strBuf.push_back('\'');
					p += 2;
				}
				else
				{
					++p;
					res = strBuf;
					return true;
				}
			}
			else
			{
				strBuf.push_back(*p++);
			}
		}
		return backtrack(p, bt, res);
	}
	else if(isalpha(*p) || *p == '_')
	{
		strBuf.push_back(*p++);
		while(p < pe && (isalnum(*p) || *p == '_' || *p == '-' || *p == '.'))
			strBuf.push_back(*p++);
		res = strBuf;
		return true;
	}

	return backtrack(p, bt, res);
}

static bool parse_int (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	parse_ws(p, pe);
	if(p == pe || (!isdigit(*p) && *p != '-' && *p != '+'))
		return backtrack(p, bt, res);

	char* dummy;
	quad_t val = strtoq(p, &dummy, 0);
	p = dummy;
	if(std::clamp<quad_t>(val, INT32_MIN, INT32_MAX) == val)
			res = int32_t(val);
	else	res = uint64_t(val);

	return true;
}

static bool parse_bool (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	parse_ws(p, pe);
	size_t bytes = pe - p;
	if(bytes >= 5 && strncmp(p, ":true", 5) == 0)
		return (res = true), (p += 5), true;
	if(bytes >= 6 && strncmp(p, ":false", 6) == 0)
		return (res = false), (p += 6), true;
	return backtrack(p, bt, res);
}

static bool parse_date (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	if(!parse_char(p, pe, '@'))
		return backtrack(p, bt, res);

	size_t bytes = pe - p;
	if(bytes >= 25)
	{
		oak::date_t date(std::string(p, p + 25));
		if(date)
		{
			res = date;
			p += 25;
			return true;
		}
	}
	return backtrack(p, bt, res);
}

static bool parse_element (char const*& p, char const* pe, plist::any_t& res);

static bool parse_array (char const*& p, char const* pe, plist::any_t& res)
{
	// '(' (element ',')* (element)? ')'
	char const* bt = p;
	if(!parse_char(p, pe, '('))
		return backtrack(p, bt, res);

	plist::any_t element;
	std::vector<plist::any_t>& ref = plist::get< std::vector<plist::any_t> >(res = std::vector<plist::any_t>());
	while(parse_element(p, pe, element))
	{
		ref.push_back(element);
		if(!parse_char(p, pe, ','))
			break;
	}
	return parse_char(p, pe, ')') || backtrack(p, bt, res);
}

static bool parse_key (char const*& p, char const* pe, plist::any_t& res)
{
	plist::any_t tmp;
	if(!parse_element(p, pe, tmp))
		return false;
	res = plist::convert<std::string>(tmp);
	return !plist::get<std::string>(res).empty();
}

static bool parse_dict (char const*& p, char const* pe, plist::any_t& res)
{
	// '{' (key '=' value ';')* '}'
	char const* bt = p;
	if(!parse_char(p, pe, '{'))
		return backtrack(p, bt, res);

	plist::any_t key, value;
	std::map<std::string, plist::any_t>& ref = plist::get< std::map<std::string, plist::any_t> >(res = std::map<std::string, plist::any_t>());
	for(char const* lp = p; parse_key(lp, pe, key) && parse_char(lp, pe, '=') && parse_element(lp, pe, value) && parse_char(lp, pe, ';'); p = lp)
		ref.emplace(plist::get<std::string>(key), value);

	return parse_char(p, pe, '}') || backtrack(p, bt, res);
}

static bool parse_element (char const*& p, char const* pe, plist::any_t& res)
{
	return parse_string(p, pe, res) || parse_int(p, pe, res) || parse_bool(p, pe, res) || parse_date(p, pe, res) || parse_dict(p, pe, res) || parse_array(p, pe, res);
}

namespace plist
{
	plist::any_t parse_ascii (std::string const& str, bool* success)
	{
		plist::any_t res;
		char const* p  = str.data();
		char const* pe = p + str.size();
		bool didParse = parse_element(p, pe, res) && parse_ws(p, pe) && p == pe;
		if(success)
			*success = didParse;
		return didParse ? res : plist::any_t();
	}

} /* plist */
