#include "ascii.h"
#include <text/format.h>
#include <oak/debug.h>

/*
array:    '(' (element ',')* (element)? ')'
dict:     '{' (key '=' value ';')* '}'
integer:  ('-'|'+')? ('0x'|'0')? [0-9]+
float:    '-'? [0-9]* '.' [0-9]+
boolean:  :true | :false
string:   ["] … ["] | ['] … ['] | [a-zA-Z_-]+
data:     <DEADBEEF>
date:     @2010-05-10 20:34:12 +0000
*/

%%{

	machine string;

	action clear_str       { strBuf.clear(); }
	action push_char       { strBuf.push_back(fc); }
	action push_esc        { strBuf.push_back('\\'); }
	action matched         { matched = true; }

	BARE      = ([a-zA-Z_] [a-zA-Z0-9_\-.]*) >clear_str $push_char;

	S_ESCAPE  = '\'' '\'' @push_char;
	S_ANY     = [^'] $push_char;
	SINGLE    = '\'' (S_ESCAPE | S_ANY)* >clear_str '\'';

	D_ESCAPE  = '\\' ([\\"] | [^\\"] >push_esc) $push_char;
	D_ANY     = [^\\"] $push_char;
	DOUBLE    = '"' (D_ESCAPE | D_ANY)* >clear_str '"';

	string   := (BARE | SINGLE | DOUBLE) @matched;

	write data;

	machine comment;
	SINGLE   = '//' [^\n]* '\n';
	DOUBLE   = '/*' ([^*] | '*' [^/])* '*/';
	WS       = [ \t\n]+;
	comment := (SINGLE | DOUBLE | WS)+;
	write data;

}%%

static bool backtrack (char const*& p, char const* bt, plist::any_t& res)
{
	return (res = plist::any_t()), (p = bt), false;
}

static bool parse_ws (char const*& p, char const* pe)
{
	int cs;
	%% machine comment; write init; write exec;
	return true;
}

static bool parse_char (char const*& p, char const*& pe, char ch)
{
	return parse_ws(p, pe) && p != pe && *p == ch ? (++p, true) : false;
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
	if(oak::cap<quad_t>(INT32_MIN, val, INT32_MAX) == val)
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

static bool parse_string (char const*& p, char const* pe, plist::any_t& res)
{
	int cs;
	char const* bt = p;
	bool matched = false;
	std::string& strBuf = boost::get<std::string>(res = std::string());

	parse_ws(p, pe);
	%% machine string; write init; write exec;
	return matched || backtrack(p, bt, res);
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
	std::vector<plist::any_t>& ref = boost::get< std::vector<plist::any_t> >(res = std::vector<plist::any_t>());
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
	res = plist::get<std::string>(tmp);
	return !boost::get<std::string>(res).empty();
}

static bool parse_dict (char const*& p, char const* pe, plist::any_t& res)
{
	// '{' (key '=' value ';')* '}'
	char const* bt = p;
	if(!parse_char(p, pe, '{'))
		return backtrack(p, bt, res);

	plist::any_t key, value;
	std::map<std::string, plist::any_t>& ref = boost::get< std::map<std::string, plist::any_t> >(res = std::map<std::string, plist::any_t>());
	for(char const* lp = p; parse_key(lp, pe, key) && parse_char(lp, pe, '=') && parse_element(lp, pe, value) && parse_char(lp, pe, ';'); p = lp)
		ref.emplace(boost::get<std::string>(key), value);

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
