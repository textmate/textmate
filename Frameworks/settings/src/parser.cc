// file:          ( «line» )*
// line:          ( «comment» | ( «section» | «assignment» )? ( «comment» )? ) ( '\n' | EOF )
// section:       '[' «name» ( ";" «name» )* ']'
// name:          ( /[^\] \t\n]/ | /\\[\] \t\n\\]/ )+
// assignment:    «key» '=' «value»
// key:           ( /[^= \t\n]/ | /\\[= \t\n\\]/ )+
// value:         ( «single_string» | «double_string» | «bare_string» )
// single_string: "'" ( /[^']/ | /\\['\\]/ )* "'"
// double_string: '"' ( /[^"]/ | /\\["\\]/ )* '"'
// bare_string:   ( /[^ \t\n]/ | /\\[ \t\n\\]/ )+
// comment:       '#' ( /[^\n]/ )*

#include "parser.h"

static bool backtrack (char const*& p, char const* bt)
{
	return (p = bt), false;
}

static bool parse_ws (char const*& p, char const* pe)
{
	while(p != pe && strchr(" \t", *p))
		++p;
	return true;
}

static bool parse_char (char const*& p, char const*& pe, char const* ch)
{
	return parse_ws(p, pe) && p != pe && strchr(ch, *p) ? (++p, true) : false;
}

static bool parse_until (char const*& p, char const*& pe, char const* stopChars, std::string& res)
{
	res.clear();
	for(; p != pe && !strchr(stopChars, *p); ++p)
	{
		if(*p == '\\' && p+1 != pe && (p[1] == '\\' || strchr(stopChars, p[1])))
			++p;
		res += *p;
	}
	return true;
}

static bool parse_comment (char const*& p, char const*& pe)
{
	std::string commentString;
	if(parse_char(p, pe, "#") && !parse_until(p, pe, "\n", commentString))
		p = pe; // failed to find ‘\n’ (at EOF) so ‘parse_until’ backtracked to start of comment
	return true;
}

static bool parse_string_single (char const*& p, char const* pe, std::string& res)
{
	char const* bt = p;
	return parse_ws(p, pe) && parse_char(p, pe, "'") && parse_until(p, pe, "'\n", res) && parse_char(p, pe, "'") || backtrack(p, bt);
}

static bool parse_string_double (char const*& p, char const* pe, std::string& res)
{
	char const* bt = p;
	return parse_ws(p, pe) && parse_char(p, pe, "\"") && parse_until(p, pe, "\"\n", res) && parse_char(p, pe, "\"") || backtrack(p, bt);
}

static bool parse_string_unquoted (char const*& p, char const* pe, std::string& res)
{
	char const* bt = p;
	return parse_ws(p, pe) && p != pe && *p != '\n' && parse_until(p, pe, " \t\n", res) && !res.empty() || backtrack(p, bt);
}

static bool parse_string (char const*& p, char const* pe, std::string& res)
{
	return parse_string_single(p, pe, res) || parse_string_double(p, pe, res) || parse_string_unquoted(p, pe, res);
}

static bool parse_key (char const*& p, char const* pe, std::string& res)
{
	char const* bt = p;
	bool match = parse_ws(p, pe) && p != pe && *p != '\n' && parse_until(p, pe, "= \t\n", res) && !res.empty() || backtrack(p, bt);
	return match;
}

static bool parse_assignment (char const*& p, char const* pe, std::pair<std::string, std::string>& res)
{
	char const* bt = p;
	return parse_key(p, pe, res.first) && parse_char(p, pe, "=") && parse_string(p, pe, res.second) || backtrack(p, bt);
}

static bool parse_header (char const*& p, char const* pe, std::vector<std::string>& res)
{
	char const* bt = p;
	if(parse_char(p, pe, "["))
	{
		std::string name;
		while(parse_string_single(p, pe, name) || parse_string_double(p, pe, name) || (parse_ws(p, pe) && parse_until(p, pe, ";] \t\n", name)))
		{
			res.push_back(name);
			if(parse_char(p, pe, "]"))
				return true;
			if(!parse_char(p, pe, ";"))
				break;
		}
	}
	res.clear();
	return backtrack(p, bt);
}

static bool parse_line (char const*& p, char const* pe, ini_file_t& res, int n = 0)
{
	std::vector<std::string> header;
	std::pair<std::string, std::string> assignment;

	char const* bt = p;
	parse_ws(p, pe);
	parse_comment(p, pe);

	if(parse_header(p, pe, header))
		res.new_section(header);
	else if(parse_assignment(p, pe, assignment))
		res.insert_value(assignment.first, assignment.second, n);

	if(parse_comment(p, pe) && (p == pe || parse_char(p, pe, "\n")))
		return true;

	std::string trailing;
	parse_until(p, pe, "\n", trailing);
	os_log_error(OS_LOG_DEFAULT, "%{public}s:%d: error: incorrect syntax ‘%.*s’", res.path.c_str(), n, (int)((strchr(bt, '\n') ?: pe) - bt), bt);
	parse_char(p, pe, "\n");
	return false;
}

char const* parse_ini (char const* p, char const* pe, ini_file_t& res)
{
	for(int n = 1; p != pe; ++n)
	{
		// fprintf(stderr, "parse line %d: ‘%.*s’ (%s)\n", n, (strchr(p, '\n') ?: pe) - p, p, res.path.c_str());
		parse_line(p, pe, res, n);
	}
	return p;
}
