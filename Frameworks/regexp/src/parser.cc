#include "parser.h"
#include "parser_base.h"
#include <text/format.h>
#include <text/utf8.h>
#include <oak/oak.h>

/*

# In Snippets

## Placeholders

	$«int»
	${«int»}
	${«int»:«snippet»}
	${«int»/«regexp»/«format»/«options»}
	${«int»|«choice 1»,…,«choice n»|}

## Code

	`«code»`

# In Format Strings

	$0-n

	\U, \L, \E, \u, \l
	\t, \r, \n, \x{HHHH}, \xHH

	«variables»

	(?«var»:«if»:«else»}
	(?«var»:«if»}

# In Both

## Variables

	${«var»:?«if»:«else»}
	${«var»:+«if»}
	${«var»:-«else»}
	${«var»:«else»}
	${«var»/«regexp»/«format»/«options»}
	${«var»:[/upcase][/downcase][/capitalize][/asciify]}

*/

namespace parser {

OnigOptionType convert (regexp_options::type const& options)
{
	OnigOptionType res = ONIG_OPTION_NONE;
	if(options & parser::regexp_options::i)
		res |= ONIG_OPTION_IGNORECASE;
	if(options & parser::regexp_options::s)
		res |= ONIG_OPTION_SINGLELINE;
	if(options & parser::regexp_options::m)
		res |= ONIG_OPTION_MULTILINE;
	if(options & parser::regexp_options::e)
		res |= ONIG_OPTION_EXTEND;
	return res;
}

struct parse_context_t : parser_base_t
{
	WATCH_LEAKS(parser::parse_context_t);

	parse_context_t (std::string const& str) : parser_base_t(str) { }

	bool parse_regexp_options (regexp_options::type& options);

	bool parse_variable (bool(parse_context_t::*parse_content)(char const* stopChars, nodes_t& nodes), nodes_t& nodes);
	bool parse_variable_simple (nodes_t& nodes);
	bool parse_variable_complex (bool(parse_context_t::*parse_content)(char const* stopChars, nodes_t& nodes), nodes_t& nodes);
	bool parse_condition (nodes_t& nodes);
	bool parse_case_change (nodes_t& nodes);
	bool parse_control_code (nodes_t& nodes);
	bool parse_escape (char const* escapeChars, nodes_t& nodes);
	bool parse_text (nodes_t& nodes);
	bool parse_format_string (char const* stopChars, nodes_t& nodes);
	bool parse_placeholder (nodes_t& nodes);
	bool parse_code (nodes_t& nodes);
	bool parse_snippet (char const* stopChars, nodes_t& nodes);
	
	std::string& text_node (nodes_t& nodes)
	{
		if(nodes.empty() || !boost::get<text_t>(&nodes.back()))
			nodes.emplace_back();
		return boost::get<text_t>(nodes.back()).text;
	}
};

bool parse_context_t::parse_regexp_options (regexp_options::type& options)
{
	options = regexp_options::none;
	while(parse_char("giems"))
	{
		switch(it[-1])
		{
			case 'g': options = regexp_options::type(options | regexp_options::g); break;
			case 'i': options = regexp_options::type(options | regexp_options::i); break;
			case 'e': options = regexp_options::type(options | regexp_options::e); break;
			case 'm': options = regexp_options::type(options | regexp_options::m); break;
			case 's': options = regexp_options::type(options | regexp_options::s); break;
		}
	}
	return true;
}

bool parse_context_t::parse_variable (bool(parse_context_t::*parse_content)(char const* stopChars, nodes_t& nodes), nodes_t& nodes)
{
	return parse_variable_simple(nodes) || parse_variable_complex(parse_content, nodes);
}

bool parse_context_t::parse_variable_simple (nodes_t& nodes)
{
	char const* backtrack = it;
	if(parse_char("$"))
	{
		size_t index;
		if(parse_int(index))
			return nodes.push_back((variable_t){ std::to_string(index) }), true;

		std::string variable;
		if(parse_chars("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abcdefghijklmnopqrstuvwxyz", variable))
			return nodes.push_back((variable_t){ variable }), true;
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_variable_complex (bool(parse_context_t::*parse_content)(char const* stopChars, nodes_t& nodes), nodes_t& nodes)
{
	char const* backtrack = it;

	std::string name;
	if(parse_char("$") && parse_char("{") && parse_until("/:}", name))
	{
		if(it[-1] == '}')
		{
			return nodes.push_back((variable_t){ name }), true;
		}
		else if(it[-1] == '/')
		{
			variable_transform_t res = { name };
			while(it != last && *it != '/' && (parse_escape("\\/", res.pattern) || parse_variable_complex(&parse_context_t::parse_format_string, res.pattern) || parse_text(res.pattern)))
				;
			if(parse_char("/") && parse_format_string("/", res.format) && parse_regexp_options(res.options) && parse_char("}"))
				return nodes.push_back(res), true;
		}
		else // it[-1] == ':'
		{
			if(parse_char("+"))
			{
				variable_condition_t res = { name };
				if((this->*parse_content)("}", res.if_set))
					return nodes.push_back(res), true;
			}
			else if(parse_char("?"))
			{
				variable_condition_t res = { name };
				if((this->*parse_content)(":", res.if_set) && (this->*parse_content)("}", res.if_not_set))
					return nodes.push_back(res), true;
			}
			else if(parse_char("/"))
			{
				variable_change_t res = { name, transform::kNone };
				while(it[-1] == '/')
				{
					std::string option;
					if(parse_until("/}", option))
					{
						static struct { std::string option; uint8_t change; } const options[] =
						{
							{ "upcase",      transform::kUpcase     },
							{ "downcase",    transform::kDowncase   },
							{ "capitalize",  transform::kCapitalize },
							{ "asciify",     transform::kAsciify    },
						};

						for(size_t i = 0; i < sizeofA(options); ++i)
						{
							if(option == options[i].option)
								res.change |= options[i].change;
						}
					}
					else
					{
						break;
					}
				}

				if(it[-1] == '}')
					return nodes.push_back(res), true;
			}
			else
			{
				parse_char("-"); // to be backwards compatible, this character is not required
				variable_fallback_t res = { name };
				if((this->*parse_content)("}", res.fallback))
					return nodes.push_back(res), true;
			}
		}
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_condition (nodes_t& nodes)
{
	char const* backtrack = it;
	size_t captureRegister;
	if(parse_char("(") && parse_char("?") && parse_int(captureRegister) && parse_char(":"))
	{
		variable_condition_t res;
		res.name = std::to_string(captureRegister);
		if(parse_format_string(":)", res.if_set) && (it[-1] == ')' || it[-1] == ':' && parse_format_string(")", res.if_not_set) && it[-1] == ')'))
			return nodes.push_back(res), true;
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_case_change (nodes_t& nodes)
{
	char const* backtrack = it;
	if(parse_char("\\") && parse_char("ULEul"))
	{
		switch(it[-1])
		{
			case 'U': nodes.emplace_back(case_change::upper);       break;
			case 'L': nodes.emplace_back(case_change::lower);       break;
			case 'E': nodes.emplace_back(case_change::none);        break;
			case 'u': nodes.emplace_back(case_change::upper_next);  break;
			case 'l': nodes.emplace_back(case_change::lower_next);  break;
		}
		return true;
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_control_code (nodes_t& nodes)
{
	char const* backtrack = it;
	if(parse_char("\\") && parse_char("trnx"))
	{
		switch(it[-1])
		{
			case 't': text_node(nodes) += '\t'; return true;
			case 'r': text_node(nodes) += '\r'; return true;
			case 'n': text_node(nodes) += '\n'; return true;
			case 'x':
			{
				std::string value;
				if(parse_char("{") && parse_until("}", value))
				{
					if(value.size() <= 8 && std::find_if_not(value.begin(), value.end(), isxdigit) == value.end())
					{
						text_node(nodes) += utf8::to_s(std::stoul(value, nullptr, 16));
						return true;
					}
				}
				else if(it != last && it+1 != last && isxdigit(it[0]) && isxdigit(it[1]))
				{
					text_node(nodes) += digittoint(it[0]) << 4 | digittoint(it[1]);
					it += 2;
					return true;
				}
			}
			break;
		}
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_escape (char const* escapeChars, nodes_t& nodes)
{
	char const* backtrack = it;
	if(parse_char("\\") && parse_char(escapeChars))
		return text_node(nodes) += it[-1], true;
	return it = backtrack, false;
}

bool parse_context_t::parse_text (nodes_t& nodes)
{
	return it != last ? (text_node(nodes) += *it++, true) : false;
}

bool parse_context_t::parse_format_string (char const* stopChars, nodes_t& nodes)
{
	char const* backtrack = it;
	
	std::string esc = std::string("\\$(") + stopChars;
	while(it != last && !strchr(stopChars, *it))
	{
		if(false
			|| parse_variable(&parse_context_t::parse_format_string, nodes)
			|| parse_condition(nodes)
			|| parse_control_code(nodes)
			|| parse_case_change(nodes)
			|| parse_escape(esc.c_str(), nodes)
			|| parse_text(nodes))
			continue;
		break;
	}

	return (it == last && strlen(stopChars) == 0) || parse_char(stopChars) ? true : (it = backtrack, false);
}

// ==========================
// = Snippet Specific Stuff =
// ==========================

bool parse_context_t::parse_placeholder (nodes_t& nodes)
{
	char const* backtrack = it;
	if(parse_char("$"))
	{
		size_t index;
		if(parse_char("{") && parse_int(index))
		{
			if(parse_char(":"))
			{
				placeholder_t res = { index };
				if(parse_snippet("}", res.content))
					return nodes.push_back(res), true;
			}
			else if(parse_char("/"))
			{
				std::string regexp;
				placeholder_transform_t res = { index };
				if(parse_until("/", regexp) && parse_format_string("/", res.format) && parse_regexp_options(res.options) && parse_char("}"))
				{
					res.pattern = regexp::pattern_t(regexp, convert(res.options));
					return nodes.push_back(res), true;
				}
			}
			else if(parse_char("|"))
			{
				placeholder_choice_t res = { index };
				res.choices.emplace_back();
				while(parse_format_string(",|", res.choices.back()) && it[-1] == ',')
					res.choices.emplace_back();
				if(it[-1] == '|' && parse_char("}"))
					return nodes.push_back(res), true;
			}
			else if(parse_char("}"))
			{
				return nodes.push_back((placeholder_t){ index }), true;
			}
		}
		else if(parse_int(index))
		{
			return nodes.push_back((placeholder_t){ index }), true;
		}
	}
	return it = backtrack, false;
}

bool parse_context_t::parse_code (nodes_t& nodes)
{
	char const* backtrack = it;
	code_t res;
	if(parse_char("`") && parse_until("`", res.code))
		return nodes.push_back(res), true;
	return it = backtrack, false;
}

bool parse_context_t::parse_snippet (char const* stopChars, nodes_t& nodes)
{
	char const* backtrack = it;

	std::string esc = std::string("\\$`") + stopChars;
	while(it != last && !strchr(stopChars, *it))
	{
		if(false
			|| parse_placeholder(nodes)
			|| parse_variable(&parse_context_t::parse_snippet, nodes)
			|| parse_code(nodes)
			|| parse_escape(esc.c_str(), nodes)
			|| parse_text(nodes))
			continue;
		break;
	}

	return (it == last && strlen(stopChars) == 0) || parse_char(stopChars) ? true : (it = backtrack, false);
}

// =================
// = API Functions =
// =================

nodes_t parse_format_string (std::string const& str, char const* stopChars, size_t* length)
{
	parse_context_t context(str);
	nodes_t nodes;
	context.parse_format_string(stopChars, nodes);
	if(length)
		*length = context.bytes_parsed();
	return nodes;
}

nodes_t parse_snippet (std::string const& str)
{
	parse_context_t context(str);
	nodes_t nodes;
	context.parse_snippet("", nodes);
	return nodes;
}

} /* parser */ 
