#ifndef PARSER_H_E2QKW5G8
#define PARSER_H_E2QKW5G8

#include "parser_fwd.h"
#include "regexp.h"
#include <oak/misc.h>

namespace parser
{
	namespace regexp_options           { enum type { none = (1 << 0), g = (1 << 1), i = (1 << 2), e = (1 << 3), m = (1 << 4), s = (1 << 5) }; }
	namespace case_change              { enum type { none = 0, upper_next, lower_next, upper, lower }; };
	namespace transform                { enum type { kNone = (0 << 0), kUpcase = (1 << 0), kDowncase = (1 << 1), kCapitalize = (1 << 2), kAsciify = (1 << 3) }; };

	struct text_t                      { std::string text; WATCH_LEAKS(parser::text); };

	struct placeholder_t               { size_t index; nodes_t content; WATCH_LEAKS(parser::placeholder_t); };
	struct placeholder_choice_t        { size_t index; std::vector<nodes_t> choices; WATCH_LEAKS(parser::placeholder_choice_t); };
	struct placeholder_transform_t     { size_t index; regexp::pattern_t pattern; nodes_t format; regexp_options::type options; WATCH_LEAKS(parser::placeholder_transform_t); };

	struct variable_t                  { std::string name; WATCH_LEAKS(parser::variable_t); };
	struct variable_transform_t        { std::string name; nodes_t pattern; nodes_t format; regexp_options::type options; WATCH_LEAKS(parser::variable_transform_t); };
	struct variable_fallback_t         { std::string name; nodes_t fallback; WATCH_LEAKS(parser::variable_fallback_t); };
	struct variable_condition_t        { std::string name; nodes_t if_set, if_not_set; WATCH_LEAKS(parser::variable_condition_t); };
	struct variable_change_t           { std::string name; uint8_t change; WATCH_LEAKS(parser::variable_change_t); };

	struct case_change_t               { case_change_t (case_change::type type) : type(type) { } case_change::type type; WATCH_LEAKS(parser::case_change_t); };
	struct code_t                      { std::string code; WATCH_LEAKS(parser::code_t); };

	OnigOptionType convert (regexp_options::type const& options);

	nodes_t parse_format_string (std::string const& str, char const* stopChars = "", size_t* length = nullptr);
	nodes_t parse_snippet (std::string const& str);

} /* parser */ 

#endif /* end of include guard: PARSER_H_E2QKW5G8 */
