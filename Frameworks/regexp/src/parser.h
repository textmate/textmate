#ifndef PARSER_H_E2QKW5G8
#define PARSER_H_E2QKW5G8

#include "parser_fwd.h"
#include "regexp.h"
#include <oak/misc.h>
#include <Onigmo/oniguruma.h>

namespace parser
{
	namespace regexp_options           { enum type { none = (1 << 0), g = (1 << 1), i = (1 << 2), e = (1 << 3), m = (1 << 4), s = (1 << 5) }; }
	namespace case_change              { enum type { none = 0, upper_next, lower_next, upper, lower }; };
	namespace transform                { enum type { kNone = (0 << 0), kUpcase = (1 << 0), kDowncase = (1 << 1), kCapitalize = (1 << 2), kAsciify = (1 << 3), kUrlEncode = (1 << 4), kShellEscape = (1 << 5), kRelative = (1 << 6), kNumber = (1 << 7), kDuration = (1 << 8), kDirname = (1 << 9), kBasename = (1 << 10) }; };

	struct text_t                      { std::string text; };

	struct placeholder_t               { size_t index; nodes_t content; };
	struct placeholder_choice_t        { size_t index; std::vector<nodes_t> choices; };
	struct placeholder_transform_t     { size_t index; regexp::pattern_t pattern; nodes_t format; regexp_options::type options; };

	struct variable_t                  { std::string name; };
	struct variable_transform_t        { std::string name; nodes_t pattern; nodes_t format; regexp_options::type options; };
	struct variable_fallback_t         { std::string name; nodes_t fallback; };
	struct variable_condition_t        { std::string name; nodes_t if_set, if_not_set; };
	struct variable_change_t           { std::string name; uint16_t change; };

	struct case_change_t               { case_change_t (case_change::type type) : type(type) { } case_change::type type; };
	struct code_t                      { std::string code; };

	OnigOptionType convert (regexp_options::type const& options);

	nodes_t parse_format_string (std::string const& str, char const* stopChars = "", size_t* length = nullptr);
	nodes_t parse_snippet (std::string const& str);

} /* parser */

#endif /* end of include guard: PARSER_H_E2QKW5G8 */
