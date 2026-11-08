#ifndef PARSER_H_E2QKW5G8
#define PARSER_H_E2QKW5G8

#include "parser_fwd.h"
#include "regexp.h"
#include <oak/misc.h>
#include <variant>

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

	struct node_t
	{
		using variant_type = std::variant<
			text_t,
			placeholder_t, placeholder_transform_t, placeholder_choice_t,
			variable_t, variable_transform_t, variable_fallback_t, variable_condition_t, variable_change_t,
			case_change_t,
			code_t
		>;

		variant_type data;

		node_t () : data(text_t{}) { }
		node_t (node_t const&) = default;
		node_t (node_t&&) = default;
		node_t& operator= (node_t const&) = default;
		node_t& operator= (node_t&&) = default;

		template <typename T, typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, node_t>>>
		node_t (T&& value) : data(std::forward<T>(value)) { }
	};

	OnigOptionType convert (regexp_options::type const& options);

	nodes_t parse_format_string (std::string const& str, char const* stopChars = "", size_t* length = nullptr);
	nodes_t parse_snippet (std::string const& str);

} /* parser */

#endif /* end of include guard: PARSER_H_E2QKW5G8 */
