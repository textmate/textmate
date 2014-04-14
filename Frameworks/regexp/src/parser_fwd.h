#ifndef PARSER_FWD_H_T20BLRIP
#define PARSER_FWD_H_T20BLRIP

#define RW(t) boost::recursive_wrapper<t>

namespace parser
{
	struct text_t;
	struct placeholder_t;
	struct placeholder_transform_t;
	struct placeholder_choice_t;
	struct variable_t;
	struct variable_transform_t;
	struct variable_fallback_t;
	struct variable_condition_t;
	struct variable_change_t;
	struct case_change_t;
	struct code_t;

	typedef boost::variant<
		RW(text_t),
		RW(placeholder_t), RW(placeholder_transform_t), RW(placeholder_choice_t),
		RW(variable_t), RW(variable_transform_t), RW(variable_fallback_t), RW(variable_condition_t), RW(variable_change_t),
		RW(case_change_t),
		RW(code_t)
	> node_t;

	typedef std::vector<node_t> nodes_t;

} /* parser */

#undef RW

#endif /* end of include guard: PARSER_FWD_H_T20BLRIP */
