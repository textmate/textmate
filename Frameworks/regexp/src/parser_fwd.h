#ifndef PARSER_FWD_H_T20BLRIP
#define PARSER_FWD_H_T20BLRIP

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
		text_t,
		placeholder_t, placeholder_transform_t, placeholder_choice_t,
		variable_t, variable_transform_t, variable_fallback_t, variable_condition_t, variable_change_t,
		case_change_t,
		code_t
	> node_t;

	typedef std::vector<node_t> nodes_t;

} /* parser */ 

#endif /* end of include guard: PARSER_FWD_H_T20BLRIP */
