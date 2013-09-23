#ifndef FIND_H_F9GHKU01
#define FIND_H_F9GHKU01

#include <oak/oak.h>

namespace find
{
	enum options_t
	{
		none               = (     0),
		full_words         = (1 << 0),
		ignore_case        = (1 << 1),
		ignore_whitespace  = (1 << 2),
		regular_expression = (1 << 3),
		backwards          = (1 << 4),
		not_bol            = (1 << 5),
		not_eol            = (1 << 6),
		wrap_around        = (1 << 7),
		all_matches        = (1 << 8),
		extend_selection   = (1 << 9),
	};

	PUBLIC options_t operator| (options_t lhs, options_t rhs);
	PUBLIC options_t operator^ (options_t lhs, options_t rhs);
	PUBLIC options_t operator& (options_t lhs, options_t rhs);
	PUBLIC options_t& operator|= (options_t& lhs, options_t rhs);
	PUBLIC options_t& operator&= (options_t& lhs, unsigned rhs);

	struct find_implementation_t;

	struct PUBLIC find_t
	{
		find_t (std::string const& str, options_t options = none);

		std::pair<ssize_t, ssize_t> match (char const* buf, ssize_t len, std::map<std::string, std::string>* captures = NULL);
		void set_skip_first (ssize_t offset);
		void set_skip_last (ssize_t offset);

	private:
		std::shared_ptr<find_implementation_t> pimpl;
	};
}

#endif /* end of include guard: FIND_H_F9GHKU01 */
