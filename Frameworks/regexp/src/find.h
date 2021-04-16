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
		filesize_limit     = (1 << 10),
	};

	options_t operator| (options_t lhs, options_t rhs);
	options_t operator^ (options_t lhs, options_t rhs);
	options_t operator& (options_t lhs, options_t rhs);
	options_t& operator|= (options_t& lhs, options_t rhs);
	options_t& operator&= (options_t& lhs, unsigned rhs);

	struct find_implementation_t;

	struct find_t
	{
		find_t (std::string const& str, options_t options = none);

		void each_match (char const* buf, size_t len, bool moreToCome, std::function<void(std::pair<size_t, size_t> const&, std::map<std::string, std::string> const&)> const& f);
		void each_match (char const* buf, size_t len, bool moreToCome, std::function<void(std::pair<size_t, size_t> const&, std::map<std::string, std::string> const&, bool*)> const& f);

	private:
		std::shared_ptr<find_implementation_t> pimpl;
		size_t _offset = 0;
	};
}

#endif /* end of include guard: FIND_H_F9GHKU01 */
