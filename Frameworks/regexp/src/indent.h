#ifndef REGEXP_INDENT_H_5C9Q0OOM
#define REGEXP_INDENT_H_5C9Q0OOM

#include "regexp.h"
#include <oak/oak.h>

namespace indent
{
	std::string create (size_t size, size_t tabSize, bool softTabs);
	int leading_whitespace (char const* it, char const* last, size_t tabSize);

	enum pattern_type
	{
		kIncrease = 1, kDecrease = 2, kIncreaseNext = 4, kIgnore = 8, kZeroIndent = 16
	};

	// ================
	// = FSM Approach =
	// ================

	struct fsm_t
	{
		fsm_t (size_t indentSize, size_t tabSize) : _indent_size(indentSize), _tab_size(tabSize), _level(0), _carry(0), _seen(0)
		{
		}

		bool is_seeded (std::string const& line, std::map<pattern_type, regexp::pattern_t> const& patterns);
		bool is_ignored (std::string const& line, std::map<pattern_type, regexp::pattern_t> const& patterns) const;
		size_t scan_line (std::string const& line, std::map<pattern_type, regexp::pattern_t> const& patterns); // returns indent for this line

	private:
		ssize_t _indent_size, _tab_size;
		ssize_t _level, _carry;
		size_t _seen;
		size_t _last_type;
		ssize_t _last_indent;
	};

} /* indent */

#endif /* end of include guard: REGEXP_INDENT_H_5C9Q0OOM */
