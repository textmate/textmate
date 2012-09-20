#ifndef REGEXP_INDENT_H_5C9Q0OOM
#define REGEXP_INDENT_H_5C9Q0OOM

#include "regexp.h"
#include <oak/oak.h>

namespace indent
{
	PUBLIC std::string create (size_t size, size_t tabSize, bool softTabs);
	PUBLIC int leading_whitespace (char const* it, char const* last, size_t tabSize);

	// ================
	// = FSM Approach =
	// ================

	struct PUBLIC fsm_t
	{
		fsm_t (regexp::pattern_t const (&patterns)[4], size_t indentSize, size_t tabSize) : _indent_size(indentSize), _tab_size(tabSize), _level(0), _carry(0), _seen(0)
		{
			std::copy(std::begin(patterns), std::end(patterns), std::begin(_patterns));
		}

		bool is_seeded (std::string const& line);
		bool is_ignored (std::string const& line) const;
		size_t scan_line (std::string const& line); // returns indent for this line

	private:
		regexp::pattern_t _patterns[4];
		ssize_t _indent_size, _tab_size;
		ssize_t _level, _carry;
		size_t _seen;
		size_t _last_type;
		ssize_t _last_indent;
	};

} /* indent */

#endif /* end of include guard: REGEXP_INDENT_H_5C9Q0OOM */
