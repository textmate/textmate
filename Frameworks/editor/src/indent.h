#ifndef INDENT_H_8HVG320W
#define INDENT_H_8HVG320W

#include <regexp/indent.h>
#include <scope/scope.h>

namespace indent
{
	struct patterns_t
	{
		typedef regexp::pattern_t const (&array_type)[4];
		operator array_type () const { return array; }
		regexp::pattern_t array[4];
	};

	patterns_t patterns_for_scope (scope::context_t const& scope);

	template <typename T>
	std::string line (T const& buf, size_t n)
	{
		return buf.substr(buf.begin(n), buf.eol(n));
	}

	template <typename T>
	fsm_t create_fsm (T const& buf, regexp::pattern_t const (&patterns)[4], size_t from, size_t indentSize, size_t tabSize)
	{
		fsm_t fsm(patterns, indentSize, tabSize);
		while(from > 0 && !fsm.is_seeded(line(buf, --from)))
			continue;
		return fsm;
	}

} /* indent */

#endif /* end of include guard: INDENT_H_8HVG320W */
