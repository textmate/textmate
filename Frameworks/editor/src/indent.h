#ifndef INDENT_H_8HVG320W
#define INDENT_H_8HVG320W

#include <regexp/src/indent.h>
#include <scope/src/scope.h>

namespace indent
{
	std::map<indent::pattern_type, regexp::pattern_t> patterns_for_scope (scope::context_t const& scope);

	template <typename T>
	std::string line (T const& buf, size_t n)
	{
		return buf.substr(buf.begin(n), buf.eol(n));
	}

	template <typename T>
	std::map<indent::pattern_type, regexp::pattern_t> patterns_for_line (T const& buf, size_t n)
	{
		return patterns_for_scope(scope::context_t(buf.scope(buf.end(n), false).left, buf.scope(buf.begin(n), false).right));
	}

	template <typename T>
	fsm_t create_fsm (T const& buf, size_t from, size_t indentSize, size_t tabSize)
	{
		fsm_t fsm(indentSize, tabSize);
		while(from-- > 0 && !fsm.is_seeded(line(buf, from), patterns_for_line(buf, from)))
			continue;
		return fsm;
	}

} /* indent */

#endif /* end of include guard: INDENT_H_8HVG320W */
