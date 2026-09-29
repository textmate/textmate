#include "types.h"
#include <oak/oak.h>
#include <text/src/format.h>

namespace scope
{
	namespace types
	{
		template <typename T>
		std::string join (T const& container, std::string const& sep)
		{
			std::string res = "";
			for(auto const& it : container)
				res += (res.empty() ? "" : sep) + to_s(it);
			return res;
		}

		std::string to_s (any_ptr const& v)         { return v ? v->to_s() : "(null)"; }
		std::string to_s (scope_t const& v)         { return (v.anchor_to_previous ? "> " : "") + v.atoms; }
		std::string to_s (expression_t const& v)    { return std::string(v.op != expression_t::op_none ? text::format("%c ", v.op) : "") + (v.negate ? "-" : "") + to_s(v.selector); }
		std::string to_s (composite_t const& v)     { return join(v.expressions, " "); }
		std::string to_s (selector_t const& v)      { return join(v.composites, ", "); }

		std::string path_t::to_s () const           { return (anchor_to_bol ? "^ " : "") + join(scopes, " ") + (anchor_to_eol ? " $" : ""); }
		std::string group_t::to_s () const          { return "(" + scope::types::to_s(selector) + ")"; }
		std::string filter_t::to_s () const         { return text::format("%c:", filter) + scope::types::to_s(selector); }

	} /* types */

} /* scope */
