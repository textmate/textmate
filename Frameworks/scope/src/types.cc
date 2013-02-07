#include "types.h"
#include <oak/oak.h>
#include <text/format.h>

namespace scope
{
	namespace types
	{
		atom_t const atom_any = "*";

		std::string to_s (atom_t const& v);

		template <typename T>
		std::string join (T const& container, std::string const& sep)
		{
			std::string res = "";
			iterate(it, container)
				res += (res.empty() ? "" : sep) + to_s(*it);
			return res;
		}

		std::string to_s (any_ptr const& v)         { return v ? v->to_s() : "(null)"; }

		std::string to_s (atom_t const& v)          { return v.empty() ? "(empty)" : v.c_str(); }
		std::string to_s (scope_t const& v)         { return (v.anchor_to_previous ? "> " : "") + join(v.atoms, "."); }

		std::string to_s (path_t const& v)          { return (v.anchor_to_bol ? "^ " : "") + join(v.scopes, " ") + (v.anchor_to_eol ? " $" : ""); }

		std::string to_s (group_t const& v)         { return "(" + to_s(v.selector) + ")"; }
		std::string to_s (filter_t const& v)        { return text::format("%c:", v.filter) + to_s(v.selector); }

		std::string to_s (expression_t const& v)    { return std::string(v.op != expression_t::op_none ? text::format("%c ", v.op) : "") + (v.negate ? "-" : "") + to_s(v.selector); }
		std::string to_s (composite_t const& v)     { return join(v.expressions, " "); }

		std::string to_s (selector_t const& v)      { return join(v.composites, ", "); }

		std::string path_t::to_s () const           { return scope::types::to_s(*this); }
		std::string group_t::to_s () const          { return scope::types::to_s(*this); }
		std::string filter_t::to_s () const         { return scope::types::to_s(*this); }

	} /* types */ 

} /* scope */ 
