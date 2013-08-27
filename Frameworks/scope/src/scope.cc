#include "scope.h"
#include "parse.h"
#include <text/format.h>
#include <text/parse.h>
#include <oak/oak.h>

namespace scope
{
	scope_t wildcard("x-any");

	// =========
	// = Scope =
	// =========

	scope_t::scope_t ()                         { ; }
	scope_t::scope_t (char const* scope)        { setup(scope); }
	scope_t::scope_t (std::string const& scope) { setup(scope); }

	void scope_t::setup (std::string const& scope)
	{
		path.reset(new scope::types::path_t);
		scope::parse::path(scope.data(), scope.data() + scope.size(), *path);
	}

	bool scope_t::has_prefix (scope_t const& rhs) const
	{
		std::vector<types::scope_t> const& lhsScopes = path     ? path->scopes     : std::vector<types::scope_t>();
		std::vector<types::scope_t> const& rhsScopes = rhs.path ? rhs.path->scopes : std::vector<types::scope_t>();

		size_t i = 0;
		for(; i < std::min(lhsScopes.size(), rhsScopes.size()); ++i)
		{
			if(lhsScopes[i] != rhsScopes[i])
				break;
		}

		return i == rhsScopes.size();
	}

	scope_t scope_t::append_scope (std::string const& atom, bool contentScope) const
	{
		scope_t res;
		res.path.reset(new types::path_t(path ? *path : types::path_t()));
		res.path->scopes.emplace_back();
		res.path->scopes.back().atoms = text::split(atom, ".");
		res.path->scopes.back().content_scope = contentScope;
		return res;
	}

	scope_t scope_t::parent_scope () const
	{
		scope_t res;
		if(path && !path->scopes.empty())
		{
			res.path.reset(new types::path_t(*path));
			res.path->scopes.pop_back();
		}
		return res;
	}

	std::string scope_t::back () const
	{
		if(path && !path->scopes.empty())
			return text::join(path->scopes.back().atoms, ".");
		return NULL_STR;
	}

	bool scope_t::operator== (scope_t const& rhs) const   { return (!path && !rhs.path) || (path && rhs.path && *path == *rhs.path); }
	bool scope_t::operator!= (scope_t const& rhs) const   { return !(*this == rhs); }
	bool scope_t::operator< (scope_t const& rhs) const    { return (!path && rhs.path) || (path && rhs.path && *path < *rhs.path); }
	scope_t::operator bool () const                       { return path ? true : false; }

	scope_t shared_prefix (scope_t const& lhs, scope_t const& rhs)
	{
		std::vector<types::scope_t> const& lhsScopes = lhs.path ? lhs.path->scopes : std::vector<types::scope_t>();
		std::vector<types::scope_t> const& rhsScopes = rhs.path ? rhs.path->scopes : std::vector<types::scope_t>();

		size_t i = 0;
		for(; i < std::min(lhsScopes.size(), rhsScopes.size()); ++i)
		{
			if(lhsScopes[i] != rhsScopes[i])
				break;
		}

		scope_t res;
		res.path.reset(new types::path_t);
		res.path->scopes.insert(res.path->scopes.end(), lhsScopes.begin(), lhsScopes.begin() + i);
		return res;
	}

	std::string xml_difference (scope_t const& lhs, scope_t const& rhs, std::string const& open, std::string const& close)
	{
		std::vector<types::scope_t> const& lhsScopes = lhs.path ? lhs.path->scopes : std::vector<types::scope_t>();
		std::vector<types::scope_t> const& rhsScopes = rhs.path ? rhs.path->scopes : std::vector<types::scope_t>();

		size_t i = 0;
		for(; i < std::min(lhsScopes.size(), rhsScopes.size()); ++i)
		{
			if(lhsScopes[i] != rhsScopes[i])
				break;
		}

		std::string res = "";
		for(size_t j = lhsScopes.size(); j > i; --j)
			res += open + "/" + to_s(lhsScopes[j-1]) + close;
		for(size_t j = i; j < rhsScopes.size(); ++j)
			res += open + to_s(rhsScopes[j]) + close;
		return res;
	}

	std::string to_s (scope_t const& s)
	{
		return s.path ? to_s(*s.path) : "";
	}

	std::string to_s (context_t const& c)
	{
		return c.left == c.right ? text::format("(l/r ‘%s’)", to_s(c.left).c_str()) : text::format("(left ‘%s’, right ‘%s’)", to_s(c.left).c_str(), to_s(c.right).c_str());
	}

	// ============
	// = Selector =
	// ============

	selector_t::selector_t ()                        { ; }
	selector_t::selector_t (char const* str)         { setup(str); }
	selector_t::selector_t (std::string const& str)  { setup(str); }

	void selector_t::setup (std::string const& str)
	{
		selector.reset(new scope::types::selector_t);
		scope::parse::selector(str.data(), str.data() + str.size(), *selector);
	}

	std::string to_s (selector_t const& s)
	{
		return s.selector ? to_s(*s.selector) : "";
	}

	// ============
	// = Matching =
	// ============

	bool selector_t::does_match (context_t const& scope, double* rank) const
	{
		if(!selector)
		{
			if(rank)
				*rank = 0;
			return true;
		}
		if(!scope.left.path || !scope.right.path)
			return false;
		return scope.left == wildcard || scope.right == wildcard || selector->does_match(*scope.left.path, *scope.right.path, rank);
	}
}
