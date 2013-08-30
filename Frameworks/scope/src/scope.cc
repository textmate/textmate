#include "scope.h"
#include "parse.h"
#include <text/format.h>
#include <text/parse.h>
#include <text/tokenize.h>
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

	scope_t::scope_t (scope_t&& rhs)
	{
		std::swap(path, rhs.path);
	}

	scope_t::scope_t (scope_t const& rhs)
	{
		if(!rhs.empty())
			path.reset(new scope::types::path_t(*rhs.path));
	}

	scope_t& scope_t::operator= (scope_t&& rhs)
	{
		std::swap(path, rhs.path);
		return *this;
	}

	scope_t& scope_t::operator= (scope_t const& rhs)
	{
		if(!rhs.empty())
				path.reset(new scope::types::path_t(*rhs.path));
		else	path.reset();
		return *this;
	}

	void scope_t::setup (std::string const& scope)
	{
		for(auto str : text::tokenize(scope.begin(), scope.end(), ' '))
		{
			if(!str.empty())
				push_scope(str, true);
		}
	}

	bool scope_t::has_prefix (scope_t const& rhs) const
	{
		scope_t lhs = *this;
		while(rhs.size() < lhs.size())
			lhs.pop_scope();
		return lhs == rhs;
	}

	void scope_t::push_scope (std::string const& atom, bool contentScope)
	{
		if(!path)
			path.reset(new scope::types::path_t);
		path->scopes.emplace_back();
		path->scopes.back().atoms = text::split(atom, ".");
		path->scopes.back().content_scope = contentScope;
	}

	void scope_t::pop_scope ()
	{
		if(path && !path->scopes.empty())
			path->scopes.pop_back();
	}

	std::string scope_t::back () const
	{
		if(path && !path->scopes.empty())
			return text::join(path->scopes.back().atoms, ".");
		return NULL_STR;
	}

	size_t scope_t::size () const
	{
		return path ? path->scopes.size() : 0;
	}

	bool scope_t::empty () const
	{
		return size() == 0;
	}

	bool scope_t::operator== (scope_t const& rhs) const   { return (empty() && rhs.empty()) || (path && rhs.path && *path == *rhs.path); }
	bool scope_t::operator!= (scope_t const& rhs) const   { return !(*this == rhs); }
	bool scope_t::operator< (scope_t const& rhs) const    { return (empty() && !rhs.empty()) || (path && rhs.path && *path < *rhs.path); }
	scope_t::operator bool () const                       { return !empty(); }

	scope_t shared_prefix (scope_t lhs, scope_t rhs)
	{
		while(lhs.size() < rhs.size())
			rhs.pop_scope();
		while(rhs.size() < lhs.size())
			lhs.pop_scope();

		while(lhs != rhs)
		{
			lhs.pop_scope();
			rhs.pop_scope();
		}

		return lhs;
	}

	std::string xml_difference (scope_t const& from, scope_t const& to, std::string const& open, std::string const& close)
	{
		std::vector<std::string> fromScopes, toScopes;
		for(scope_t tmp = from; !tmp.empty(); tmp.pop_scope())
			fromScopes.push_back(tmp.back());
		for(scope_t tmp = to; !tmp.empty(); tmp.pop_scope())
			toScopes.push_back(tmp.back());

		auto fromIter = fromScopes.rbegin(), toIter = toScopes.rbegin();
		while(fromIter != fromScopes.rend() && toIter != toScopes.rend() && *fromIter == *toIter)
			++fromIter, ++toIter;

		std::string res = "";
		for(auto it = fromScopes.begin(); it != fromIter.base(); ++it)
			res += open + "/" + *it + close;
		for(auto it = toIter; it != toScopes.rend(); ++it)
			res += open + *it + close;
		return res;
	}

	std::string to_s (scope_t const& s)
	{
		std::string res = "";
		for(scope_t tmp = s; !tmp.empty(); tmp.pop_scope())
		{
			if(!res.empty())
				res.append(1, ' ');
			std::string const str = tmp.back();
			res.insert(res.end(), str.rbegin(), str.rend());
		}
		return std::string(res.rbegin(), res.rend());
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
