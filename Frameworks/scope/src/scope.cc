#include "scope.h"
#include "parse.h"
#include <text/format.h>
#include <text/parse.h>
#include <text/tokenize.h>
#include <oak/oak.h>

namespace scope
{
	scope_t wildcard("x-any");

	// ===================
	// = scope_t::node_t =
	// ===================

	scope_t::node_t::node_t (std::string const& atoms, node_t* parent) : _atoms(atoms), _parent(parent), _retain_count(1)
	{
	}

	scope_t::node_t::~node_t ()
	{
		if(_parent)
			_parent->release();
	}

	void scope_t::node_t::retain ()
	{
		++_retain_count;
	}

	void scope_t::node_t::release ()
	{
		bool shouldDelete = --_retain_count == 0;
		if(shouldDelete)
			delete this;
	}

	bool scope_t::node_t::is_auxiliary_scope () const
	{
		return strncmp(c_str(), "attr.", 5) == 0 || strncmp(c_str(), "dyn.", 4) == 0;
	}

	size_t scope_t::node_t::number_of_atoms () const
	{
		return std::count(_atoms.begin(), _atoms.end(), '.') + 1;
	}

	char const* scope_t::node_t::c_str () const
	{
		return _atoms.c_str();
	}

	// =========
	// = Scope =
	// =========

	scope_t::scope_t ()                         { ; }
	scope_t::scope_t (char const* scope)        { setup(scope); }
	scope_t::scope_t (std::string const& scope) { setup(scope); }

	scope_t::scope_t (scope_t::node_t* node) : node(node)
	{
		if(node)
			node->retain();
	}

	scope_t::~scope_t ()
	{
		if(node)
			node->release();
	}

	scope_t::scope_t (scope_t&& rhs)
	{
		std::swap(node, rhs.node);
	}

	scope_t::scope_t (scope_t const& rhs)
	{
		if(node = rhs.node)
			node->retain();
	}

	scope_t& scope_t::operator= (scope_t&& rhs)
	{
		std::swap(node, rhs.node);
		return *this;
	}

	scope_t& scope_t::operator= (scope_t const& rhs)
	{
		if(node != rhs.node)
		{
			if(node)
				node->release();
			if(node = rhs.node)
				node->retain();
		}
		return *this;
	}

	void scope_t::setup (std::string const& scope)
	{
		for(auto str : text::tokenize(scope.begin(), scope.end(), ' '))
		{
			if(!str.empty())
				push_scope(str);
		}
	}

	bool scope_t::has_prefix (scope_t const& rhs) const
	{
		scope_t lhs = *this;
		ssize_t lhsSize = lhs.size(), rhsSize = rhs.size();
		for(ssize_t i = 0; i < lhsSize - rhsSize; ++i)
			lhs.pop_scope();
		return lhs == rhs;
	}

	void scope_t::push_scope (std::string const& atom)
	{
		node = new node_t(atom, node);
	}

	void scope_t::pop_scope ()
	{
		ASSERT(node);
		node_t* old = node;
		if(node = node->parent())
			node->retain();
		old->release();
	}

	std::string scope_t::back () const
	{
		ASSERT(node);
		return node->c_str();
	}

	size_t scope_t::size () const
	{
		size_t res = 0;
		for(node_t* n = node; n; n = n->parent())
			++res;
		return res;
	}

	bool scope_t::empty () const
	{
		return !node;
	}

	bool scope_t::operator== (scope_t const& rhs) const
	{
		auto n1 = node, n2 = rhs.node;
		while(n1 && n2 && strcmp(n1->c_str(), n2->c_str()) == 0)
		{
			n1 = n1->parent();
			n2 = n2->parent();
		}
		return !n1 && !n2;
	}

	bool scope_t::operator< (scope_t const& rhs) const
	{
		auto n1 = node, n2 = rhs.node;
		while(n1 && n2 && strcmp(n1->c_str(), n2->c_str()) == 0)
		{
			n1 = n1->parent();
			n2 = n2->parent();
		}
		return (!n1 && n2) || (n1 && n2 && strcmp(n1->c_str(), n2->c_str()) < 0);
	}

	bool scope_t::operator!= (scope_t const& rhs) const   { return !(*this == rhs); }
	scope_t::operator bool () const                       { return !empty(); }

	scope_t shared_prefix (scope_t const& lhs, scope_t const& rhs)
	{
		size_t lhsSize = lhs.size(), rhsSize = rhs.size();
		auto n1 = lhs.node, n2 = rhs.node;

		for(size_t i = rhsSize; i < lhsSize; ++i)
			n1 = n1->parent();
		for(size_t i = lhsSize; i < rhsSize; ++i)
			n2 = n2->parent();

		while(n1 && n2 && strcmp(n1->c_str(), n2->c_str()) != 0)
		{
			n1 = n1->parent();
			n2 = n2->parent();
		}

		return scope_t(n1);
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
		selector = std::make_shared<scope::types::selector_t>();
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
		if(selector)
			return scope.left == wildcard || scope.right == wildcard || selector->does_match(scope.left, scope.right, rank);
		if(rank)
			*rank = 0;
		return true;
	}
}
