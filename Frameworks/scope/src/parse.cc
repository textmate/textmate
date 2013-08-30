#include "parse.h"

/*
	atom:         «string» | '*'
	scope:        «atom» ('.' «atom»)*
	path:         '^'? «scope» ('>'? «scope»)* '$'?
	group:        '(' «selector» ')'
	filter:       ("L:"|"R:"|"B:") («group» | «path»)
	expression:   '-'? («filter» | «group» | «path»)
	composite:    «expression» ([|&-] «expression»)*
	selector:     «composite» (',' «composite»)*
*/

// #define ENTER fprintf(stderr, "%s\n", __FUNCTION__)
#define ENTER

namespace scope
{
	namespace parse
	{
		using namespace types;

		struct context_t
		{
			char const* it;
			char const* last;

			bool ws ();

			bool parse_char (char const* ch, char* dst = NULL);

			bool parse_atom (atom_t& res);
			bool parse_scope (scope::types::scope_t& res);
			bool parse_path (path_t& res);
			bool parse_path (any_ptr& res);
			bool parse_group (any_ptr& res);
			bool parse_filter (any_ptr& res);
			bool parse_expression (expression_t& res);
			bool parse_composite (composite_t& res);
			bool parse_selector (selector_t& res);
		};

		bool context_t::parse_atom (atom_t& res)
		{
			ENTER;
			if(parse_char("*"))
				return res = scope::types::atom_any, true;

			if(it == last || (!isalnum(*it) && *it < 0x80))
				return false;

			char const* from = it;
			while(it != last && (isalnum(*it) || *it == '_' || *it == '-' || *it == '+' || *it > 0x7F))
				++it;
			res.insert(res.end(), from, it);
			return true;
		}

		bool context_t::parse_scope (scope::types::scope_t& res)
		{
			ENTER;
			bool rc = false;
			res.anchor_to_previous = parse_char(">") && ws();
			do {
				res.atoms.emplace_back();
				if(!parse_atom(res.atoms.back()))
				{
					res.atoms.pop_back();
					break;
				}
				rc = true;
			} while(parse_char("."));

			return rc;
		}

		bool context_t::parse_path (path_t& res)
		{
			ENTER;
			res.anchor_to_bol = parse_char("^") && ws();

			do {
				res.scopes.push_back(scope::types::scope_t());
				if(!parse_scope(res.scopes.back()))
					break;
			} while(ws());

			if(res.scopes.size() > 1)
				res.scopes.pop_back();

			res.anchor_to_eol = parse_char("$");
			return true;
		}

		bool context_t::parse_path (any_ptr& res)
		{
			path_t tmp;
			if(parse_path(tmp))
				return res.reset(new path_t(tmp)), true;
			return false;
		}

		bool context_t::parse_group (any_ptr& res)
		{
			ENTER;
			char const* bt = it;
			group_t group;
			if(parse_char("(") && parse_selector(group.selector) && ws() && parse_char(")"))
				return res.reset(new group_t(group)), true;
			return it = bt, false;
		}

		bool context_t::parse_filter (any_ptr& res)
		{
			ENTER;
			char const* bt = it;
			filter_t filter;
			char side;
			if(parse_char("LRB", &side) && parse_char(":") && ws() && (parse_group(filter.selector) || parse_path(filter.selector)))
				return filter.filter = filter_t::side_t(side), res.reset(new filter_t(filter)), true;
			return it = bt, false;
		}

		bool context_t::parse_expression (expression_t& res)
		{
			ENTER;
			if(parse_char("-") && ws())
				res.negate = true;
			return parse_filter(res.selector) || parse_group(res.selector) || parse_path(res.selector);
		}

		bool context_t::parse_composite (composite_t& res)
		{
			ENTER;
			bool rc = false;
			char op = expression_t::op_none;
			do {
				expression_t tmp(op);
				if(!parse_expression(tmp))
					break;
				res.expressions.push_back(tmp);
				rc = true;
			} while(ws() && parse_char("&|-", &op) && ws());
			return rc;
		}

		bool context_t::parse_selector (selector_t& res)
		{
			ENTER;
			bool rc = false;
			ws();
			do {
				composite_t tmp;
				if(!parse_composite(tmp))
					break;
				res.composites.push_back(tmp);
				rc = true;
			} while(ws() && parse_char(",") && ws());
			return rc;
		}

		// =============================
		// = General Utility Functions =
		// =============================

		bool context_t::ws ()
		{
			while(it != last && strchr(" \t", *it))
				++it;
			return true;
		}

		bool context_t::parse_char (char const* ch, char* dst)
		{
			if(it == last || !strchr(ch, *it))
				return false;
			if(dst)
				*dst = *it;
			return ++it, true;
		}

		// =======
		// = API =
		// =======

		char const* scope (char const* first, char const* last, scope::types::scope_t& scope)
		{
			context_t context = { first, last };
			context.ws();
			context.parse_scope(scope);
			return context.it;
		}

		char const* path (char const* first, char const* last, scope::types::path_t& path)
		{
			context_t context = { first, last };
			context.ws();
			context.parse_path(path);
			return context.it;
		}

		char const* selector (char const* first, char const* last, scope::types::selector_t& selector)
		{
			context_t context = { first, last };
			context.ws();
			context.parse_selector(selector);
			return context.it;
		}

	} /* parse */ 

} /* scope */ 
