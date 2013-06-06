#ifndef PARSE_PRIVATE_H_XR3VJA2H
#define PARSE_PRIVATE_H_XR3VJA2H

#include "parse.h"
#include <scope/scope.h>
#include <regexp/regexp.h>

namespace parse
{
	struct rule_t;

	typedef std::shared_ptr<rule_t> rule_ptr;
	typedef std::weak_ptr<rule_t> rule_weak_ptr;
	typedef std::map<std::string, rule_ptr> repository_t;
	typedef std::shared_ptr<repository_t> repository_ptr;

	struct rule_t
	{
		WATCH_LEAKS(rule_t);

		static size_t rule_id_counter;

		rule_t () : rule_id(++rule_id_counter), include_string(NULL_STR), scope_string(NULL_STR), content_scope_string(NULL_STR), match_string(NULL_STR), while_string(NULL_STR), end_string(NULL_STR), apply_end_last(NULL_STR), resolved_injections(false) { }

		size_t rule_id;

		bool operator== (rule_t const& rhs) const { return rule_id == rhs.rule_id; }
		bool operator!= (rule_t const& rhs) const { return rule_id != rhs.rule_id; }
		
		std::string include_string;

		std::string scope_string;
		std::string content_scope_string;

		std::string match_string;
		std::string while_string;
		std::string end_string;
		
		std::string apply_end_last;

		std::vector<rule_ptr> children;
		repository_ptr captures;
		repository_ptr begin_captures;
		repository_ptr while_captures;
		repository_ptr end_captures;
		repository_ptr repository;
		repository_ptr injections;

		// =======================
		// = Pre-parsed versions =
		// =======================

		rule_weak_ptr include;

		regexp::pattern_t match_pattern;
		regexp::pattern_t while_pattern;
		regexp::pattern_t end_pattern;

		bool resolved_injections;
	};

	struct stack_t
	{
		WATCH_LEAKS(stack_t);

		stack_t (rule_ptr const& rule, scope::scope_t const& scope, stack_ptr const& parent = stack_ptr()) : parent(parent), rule(rule), scope(scope), anchor(0) { }

		stack_ptr parent;

		rule_ptr rule;                      // the rule supplying patterns for current context
		scope::scope_t scope;               // the scope of the current context

		regexp::pattern_t while_pattern;    // a while-pattern active in current context
		regexp::pattern_t end_pattern;      // the end-pattern which exits this context
		size_t anchor;
		bool zw_begin_match;
		bool apply_end_last;

		bool operator== (stack_t const& rhs) const;
		bool operator!= (stack_t const& rhs) const;
	};

	std::vector< std::pair<scope::selector_t, rule_ptr> >& injected_grammars ();

} /* parse */

#endif /* end of include guard: PARSE_PRIVATE_H_XR3VJA2H */
