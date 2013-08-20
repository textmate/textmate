#include "grammar.h"
#include "private.h"
#include <regexp/regexp.h>
#include <regexp/format_string.h>
#include <bundles/bundles.h>
#include <oak/oak.h>

OAK_DEBUG_VAR(Parser);
OAK_DEBUG_VAR(Parser_Flow);

namespace parse
{
	size_t rule_t::rule_id_counter = 0;

	bool equal (stack_ptr lhs, stack_ptr rhs)
	{
		return lhs == rhs || lhs && rhs && *lhs == *rhs;
	}

	static bool pattern_is_format_string (std::string const& ptrn)
	{
		bool res = oak::contains(ptrn.begin(), ptrn.end(), '$');
		D(DBF_Parser, bug("%s: %s\n", ptrn.c_str(), BSTR(res)););
		return res;
	}

	static OnigOptionType anchor_options (bool isFirstLine, bool isGPos, char const* first, char const* last)
	{
		OnigOptionType res = ONIG_OPTION_NONE;
		if(!isFirstLine)
			res |= ONIG_OPTION_NOTBOS;
		if(!isGPos)
			res |= ONIG_OPTION_NOTGPOS;
		if(first != last && last[-1] == '\n')
			res |= ONIG_OPTION_NOTEOS;
		return res;
	}

	template <typename _OutputIter>
	_OutputIter escape_regexp (char const* it, char const* last, _OutputIter out)
	{
		DB(std::string tmp);
		DB(std::string org(it, last));
		static char const* special = "\\|([{}]).?*+^$";
		while(it != last)
		{
			if(strchr(special, *it))
			{
				DB(tmp += '\\');
				*out++ = '\\';
			}
			DB(tmp += *it);
			*out++ = *it++;
		}
		D(DBF_Parser, bug("%s → %s\n", org.c_str(), tmp.c_str()););
		return out;
	}

	static std::string expand_back_references (std::string const& ptrn, regexp::match_t const& m)
	{
		bool escape = false;
		std::string res;
		iterate(it, ptrn)
		{
			if(escape && isdigit(*it))
			{
				int i = digittoint(*it);
				if(!m.empty(i))
					escape_regexp(m.buffer() + m.begin(i), m.buffer() + m.end(i), back_inserter(res));
				escape = false;
				continue;
			}

			if(escape)
				res += '\\';
			if(!(escape = !escape && *it == '\\'))
				res += *it;
		}
		D(DBF_Parser, bug("%s → %s\n", ptrn.c_str(), res.c_str()););
		return res;
	}

	bool stack_t::operator== (stack_t const& rhs) const
	{
		if(*rule != *rhs.rule || scope != rhs.scope)
			return false;
		if(while_pattern != rhs.while_pattern || end_pattern != rhs.end_pattern)
			return false;
		if((!parent && rhs.parent) || (parent && (!rhs.parent || *parent != *rhs.parent)))
			return false;
		return true;
	}

	bool stack_t::operator!= (stack_t const& rhs) const
	{
		return !(*this == rhs);
	}

	struct ranked_match_t
	{
		ranked_match_t (rule_t const* rule, regexp::match_t const& match, size_t rank, bool is_end_pattern = false) : rule(rule), match(match), rank(rank), is_end_pattern(is_end_pattern) { }
		rule_t const* rule;
		regexp::match_t match;
		size_t rank;
		bool is_end_pattern;

		WATCH_LEAKS(ranked_match_t);

		bool operator< (ranked_match_t const& rhs) const
		{
			return match.begin() == rhs.match.begin() ? rank < rhs.rank : match.begin() < rhs.match.begin();
		}
	};

	static scope::scope_t create_scope (scope::scope_t const& current_scope, std::string const& format_string, regexp::match_t const& match)
	{
		return current_scope.append(pattern_is_format_string(format_string) ? format_string::expand(format_string, match.captures()) : format_string, true);
	}

	static void apply_captures (scope::scope_t scope, regexp::match_t const& m, repository_ptr const& captures, std::map<size_t, scope::scope_t>& res, bool firstLine)
	{
		if(!captures)
			return;

		std::multimap<std::pair<size_t, ssize_t>, rule_ptr> rules;

		repository_t::const_iterator ruleIter = captures->begin();
		std::multimap<std::string, std::pair<size_t, size_t> >::const_iterator indexIter = m.capture_indices().begin();
		while(ruleIter != captures->end() && indexIter != m.capture_indices().end())
		{
			if(ruleIter->first == indexIter->first)
				rules.insert(std::make_pair(std::make_pair(indexIter->second.first, -(indexIter->second.second - indexIter->second.first)), ruleIter->second));

			if(ruleIter->first < indexIter->first)
					++ruleIter;
			else	++indexIter;
		}

		std::vector< std::pair<size_t, scope::scope_t> > stack;
		iterate(it, rules)
		{
			size_t from = it->first.first;
			for(; !stack.empty() && stack.back().first <= from; stack.pop_back())
				scope = res[stack.back().first] = stack.back().second;

			size_t to = it->first.first - it->first.second;
			stack.push_back(std::make_pair(to, scope));

			rule_ptr const& rule = it->second;
			if(rule->scope_string != NULL_STR)
				scope = res[from] = create_scope(scope, rule->scope_string, m);

			if(!rule->children.empty())
			{
				D(DBF_Parser, bug("re-parse: ‘%.*s’ (range %zu-%zu)\n", (int)(to - from), m.buffer() + from, from, to););
				parse::stack_ptr stack(new parse::stack_t(rule.get(), scope));
				stack->anchor = from;
				parse(m.buffer(), m.buffer() + to, stack, res, firstLine, from);
			}
		}

		for(; !stack.empty(); stack.pop_back())
			scope = res[stack.back().first] = stack.back().second;
	}

	static void collect_children (std::vector<rule_ptr> const& children, std::vector<rule_t*>& res);

	static void collect_rule (rule_t* rule, std::vector<rule_t*>& res)
	{
		while(rule && rule->include)
			rule = rule->include;

		if(!rule || rule->included)
			return;

		if(rule->match_pattern)
		{
			rule->included = true;
			res.push_back(rule);
		}
		else if(!rule->children.empty())
		{
			collect_children(rule->children, res);
		}
	}

	static void collect_children (std::vector<rule_ptr> const& children, std::vector<rule_t*>& res)
	{
		for(rule_ptr const& rule : children)
			collect_rule(rule.get(), res);
	}

	static void collect_injections (stack_ptr const& stack, scope::context_t const& scope, std::vector<rule_t*>& res)
	{
		for(stack_ptr node = stack; node; node = node->parent)
		{
			for(auto const& pair : node->rule->injections)
			{
				if(pair.first.does_match(scope))
					collect_rule(pair.second.get(), res);
			}
		}
	}

	static void collect_rules (char const* first, char const* last, size_t i, bool firstLine, stack_ptr const& stack, std::set<ranked_match_t>& res, std::map<size_t, regexp::match_t>& match_cache)
	{
		std::vector<rule_t*> rules;
		collect_injections(stack, scope::context_t(stack->scope, ""), rules);
		collect_children(stack->rule->children, rules);
		collect_injections(stack, scope::context_t("", stack->scope), rules);

		// ============================
		// = Match rules against text =
		// ============================

		res.clear();

		OnigOptionType const options = anchor_options(firstLine, stack->anchor == i, first, last);
		if(stack->end_pattern)
		{
			D(DBF_Parser, bug("end pattern: %s\n", to_s(stack->end_pattern).c_str()););
			if(regexp::match_t const& match = regexp::search(stack->end_pattern, first, last, first + i, last, options))
				res.emplace(stack->rule, match, stack->apply_end_last ? SIZE_T_MAX : 0, true);
		}

		size_t rank = 0;
		for(rule_t* rule : rules)
		{
			rule->included = false;

			auto it = match_cache.find(rule->rule_id);
			if(it != match_cache.end())
			{
				if(it->second)
					res.emplace(rule, it->second, ++rank);
			}
			else
			{
				auto match = regexp::search(rule->match_pattern, first, last, first + i, last, options);
				if(!rule->match_pattern_is_anchored)
					match_cache.emplace(rule->rule_id, match);
				if(match)
					res.emplace(rule, match, ++rank);
			}
		}
	}

	static bool has_cycle (size_t rule_id, size_t i, stack_ptr const& stack)
	{
		if(!stack->zw_begin_match || stack->anchor != i)
			return false;
		else if(rule_id == stack->rule->rule_id)
			return true;
		return stack->parent ? has_cycle(rule_id, i, stack->parent) : false;
	}

	stack_ptr parse (char const* first, char const* last, stack_ptr stack, std::map<size_t, scope::scope_t>& scopes, bool firstLine, size_t i)
	{
		D(DBF_Parser_Flow, bug("%.*s", (int)(last - first), first););

		// ==============================
		// = apply the ‘while’ patterns =
		// ==============================

		std::vector<stack_ptr> while_rules;
		for(stack_ptr node = stack; node->while_pattern; node = node->parent)
			while_rules.push_back(node);

		scope::scope_t scope = scopes[i] = while_rules.empty() ? stack->scope : while_rules.back()->parent->scope;
		D(DBF_Parser, bug("%s, offset %zu, %zu while rules\n", to_s(scope).c_str(), i, while_rules.size()););
		riterate(it, while_rules)
		{
			if(regexp::match_t const& m = regexp::search((*it)->while_pattern, first, last, first + i))
			{
				D(DBF_Parser_Flow, bug("while match %zu-%zu\n", m.begin(), m.end()););

				rule_t const* rule = (*it)->rule;
				if(rule->scope_string != NULL_STR)
					scope = scopes[m.begin()] = create_scope(scope, rule->scope_string, m);
				apply_captures(scope, m, rule->while_captures ?: rule->captures, scopes, firstLine);
				if(rule->content_scope_string != NULL_STR)
					scope = scopes[m.end()] = create_scope(scope, rule->content_scope_string, m);

				stack->anchor = i = m.end();
				continue;
			}

			stack = (*it)->parent;
			if(stack->while_pattern)
				stack->anchor = i;
			break;
		}

		// ======================
		// = Parse rest of line =
		// ======================

		std::set<ranked_match_t> rules;
		std::map<size_t, regexp::match_t> match_cache;
		collect_rules(first, last, i, firstLine, stack, rules, match_cache);

		D(DBF_Parser, bug("%zu rules (out of %zu), parse: %.*s", rules.size(), stack->rule->children.size(), (int)(last - first - i), first + i););
		while(!rules.empty())
		{
			DB(
				D(DBF_Parser, bug("offset: %zu\n", i););
				iterate(it, rules)
					D(DBF_Parser, bug("\t%zu-%zu, %s\n", it->match.begin(), it->match.end(), to_s(it->rule->match_pattern).c_str()););
			)

			ranked_match_t m = *rules.begin();
			rules.erase(rules.begin());

			if(m.match.begin() < i)
			{
				regexp::pattern_t const& ptrn = m.is_end_pattern ? stack->end_pattern : m.rule->match_pattern;
				if(m.match = regexp::search(ptrn, first, last, first + i, last, anchor_options(firstLine, stack->anchor == i, first, last)))
					rules.insert(m);
				continue;
			}

			i = m.match.end();
			D(DBF_Parser_Flow, bug("match %2zu-%2zu: %s\n", m.match.begin(), m.match.end(), m.rule->scope_string != NULL_STR ? m.rule->scope_string.c_str() : "(untitled)"););

			rule_t const* rule = m.rule;
			if(m.is_end_pattern)
			{
				scope = stack->scope;
				if(stack->rule->content_scope_string != NULL_STR)
					scope = scopes[m.match.begin()] = scope.parent();
				apply_captures(scope, m.match, rule->end_captures ?: rule->captures, scopes, firstLine);

				bool nothingMatched = stack->zw_begin_match && stack->anchor == i;

				stack = stack->parent;
				scope = scopes[m.match.end()] = stack->scope;
				D(DBF_Parser_Flow, bug("leaving, new scope %s\n", to_s(scope).c_str()););

				if(nothingMatched) // we left a begin/end rule but haven’t parsed any bytes, so we’re destined to repeat this mistake
				{
					fprintf(stderr, "*** no bytes parsed by rule ‘%s’, begin = ‘%s’, end = ‘%s’, position %zu for line: %.*s\n", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), rule->end_string.c_str(), i, (int)(last - first), first);
					break;
				}
			}
			else if(!rule->children.empty() || rule->while_string != NULL_STR || rule->end_string != NULL_STR) // begin-part of rule
			{
				if(m.match.empty() && has_cycle(rule->rule_id, i, stack))
				{
					fprintf(stderr, "*** no bytes matched and recursive include of rule ‘%s’, begin = ‘%s’, end = ‘%s’, position %zu for line: %.*s\n", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), rule->end_string.c_str(), i, (int)(last - first), first);
					break;
				}

				if(rule->scope_string != NULL_STR)
					scope = scopes[m.match.begin()] = create_scope(scope, rule->scope_string, m.match);
				apply_captures(scope, m.match, rule->begin_captures ?: rule->captures, scopes, firstLine);
				if(rule->content_scope_string != NULL_STR)
					scope = scopes[m.match.end()] = create_scope(scope, rule->content_scope_string, m.match);

				stack.reset(new stack_t(rule, scope, stack));
				stack->while_pattern  = rule->while_pattern;
				stack->end_pattern    = rule->end_pattern;
				stack->apply_end_last = rule->apply_end_last == "1";
				stack->anchor         = i;
				stack->zw_begin_match = m.match.empty();
				stack->parent->anchor = SIZE_T_MAX;

				if(!rule->while_pattern && rule->while_string != NULL_STR)
					stack->while_pattern = expand_back_references(rule->while_string, m.match);
				if(!rule->end_pattern && rule->end_string != NULL_STR)
					stack->end_pattern = expand_back_references(rule->end_string, m.match);
				
				D(DBF_Parser_Flow, bug("descending, new scope %s\n", to_s(scope).c_str()););
			}
			else // regular match-rule
			{
				if(m.match.empty())
				{
					fprintf(stderr, "*** no bytes parsed by rule ‘%s’, match = ‘%s’, position %zu for line: %.*s\n", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), i, (int)(last - first), first);
					continue; // do not re-apply since this matched zero characters
				}

				if(rule->scope_string != NULL_STR)
				{
					scope::scope_t const& newScope = create_scope(scope, rule->scope_string, m.match);
					scopes[m.match.begin()] = newScope;
					apply_captures(newScope, m.match, rule->captures, scopes, firstLine);
				}
				else
				{
					apply_captures(scope, m.match, rule->captures, scopes, firstLine);
				}
				scopes[m.match.end()] = scope;

				if(m.match = regexp::search(m.rule->match_pattern, first, last, first + i, last, anchor_options(firstLine, stack->anchor == i, first, last)))
					rules.insert(m);

				continue; // no context change, so skip finding rules for this context
			}

			D(DBF_Parser, bug("%zu rules before collecting\n", rules.size()););
			collect_rules(first, last, i, firstLine, stack, rules, match_cache);
			D(DBF_Parser, bug("%zu rules after collecting\n", rules.size()););
		}
		D(DBF_Parser_Flow, bug("line done (%zu rules)\n", rules.size()););
		stack->anchor = first + stack->anchor == last ? 0 : SIZE_T_MAX;
		return stack;
	}
}
