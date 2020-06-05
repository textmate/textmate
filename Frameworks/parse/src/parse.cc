#include "grammar.h"
#include "private.h"
#include <regexp/regexp.h>
#include <regexp/format_string.h>
#include <bundles/bundles.h>
#include <text/utf8.h>
#include <oak/oak.h>

static size_t const kParserMaxLineSize = 4096;

namespace
{
	struct scopes_t
	{
		void add (size_t pos, std::string const& scope)
		{
			if(tracking)
				stack.push_back(scope);

			map.emplace(pos, record_t(scope, true));
		}

		void remove (size_t pos, std::string const& scope, bool endRule = false)
		{
			map.emplace_hint(endRule ? map.end() : map.lower_bound(pos), pos, record_t(scope, false));

			if(tracking)
			{
				if(!stack.empty() && stack.back() == scope)
						stack.pop_back();
				else	os_log_error(OS_LOG_DEFAULT, "Unbalanced scope removal: %{public}s, on stack: %{public}s", scope.c_str(), text::join(stack, " ").c_str());
			}
		}

		scope::scope_t update (scope::scope_t scope, std::map<size_t, scope::scope_t>& out) const
		{
			size_t pos = 0;

			for(auto const& pair : map)
			{
				if(pos != pair.first)
				{
					out.emplace(pos, scope);
					pos = pair.first;
				}

				if(pair.second.add)
				{
					scope.push_scope(pair.second.scope);
				}
				else
				{
					if(scope.back() == pair.second.scope)
					{
						scope.pop_scope();
					}
					else
					{
						std::vector<std::string> stack;
						while(scope.back() != pair.second.scope)
						{
							stack.emplace_back(scope.back());
							scope.pop_scope();
						}
						scope.pop_scope();
						for(auto it = stack.rbegin(); it != stack.rend(); ++it)
							scope.push_scope(*it);
					}
				}
			}

			out.emplace(pos, scope);
			return scope;
		}

		size_t tracking = 0;
		std::vector<std::string> stack;

	private:
		struct record_t
		{
			record_t (std::string const& scope, bool add) : scope(scope), add(add) { }
			std::string scope;
			bool add;
		};
		std::multimap<size_t, record_t> map;
	};
}

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
		static char const* special = "\\|([{}]).?*+^$";
		while(it != last)
		{
			if(strchr(special, *it))
				*out++ = '\\';
			*out++ = *it++;
		}
		return out;
	}

	static std::string expand_back_references (std::string const& ptrn, regexp::match_t const& m)
	{
		bool escape = false;
		std::string res;
		for(auto const& it : ptrn)
		{
			if(escape && isdigit(it))
			{
				int i = digittoint(it);
				if(!m.empty(i))
					escape_regexp(m.buffer() + m.begin(i), m.buffer() + m.end(i), back_inserter(res));
				escape = false;
				continue;
			}

			if(escape)
				res += '\\';
			if(!(escape = !escape && it == '\\'))
				res += it;
		}
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
		ranked_match_t (rule_t* rule, regexp::match_t const& match, size_t rank, bool is_end_pattern = false) : rule(rule), match(match), rank(rank), is_end_pattern(is_end_pattern) { }
		rule_t* rule;
		regexp::match_t match;
		size_t rank;
		bool is_end_pattern;

		bool operator< (ranked_match_t const& rhs) const
		{
			return match.begin() == rhs.match.begin() ? rank < rhs.rank : match.begin() < rhs.match.begin();
		}
	};

	static std::string expand (std::string const& scopeString, regexp::match_t const& match)
	{
		return pattern_is_format_string(scopeString) ? format_string::expand(scopeString, match.captures()) : scopeString;
	}

	static stack_ptr parse (char const* first, char const* last, stack_ptr stack, scopes_t& scopes, bool firstLine, size_t i);

	static void apply_captures (scope::scope_t const& scope, regexp::match_t const& m, repository_ptr const& captures, scopes_t& scopes, bool firstLine)
	{
		if(!captures)
			return;

		std::multimap<std::pair<size_t, ssize_t>, rule_ptr> rules;

		repository_t::const_iterator ruleIter = captures->begin();
		std::multimap<std::string, std::pair<size_t, size_t> >::const_iterator indexIter = m.capture_indices().begin();
		while(ruleIter != captures->end() && indexIter != m.capture_indices().end())
		{
			if(ruleIter->first == indexIter->first && indexIter->second.first != indexIter->second.second)
				rules.emplace(std::make_pair(indexIter->second.first, -(indexIter->second.second - indexIter->second.first)), ruleIter->second);

			if(ruleIter->first < indexIter->first)
					++ruleIter;
			else	++indexIter;
		}

		for(auto const& it : rules)
		{
			size_t from = it.first.first;
			size_t to = it.first.first - it.first.second;

			rule_ptr const& rule = it.second;
			if(rule->scope_string != NULL_STR)
			{
				std::string const scopeString = expand(rule->scope_string, m);
				scopes.add(from, scopeString);
				scopes.remove(to, scopeString);
			}

			if(!rule->children.empty())
			{
				auto stack = std::make_shared<parse::stack_t>(rule.get(), scope);
				stack->anchor = from;

				std::vector<std::string> tmp;
				tmp.swap(scopes.stack);
				++scopes.tracking;
				parse(m.buffer(), m.buffer() + to, stack, scopes, firstLine, from);
				while(!scopes.stack.empty())
					scopes.remove(to, scopes.stack.back(), true);
				--scopes.tracking;
				tmp.swap(scopes.stack);
			}
		}
	}

	static void collect_children (std::vector<rule_ptr> const& children, std::vector<rule_t*>& res, std::vector<rule_t*>* groups);

	static void collect_rule (rule_t* rule, std::vector<rule_t*>& res, std::vector<rule_t*>* groups)
	{
		while(rule && rule->include && !rule->included)
		{
			if(groups)
			{
				rule->included = true;
				groups->push_back(rule);
			}
			rule = rule->include;
		}

		if(!rule || rule->included)
			return;

		if(rule->match_pattern)
		{
			rule->included = true;
			res.push_back(rule);
		}
		else if(!rule->children.empty())
		{
			if(groups)
			{
				rule->included = true;
				groups->push_back(rule);
			}

			collect_children(rule->children, res, groups);
		}
	}

	static void collect_children (std::vector<rule_ptr> const& children, std::vector<rule_t*>& res, std::vector<rule_t*>* groups)
	{
		for(rule_ptr const& rule : children)
			collect_rule(rule.get(), res, groups);
	}

	static void collect_injections (stack_ptr const& stack, scope::context_t const& scope, std::vector<rule_t*> const& groups, std::vector<rule_t*>& res)
	{
		for(stack_ptr node = stack; node; node = node->parent)
		{
			for(auto const& pair : node->rule->injections)
			{
				if(pair.first.does_match(scope))
					collect_rule(pair.second.get(), res, nullptr);
			}
		}

		for(rule_t const* rule : groups)
		{
			if(rule->is_root) // already handled via the stack
				continue;

			for(auto const& pair : rule->injections)
			{
				if(pair.first.does_match(scope))
					collect_rule(pair.second.get(), res, nullptr);
			}
		}
	}

	static size_t apply_rules (size_t rank, std::vector<rule_t*> const& rules, char const* first, char const* last, OnigOptionType options, size_t i, std::set<ranked_match_t>& res, std::map<size_t, regexp::match_t>& match_cache)
	{
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
		return rank;
	}

	static void collect_rules (char const* first, char const* last, size_t i, bool firstLine, stack_ptr const& stack, std::set<ranked_match_t>& res, std::map<size_t, regexp::match_t>& match_cache)
	{
		std::vector<rule_t*> rules, groups, injectedRulesPre, injectedRulesPost;
		collect_children(stack->rule->children, rules, &groups);
		collect_injections(stack, scope::context_t(stack->scope, ""), groups, injectedRulesPre);
		collect_injections(stack, scope::context_t("", stack->scope), groups, injectedRulesPost);

		for(rule_t* rule : groups)
			rule->included = false;

		// ============================
		// = Match rules against text =
		// ============================

		res.clear();
		OnigOptionType const options = anchor_options(firstLine, stack->anchor == i, first, last);

		size_t rank = apply_rules(0, injectedRulesPre, first, last, options, i, res, match_cache);
		size_t endPatternRank = ++rank;
		rank = apply_rules(rank, rules, first, last, options, i, res, match_cache);

		if(stack->end_pattern)
		{
			if(regexp::match_t const& match = regexp::search(stack->end_pattern, first, last, first + i, last, options))
				res.emplace(stack->rule, match, stack->apply_end_last ? ++rank : endPatternRank, true);
		}

		rank = apply_rules(rank, injectedRulesPost, first, last, options, i, res, match_cache);
	}

	static bool has_cycle (size_t rule_id, size_t i, stack_ptr const& stack)
	{
		if(!stack->zw_begin_match || stack->anchor != i)
			return false;
		else if(rule_id == stack->rule->rule_id)
			return true;
		return stack->parent ? has_cycle(rule_id, i, stack->parent) : false;
	}

	static stack_ptr parse (char const* first, char const* last, stack_ptr stack, scopes_t& scopes, bool firstLine, size_t i)
	{
		// ==============================
		// = apply the ‘while’ patterns =
		// ==============================

		std::vector<stack_ptr> while_rules;
		for(stack_ptr node = stack; node->while_pattern; node = node->parent)
		{
			while_rules.push_back(node);
			if(node->scope_string != NULL_STR)
				scopes.remove(i, node->scope_string, true);
			if(node->content_scope_string != NULL_STR)
				scopes.remove(i, node->content_scope_string, true);
		}

		scope::scope_t scope = while_rules.empty() ? stack->scope : while_rules.back()->parent->scope;
		riterate(it, while_rules)
		{
			if(regexp::match_t const& m = regexp::search((*it)->while_pattern, first, last, first + i))
			{
				rule_t const* rule = (*it)->rule;
				if(rule->scope_string != NULL_STR)
				{
					std::string const scopeString = expand(rule->scope_string, m);
					scope.push_scope(scopeString);
					scopes.add(m.begin(), scopeString);
				}

				apply_captures(scope, m, rule->while_captures ?: rule->captures, scopes, firstLine);

				if(rule->content_scope_string != NULL_STR)
				{
					std::string const scopeString = expand(rule->content_scope_string, m);
					scope.push_scope(scopeString);
					scopes.add(m.end(), scopeString);
				}

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

		while(!rules.empty())
		{
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

			rule_t* rule = m.rule;
			if(m.is_end_pattern)
			{
				if(stack->content_scope_string != NULL_STR)
					scopes.remove(m.match.begin(), stack->content_scope_string, true);
				apply_captures(scope, m.match, rule->end_captures ?: rule->captures, scopes, firstLine);
				if(stack->scope_string != NULL_STR)
					scopes.remove(m.match.end(), stack->scope_string, true);

				bool nothingMatched = stack->zw_begin_match && stack->anchor == i;

				stack = stack->parent;
				scope = stack->scope;

				if(nothingMatched) // we left a begin/end rule but haven’t parsed any bytes, so we’re destined to repeat this mistake
				{
					os_log_error(OS_LOG_DEFAULT, "No bytes parsed by rule ‘%{public}s’, begin = ‘%{public}s’, end = ‘%{public}s’, position %zu for line: %.*s", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), rule->end_string.c_str(), i, (int)(last - first), first);
					break;
				}
			}
			else if(rule->while_string != NULL_STR || rule->end_string != NULL_STR) // begin-part of rule
			{
				if(m.match.empty() && has_cycle(rule->rule_id, i, stack))
				{
					os_log_error(OS_LOG_DEFAULT, "No bytes matched and recursive include of rule ‘%{public}s’, begin = ‘%{public}s’, end = ‘%{public}s’, position %zu for line: %.*s", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), rule->end_string.c_str(), i, (int)(last - first), first);
					break;
				}

				stack = std::make_shared<stack_t>(rule, scope::scope_t(), stack);

				if(rule->scope_string != NULL_STR)
				{
					stack->scope_string = expand(rule->scope_string, m.match);
					scope.push_scope(stack->scope_string);
					scopes.add(m.match.begin(), stack->scope_string);
				}

				apply_captures(scope, m.match, rule->begin_captures ?: rule->captures, scopes, firstLine);

				if(rule->content_scope_string != NULL_STR)
				{
					stack->content_scope_string = expand(rule->content_scope_string, m.match);
					scope.push_scope(stack->content_scope_string);
					scopes.add(m.match.end(), stack->content_scope_string);
				}

				stack->scope          = scope;
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
			}
			else // regular match-rule
			{
				if(m.match.empty())
				{
					os_log_error(OS_LOG_DEFAULT, "No bytes parsed by rule ‘%{public}s’, match = ‘%{public}s’, position %zu for line: %.*s", rule->scope_string != NULL_STR ? rule->scope_string.c_str() : "(untitled)", rule->match_string.c_str(), i, (int)(last - first), first);
					continue; // do not re-apply since this matched zero characters
				}

				if(rule->scope_string != NULL_STR)
				{
					std::string const scopeString = expand(rule->scope_string, m.match);
					scopes.add(m.match.begin(), scopeString);
					scopes.remove(m.match.end(), scopeString);
				}

				apply_captures(scope, m.match, rule->captures, scopes, firstLine);

				if(m.match = regexp::search(m.rule->match_pattern, first, last, first + i, last, anchor_options(firstLine, stack->anchor == i, first, last)))
					rules.insert(m);

				continue; // no context change, so skip finding rules for this context
			}

			collect_rules(first, last, i, firstLine, stack, rules, match_cache);
		}
		stack->anchor = first + stack->anchor == last ? 0 : SIZE_T_MAX;
		return stack;
	}

	stack_ptr parse (char const* first, char const* last, stack_ptr stack, std::map<size_t, scope::scope_t>& map, bool firstLine)
	{
		scopes_t scopes;
		if(last - first > kParserMaxLineSize)
			last = utf8::find_safe_end(first, first + kParserMaxLineSize);
		auto res = parse(first, last, stack, scopes, firstLine, 0);
		res->scope = scopes.update(stack->scope, map);
		return res;
	}
}
