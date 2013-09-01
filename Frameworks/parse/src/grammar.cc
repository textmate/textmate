#include "grammar.h"
#include "private.h"
#include <bundles/bundles.h>
#include <plist/plist.h>
#include <plist/schema.h>
#include <text/format.h>
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Parser);

namespace parse
{
	static rule_ptr convert_plist (plist::any_t const& plist);

	static bool convert_array (plist::array_t const& array, std::vector<rule_ptr>& res)
	{
		for(auto const& plist : array)
		{
			if(rule_ptr child = convert_plist(plist))
				res.push_back(child);
		}
		return true;
	}

	static bool convert_dictionary (plist::dictionary_t const& dict, repository_ptr& res)
	{
		res = std::make_shared<repository_t>();
		for(auto pair : dict)
		{
			if(rule_ptr child = convert_plist(pair.second))
				res->emplace(pair.first, child);
		}
		return true;
	}

	static rule_ptr convert_plist (plist::any_t const& any)
	{
		static plist::schema_t<rule_t> schema = plist::schema_t<rule_t>()
			.map("name",                &rule_t::scope_string                             )
			.map("scopeName",           &rule_t::scope_string                             )
			.map("contentName",         &rule_t::content_scope_string                     )
			.map("match",               &rule_t::match_string                             )
			.map("begin",               &rule_t::match_string                             )
			.map("while",               &rule_t::while_string                             )
			.map("end",                 &rule_t::end_string                               )
			.map("applyEndPatternLast", &rule_t::apply_end_last                           )
			.map("include",             &rule_t::include_string                           )
			.map("patterns",            &rule_t::children,             &convert_array     )
			.map("captures",            &rule_t::captures,             &convert_dictionary)
			.map("beginCaptures",       &rule_t::begin_captures,       &convert_dictionary)
			.map("whileCaptures",       &rule_t::while_captures,       &convert_dictionary)
			.map("endCaptures",         &rule_t::end_captures,         &convert_dictionary)
			.map("repository",          &rule_t::repository,           &convert_dictionary)
			.map("injections",          &rule_t::injection_rules,      &convert_dictionary)
		;

		if(plist::dictionary_t const* plist = boost::get<plist::dictionary_t>(&any))
		{
			if(!plist->empty() && (plist->find("disabled") == plist->end() || !plist::get<int32_t>(plist->find("disabled")->second)))
			{
				rule_ptr res = std::make_shared<rule_t>();
				if(schema.convert(*plist, res.get()))
					return res;
			}
		}
		return rule_ptr();
	}

	static bool pattern_has_back_reference (std::string const& ptrn)
	{
		bool escape = false;
		for(char const& ch : ptrn)
		{
			if(escape && isdigit(ch))
			{
				D(DBF_Parser, bug("%s: %s\n", ptrn.c_str(), "YES"););
				return true;
			}
			escape = !escape && ch == '\\';
		}
		D(DBF_Parser, bug("%s: %s\n", ptrn.c_str(), "NO"););
		return false;
	}

	static bool pattern_has_anchor (std::string const& ptrn)
	{
		bool escape = false;
		for(char const& ch : ptrn)
		{
			if(escape && ch == 'G')
				return true;
			escape = !escape && ch == '\\';
		}
		return false;
	}

	// =============
	// = grammar_t =
	// =============

	static void compile_patterns (rule_t* rule)
	{
		if(rule->match_string != NULL_STR)
		{
			rule->match_pattern = regexp::pattern_t(rule->match_string);
			rule->match_pattern_is_anchored = pattern_has_anchor(rule->match_string);
			if(!rule->match_pattern)
				fprintf(stderr, "bad begin/match pattern for %s\n", rule->scope_string.c_str());
		}

		if(rule->while_string != NULL_STR && !pattern_has_back_reference(rule->while_string))
		{
			rule->while_pattern = regexp::pattern_t(rule->while_string);
			if(!rule->while_pattern)
				fprintf(stderr, "bad while pattern for %s\n", rule->scope_string.c_str());
		}

		if(rule->end_string != NULL_STR && !pattern_has_back_reference(rule->end_string))
		{
			rule->end_pattern = regexp::pattern_t(rule->end_string);
			if(!rule->end_pattern)
				fprintf(stderr, "bad end pattern for %s\n", rule->scope_string.c_str());
		}

		for(rule_ptr child : rule->children)
			compile_patterns(child.get());

		repository_ptr maps[] = { rule->repository, rule->injection_rules, rule->captures, rule->begin_captures, rule->while_captures, rule->end_captures };
		for(auto const& map : maps)
		{
			if(!map)
				continue;

			for(auto const& pair : *map)
				compile_patterns(pair.second.get());
		}

		if(rule->injection_rules)
			std::copy(rule->injection_rules->begin(), rule->injection_rules->end(), back_inserter(rule->injections));
		rule->injection_rules.reset();
	}

	void grammar_t::setup_includes (rule_ptr const& rule, rule_ptr const& base, rule_ptr const& self, grammar_t::rule_stack_t const& stack)
	{
		ASSERT(!rule->include);

		std::string const include = rule->include_string;
		if(include == "$base")
		{
			rule->include = base.get();
		}
		else if(include == "$self")
		{
			rule->include = self.get();
		}
		else if(include != NULL_STR)
		{
			static auto find_repository_item = [](rule_t const* rule, std::string const& name) -> rule_t*
			{
				if(rule->repository)
				{
					auto it = rule->repository->find(name);
					if(it != rule->repository->end())
						return it->second.get();
				}
				return nullptr;
			};

			if(include[0] == '#')
			{
				std::string const name = include.substr(1);
				for(rule_stack_t const* node = &stack; node && !rule->include; node = node->parent)
					rule->include = find_repository_item(node->rule, name);
			}
			else
			{
				std::string::size_type fragment = include.find('#');
				if(rule_ptr grammar = find_grammar(include.substr(0, fragment), base))
					rule->include = fragment == std::string::npos ? grammar.get() : find_repository_item(grammar.get(), include.substr(fragment+1));
			}

			if(!rule->include)
			{
				if(base != self)
						fprintf(stderr, "%s → %s: include not found ‘%s’\n", base->scope_string.c_str(), self->scope_string.c_str(), include.c_str());
				else	fprintf(stderr, "%s: include not found ‘%s’\n", self->scope_string.c_str(), include.c_str());
			}
		}
		else
		{
			for(rule_ptr child : rule->children)
				setup_includes(child, base, self, rule_stack_t(rule.get(), &stack));

			repository_ptr maps[] = { rule->repository, rule->injection_rules, rule->captures, rule->begin_captures, rule->while_captures, rule->end_captures };
			for(auto const& map : maps)
			{
				if(!map)
					continue;

				for(auto const& pair : *map)
					setup_includes(pair.second, base, self, rule_stack_t(rule.get(), &stack));
			}
		}
	}

	rule_ptr grammar_t::find_grammar (std::string const& scope, rule_ptr const& base)
	{
		auto it = _grammars.find(scope);
		if(it != _grammars.end())
			return it->second;
		for(auto item : bundles::query(bundles::kFieldGrammarScope, scope, scope::wildcard, bundles::kItemTypeGrammar))
			return add_grammar(scope, item->plist(), base);
		return rule_ptr();
	}

	std::vector<std::pair<scope::selector_t, rule_ptr>> grammar_t::injection_grammars ()
	{
		std::vector<std::pair<scope::selector_t, rule_ptr>> res;
		for(auto item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		{
			std::string injectionSelector = item->value_for_field(bundles::kFieldGrammarInjectionSelector);
			if(injectionSelector != NULL_STR)
			{
				if(rule_ptr grammar = convert_plist(item->plist()))
				{
					setup_includes(grammar, grammar, grammar, rule_stack_t(grammar.get()));
					compile_patterns(grammar.get());
					res.emplace_back(injectionSelector, grammar);
				}
			}
		}
		return res;
	}

	rule_ptr grammar_t::add_grammar (std::string const& scope, plist::any_t const& plist, rule_ptr const& base)
	{
		rule_ptr grammar = convert_plist(plist);
		if(grammar)
		{
			_grammars.emplace(scope, grammar);
			setup_includes(grammar, base ?: grammar, grammar, rule_stack_t(grammar.get()));
			compile_patterns(grammar.get());
		}
		return grammar;
	}

	grammar_t::grammar_t (bundles::item_ptr const& grammarItem) : _bundles_callback(*this)
	{
		bundles::add_callback(&_bundles_callback);
		set_item(grammarItem);
	}

	grammar_t::~grammar_t ()
	{
		bundles::remove_callback(&_bundles_callback);
	}

	void grammar_t::set_item (bundles::item_ptr const& item)
	{
		ASSERT(item);
		_grammars.clear();

		_item  = item;
		_plist = item->plist();
		_rule  = add_grammar(item->value_for_field(bundles::kFieldGrammarScope), _plist);

		if(!_rule)
		{
			fprintf(stderr, "*** grammar missing for ‘%s’\n", _item->name().c_str());
			_rule = std::make_shared<rule_t>();
		}

		auto const grammars = injection_grammars();
		_rule->injections.insert(_rule->injections.end(), grammars.begin(), grammars.end());
		_rule->is_root = true;

		_callbacks(&callback_t::grammar_did_change);
	}

	oak::uuid_t const& grammar_t::uuid () const
	{
		static oak::uuid_t const FallbackGrammarUUID = oak::uuid_t().generate();
		return _item ? _item->uuid() : FallbackGrammarUUID;
	}

	stack_ptr grammar_t::seed () const
	{
		return std::make_shared<stack_t>(_rule.get(), _rule ? _rule->scope_string : "");
	}

	void grammar_t::bundles_did_change ()
	{
		bundles::item_ptr newItem = bundles::lookup(uuid());
		if(newItem && !plist::equal(_plist, newItem->plist())) // FIXME this is a kludge, ideally we should register as callback for the bundle item (when that is supported)
			set_item(newItem);
	}

	// ==============
	// = Public API =
	// ==============

	grammar_ptr parse_grammar (bundles::item_ptr const& grammarItem)
	{
		ASSERT(grammarItem);
		return std::make_shared<grammar_t>(grammarItem);
	}

} /* parse */ 
