#ifndef LOAD_GRAMMAR_H_FPR2TQML
#define LOAD_GRAMMAR_H_FPR2TQML

#include <bundles/src/bundles.h>
#include <oak/callbacks.h>

namespace parse
{
	struct rule_t;
	struct stack_t;
	typedef std::shared_ptr<rule_t> rule_ptr;
	typedef std::shared_ptr<stack_t> stack_ptr;

	struct grammar_t
	{
		struct callback_t
		{
			virtual ~callback_t () { }
			virtual void grammar_did_change () = 0;
		};

		grammar_t (bundles::item_ptr const& grammarItem);
		~grammar_t ();

		void set_item (bundles::item_ptr const& item);

		oak::uuid_t const& uuid () const;
		stack_ptr seed () const;

		void add_callback (callback_t* cb)      { _callbacks.add(cb);    }
		void remove_callback (callback_t* cb)   { _callbacks.remove(cb); }

		std::mutex& mutex () { return _mutex; }

	private:
		struct bundles_callback_t : bundles::callback_t
		{
			bundles_callback_t (grammar_t& grammar) : _grammar(grammar) { }
			void bundles_did_change ()                          { _grammar.bundles_did_change(); }
		private:
			grammar_t& _grammar;
		};

		struct rule_stack_t
		{
			rule_stack_t (rule_t const* rule, rule_stack_t const* parent = nullptr) : rule(rule), parent(parent) { }

			rule_t const* rule;
			rule_stack_t const* parent;
		};

		void setup_includes (rule_ptr const& rule, rule_ptr const& base, rule_ptr const& self, rule_stack_t const& stack);
		rule_ptr find_grammar (std::string const& scope, rule_ptr const& base);
		rule_ptr add_grammar (std::string const& scope, plist::any_t const& plist, rule_ptr const& base = rule_ptr());
		std::vector<std::pair<scope::selector_t, rule_ptr>> injection_grammars ();

		void bundles_did_change ();

		bundles::item_ptr _item;
		plist::dictionary_t _plist;
		bundles_callback_t _bundles_callback;
		oak::callbacks_t<callback_t> _callbacks;
		rule_ptr _rule;
		std::map<std::string, rule_ptr> _grammars;
		std::mutex _mutex;
	};

	typedef std::shared_ptr<grammar_t> grammar_ptr;
	grammar_ptr parse_grammar (bundles::item_ptr const& grammarItem);

} /* parse */

#endif /* end of include guard: LOAD_GRAMMAR_H_FPR2TQML */
