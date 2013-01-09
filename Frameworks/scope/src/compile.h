#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include <oak/oak.h>
#include "scope.h"

namespace scope
{
	namespace types
	{
		typedef std::string atom_t;
	}

	namespace compressed
	{
		struct composite_t;
	}

	namespace compile
	{
		struct interim_t;
		typedef std::unique_ptr<interim_t> interim_unique_ptr;
		struct PUBLIC interim_t {
			std::map<std::string, interim_unique_ptr > path;

			std::set<int> simple;
			std::map<int, int> multi_part;
			int hash;
			int mask;
			bool needs_right;

			void expand_wildcards();
			void calculate_bit_fields ();
			bool has_any ();
			std::string to_s (int indent=0) const;
		};

		typedef std::vector<scope::types::atom_t> scopex;
		typedef std::vector<scopex> scopesx;
		struct PUBLIC analyze_t
		{	
			struct paths_t
			{
				scopesx or_paths;
				scopesx not_paths;
				void clear()
				{
					or_paths.clear();
					not_paths.clear();
				}
			} left, right;
			bool needs_right;
			void clear()
			{
				needs_right = false;
				left.clear();
				right.clear();
			}
		};

		struct sub_rule_t
		{
			typedef std::shared_ptr<compressed::composite_t> composite_ptr;
			composite_ptr composite;
			int rule_id;
		};
		
		class PUBLIC compiler_factory_t
		{
			analyze_t _analyzer;
			interim_t root;
			interim_t right_root;
			std::multimap<int, int> sub_rule_mapping;
			std::vector<sub_rule_t> _expressions;

		public:
			void compress (const selector_t& selector, int rule_id, int composite_index);
			void expand_wildcards () { root.expand_wildcards(); right_root.expand_wildcards(); }
			void graph (const selector_t& selector, int& rule_id, int& sub_rule_id);
			std::multimap<int, int>& sub_rule_mappings () { return sub_rule_mapping;}
			analyze_t& analyzer () { return _analyzer;}
			interim_t& interim () { return root;}
			interim_t& right_interim () { return right_root;}
			void calculate_bit_fields () { root.calculate_bit_fields();}
			std::string to_s () { return root.to_s(); }
			
		private:
			void expand_wildcards (interim_t& analyzer);
		};


		// T must support:
		// scope::selector_t scope_selector

		template<typename T>
		void compile (std::vector<T> const& rules)
		{
			compiler_factory_t compiler;
			int rule_id = 0;
			int sub_rule_id=0;
			iterate(iter, rules)
			{
				//auto selector = iter->scope_selector;
				compiler.graph(iter->scope_selector, rule_id, sub_rule_id);
				rule_id++;
			}
			// add *.<path_name> to all paths
			compiler.expand_wildcards();
			// populate bit fields
			compiler.calculate_bit_fields();

			// break out all selectors into its compressed composites
			iterate(r_id, compiler.sub_rule_mappings())
			{
				//printf("rule :%d sub:%d\n", r_id->first, r_id->second);
				//auto selector = rules[r_id->first].scope_selector;
				compiler.compress(rules[r_id->first].scope_selector, r_id->first, r_id->second);
			}
		}
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */