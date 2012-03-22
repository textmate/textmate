#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include "scope.h"
#include <oak/oak.h>

namespace scope
{
	namespace types
	{
		typedef std::string atom_t;
		struct selector_t;
	}
	namespace compile
	{

		template<typename T>
		class compiled_t {
			bool match (scope::context_t const& scope, std::multimap<double, T>& ordered);
		};
		typedef std::vector<scope::types::atom_t> scopex;
		typedef std::vector<scopex> scopesx;
		typedef unsigned long long bits_t;
		struct PUBLIC analyze_t
		{	
			std::map<std::string, analyze_t> path;
			scopesx or_paths;
			scopesx not_paths;
			std::set<int> simple;
			std::map<int, int> multi_part;
			bits_t hash;
			
			void transform ();
			bool has_any ();
			bool match ();
			void clear ();
			std::string to_s (int indent=0) const;
		};
		
		PUBLIC void expand (analyze_t& root);
		// T must support:
		// +, scope::selector_t scope_selector

		PUBLIC void graph (analyze_t& root, selector_t& selector, int& rule_id, int& sub_rule_id);
		template<typename T>
		const compiled_t<T> compile (std::vector<T> const& rules)
		{
			analyze_t root;
			int rule_id = 0;
			int sub_rule_id=0;
			iterate(iter, rules)
			{
				auto selector =  iter->scope_selector();
				graph(root, selector, rule_id, sub_rule_id);
				rule_id++;
			}
			expand(root);
			root.transform();
			printf("root max:%s\n", "");
			printf("root max:%s\n", "");

			printf("root:\n");
			printf("%s\n", root.to_s().c_str());
			
			return compiled_t<T>();
		}
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */
