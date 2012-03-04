#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include "scope.h"

namespace scope
{
	namespace compile
	{
		struct Dag_Bit_t;
		struct Dag_Bit_t {
			int path;
			std::vector<Dag_Bit_t*> children;
			std::vector<Dag_Bit_t*> parents;
		};
		template<typename T>
		class compiled_t {
			bool match (scope::context_t const& scope, std::multimap<double, T>& ordered);
		};
		
		struct analyze_t
		{
			std::map<std::string, analyze_t> path;
			std::vector<int> direct;
			std::vector<int> indirect;

		};
		
		// T must support:
		// +, scope::selector_t scope_selector
		template<typename T>
		const compiled_t<T> build (const std::vector<T> const& list); 
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */
