#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include "scope.h"
#include "types.h"

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
		typedef std::vector<scope::types::atom_t> scopex;
		typedef std::vector<scopex> scopesx;
		
		struct analyze_t
		{
			
			std::map<std::string, analyze_t> path;
			scopesx or_paths;
			scopesx not_paths;
			std::set<int> simple;
			std::map<int, int> multi_part;
			
			void compress ();
			bool has_any ();
			void clear ();
			std::string to_s (int indent=0) const;
		};
		
		// T must support:
		// +, scope::selector_t scope_selector
		template<typename T>
		const compiled_t<T> build (const std::vector<T> const& list);
		void build (scope::types::path_t const& path, scope::compile::analyze_t& root, bool negate, int id);
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */
