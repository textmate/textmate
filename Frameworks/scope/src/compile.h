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
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */