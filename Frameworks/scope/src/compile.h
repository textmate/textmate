#ifndef COMPILE_H_5XWUY4P8
#define COMPILE_H_5XWUY4P8
#include <oak/oak.h>

namespace scope
{
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
	}
}

#endif /* end of include guard: COMPILE_H_5XWUY4P8 */