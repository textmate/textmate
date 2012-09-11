#ifndef GLOB_H_1FT86H1L
#define GLOB_H_1FT86H1L

#include "regexp.h"

namespace path
{
	struct PUBLIC glob_t
	{
		glob_t (const char* glob, bool matchDotFiles = false)        { setup(glob, matchDotFiles); }
		glob_t (std::string const& glob, bool matchDotFiles = false) { setup(glob, matchDotFiles); }

		bool does_match (std::string const& filename) const;

	private:
		friend std::string to_s (glob_t const& glob);
		void setup (std::string const& glob, bool matchDotFiles);
		bool _negate;
		regexp::pattern_t _compiled;
	};

	PUBLIC std::vector<std::string> expand_braces (std::string const& glob);
	PUBLIC std::string to_s (glob_t const& glob);

} /* path */

#endif /* end of include guard: GLOB_H_1FT86H1L */
