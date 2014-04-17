#ifndef GLOB_H_1FT86H1L
#define GLOB_H_1FT86H1L

#include "regexp.h"

namespace path
{
	struct PUBLIC glob_t
	{
		glob_t (char const* glob, bool matchDotFiles = false)        { setup(glob, matchDotFiles); }
		glob_t (std::string const& glob, bool matchDotFiles = false) { setup(glob, matchDotFiles); }

		bool does_match (std::string const& filename) const;

	private:
		friend std::string to_s (glob_t const& glob);
		void setup (std::string const& glob, bool matchDotFiles);
		regexp::pattern_t _compiled;
	};

	enum kPathItemType { kPathItemAny, kPathItemFile, kPathItemDirectory };

	struct PUBLIC glob_list_t
	{
		glob_list_t (char const* glob = NULL)
		{
			if(glob)
				add_include_glob(glob);
		}

		void add_include_glob (std::string const& glob, kPathItemType itemType = kPathItemAny);
		void add_exclude_glob (std::string const& glob, kPathItemType itemType = kPathItemAny);

		bool include (std::string const& path, kPathItemType itemType = kPathItemAny, bool defaultResult = false) const;
		bool exclude (std::string const& path, kPathItemType itemType = kPathItemAny, bool defaultResult = true) const;

	private:
		struct record_t
		{
			record_t (bool negate, glob_t const& glob, kPathItemType itemType) : negate(negate), glob(glob), item_type(itemType) { }

			bool negate;
			glob_t glob;
			kPathItemType item_type;
		};

		std::vector<record_t> _globs;
	};

	PUBLIC std::vector<std::string> expand_braces (std::string const& glob);
	PUBLIC std::string to_s (glob_t const& glob);

} /* path */

#endif /* end of include guard: GLOB_H_1FT86H1L */
