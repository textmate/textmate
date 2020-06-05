#ifndef GLOB_H_1FT86H1L
#define GLOB_H_1FT86H1L

#include "regexp.h"

namespace path
{
	struct glob_t
	{
		glob_t (char const* glob, bool matchDotFiles = false, bool caseSensitive = true)        { setup(glob, matchDotFiles, caseSensitive); }
		glob_t (std::string const& glob, bool matchDotFiles = false, bool caseSensitive = true) { setup(glob, matchDotFiles, caseSensitive); }

		bool does_match (std::string const& filename) const;
		static std::string escape (std::string const& src);

	private:
		friend std::string to_s (glob_t const& glob);
		void setup (std::string const& glob, bool matchDotFiles, bool caseSensitive);
		regexp::pattern_t _compiled;
	};

	static size_t const kPathItemAny         = 0x0000;
	static size_t const kPathItemFile        = 0x0001;
	static size_t const kPathItemDirectory   = 0x0002;
	static size_t const kPathItemMask        = 0x007F;
	static size_t const kPathItemExclude     = 0x0080;

	struct glob_list_t
	{
		glob_list_t (char const* glob = NULL)
		{
			if(glob)
				add_include_glob(glob);
		}

		void add_glob (std::string const& glob, size_t itemType = kPathItemAny);
		void add_include_glob (std::string const& glob, size_t itemType = kPathItemAny);
		void add_exclude_glob (std::string const& glob, size_t itemType = kPathItemAny);

		bool include (std::string const& path, size_t itemType = kPathItemAny, bool defaultResult = false) const;
		bool exclude (std::string const& path, size_t itemType = kPathItemAny, bool defaultResult = true) const;

	private:
		struct record_t
		{
			record_t (glob_t const& glob, size_t itemType) : glob(glob), item_type(itemType) { }

			glob_t glob;
			size_t item_type;
		};

		std::vector<record_t> _globs;
	};

	std::vector<std::string> expand_braces (std::string const& glob);
	std::string to_s (glob_t const& glob);

} /* path */

#endif /* end of include guard: GLOB_H_1FT86H1L */
