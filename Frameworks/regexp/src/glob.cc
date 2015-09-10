#include "glob.h"
#include "parse_glob.h"
#include "parser_base.h"
#include "format_string.h"
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Glob);
OAK_DEBUG_VAR(Glob_Parser);

namespace path
{
	std::string glob_t::escape (std::string const& src)
	{
		return format_string::replace(src, "[\\[\\\\?*{~!]", "\\\\$0");
	}

	void glob_t::setup (std::string const& glob, bool matchDotFiles, bool caseSensitive)
	{
		std::string ptrn = convert_glob_to_regexp(glob, matchDotFiles);
		_compiled = regexp::pattern_t(ptrn, caseSensitive ? ONIG_OPTION_NONE : ONIG_OPTION_IGNORECASE);
		D(DBF_Glob, bug("%s → %s\n", glob.c_str(), ptrn.c_str()););
	}

	bool glob_t::does_match (std::string const& filename) const
	{
		bool res = (bool)regexp::search(_compiled, filename);
		D(DBF_Glob, bug("%s → %s\n", filename.c_str(), BSTR(res)););
		return res;
	}

	std::string to_s (glob_t const& glob)
	{
		return to_s(glob._compiled);
	}

	// ===================
	// = Brace Expansion =
	// ===================

	std::vector<std::string> expand_braces (std::string const& glob)
	{
		return ::expand_braces(glob);
	}

	// ===============
	// = glob_list_t =
	// ===============

	void glob_list_t::add_include_glob (std::string const& glob, kPathItemType itemType)
	{
		if(glob != NULL_STR)
			_globs.emplace_back(false, glob_t(glob, false), itemType);
	}

	void glob_list_t::add_exclude_glob (std::string const& glob, kPathItemType itemType)
	{
		if(glob != NULL_STR)
			_globs.emplace_back(true, glob_t(glob, true), itemType);
	}

	bool glob_list_t::include (std::string const& path, kPathItemType itemType, bool defaultResult) const
	{
		return !exclude(path, itemType, !defaultResult);
	}

	bool glob_list_t::exclude (std::string const& path, kPathItemType itemType, bool defaultResult) const
	{
		if(_globs.empty())
			return false;

		for(auto record : _globs)
		{
			if((itemType == kPathItemAny || record.item_type == kPathItemAny || itemType == record.item_type) && record.glob.does_match(path))
				return record.negate;
		}
		return defaultResult;
	}

} /* path */
