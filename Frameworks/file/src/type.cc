#include "type.h"
#include "path_info.h"
#include <settings/settings.h>
#include <bundles/bundles.h>
#include <regexp/regexp.h>
#include <cf/cf.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(File_UserBindings);

// =====================
// = File Type Support =
// =====================

namespace
{
	struct user_bindings_t
	{
		user_bindings_t ()
		{
			if(CFPropertyListRef cfPlist = CFPreferencesCopyAppValue(CFSTR("FileTypeBindings"), kCFPreferencesCurrentApplication))
			{
				plist::dictionary_t const& plist = plist::convert(cfPlist);
				D(DBF_File_UserBindings, bug("%s\n", to_s(plist).c_str()););
				CFRelease(cfPlist);
				iterate(pair, plist)
					_bindings.insert(std::make_pair(pair->first, boost::get<std::string>(pair->second)));
			}
		}

		std::string grammar_for (std::string const& path) const
		{
			std::multimap<ssize_t, std::string> ordering;
			iterate(pair, _bindings)
			{
				if(ssize_t rank = path::rank(path, pair->first))
					ordering.insert(std::make_pair(rank, pair->second));
			}
			std::string const res = ordering.empty() ? NULL_STR : ordering.begin()->second;
			D(DBF_File_UserBindings, bug("%s → %s\n", path.c_str(), res == NULL_STR ? "«null»" : res.c_str()););
			return res;
		}

		void learn (std::string const& path, std::string const& grammar)
		{
			D(DBF_File_UserBindings, bug("%s → %s\n", path.c_str(), grammar.c_str()););
			std::string const ext = path::extensions(path);
			_bindings[ext.empty() ? path::name(path) : ext.substr(1)] = grammar;
			save();
		}

		void save ()
		{
			D(DBF_File_UserBindings, bug("\n"););
			CFPreferencesSetAppValue(CFSTR("FileTypeBindings"), cf::wrap(_bindings), kCFPreferencesCurrentApplication);
		}

	private:
		std::map<std::string, std::string> _bindings;
	};
}

static user_bindings_t& user_bindings ()
{
	static user_bindings_t bindings;
	return bindings;
}

static std::string file_type_from_grammars (std::vector<bundles::item_ptr> const& grammars)
{
	iterate(grammar, grammars)
	{
		std::string const& scopeName = (*grammar)->value_for_field(bundles::kFieldGrammarScope);
		if(scopeName != NULL_STR)
			return scopeName;
	}
	return NULL_STR;
}

static std::string file_type_from_path (std::string const& path)
{
	return path != NULL_STR ? file_type_from_grammars(bundles::grammars_for_path(path)) : NULL_STR;
}

static size_t lines_matched_by_regexp (std::string const& pattern)
{
	size_t newlines = 1;

	bool esc = false;
	iterate(ch, pattern)
	{
		if(*ch == '\\')
		{
			esc = !esc;
			continue;
		}

		if(esc && *ch == 'n' || *ch == '\n')
			++newlines;
		esc = false;
	}
	return newlines;
}

template <typename _InputIter>
_InputIter first_n_lines (_InputIter const& first, _InputIter const& last, size_t n)
{
	_InputIter eol = first;
	while(eol != last && n--)
		eol = std::find(eol != first ? ++eol : first, last, '\n');
	return eol;
}

static std::string file_type_from_bytes (io::bytes_ptr const& bytes)
{
	if(!bytes)
		return NULL_STR;

	std::multimap<ssize_t, bundles::item_ptr> ordering;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		citerate(pattern, (*item)->values_for_field(bundles::kFieldGrammarFirstLineMatch))
		{
			char const* first = bytes->begin();
			char const* last  = bytes->end();
			if(pattern->find("(?m)") == std::string::npos)
				last = first_n_lines(first, last, lines_matched_by_regexp(*pattern));

			if(regexp::match_t const& m = regexp::search(*pattern, first, last))
				ordering.insert(std::make_pair(-m.end(), *item));
		}
	}
	return ordering.empty() ? NULL_STR : file_type_from_grammars(std::vector<bundles::item_ptr>(1, ordering.begin()->second));
}

static std::string find_file_type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath = NULL_STR)
{
	// FIXME we need to use “current” folder when getting (fileType) settings
	std::string res = settings_for_path(path, file::path_attributes(path)).get("fileType", NULL_STR);

	if(res == NULL_STR && (virtualPath != NULL_STR || path != NULL_STR))
		res = user_bindings().grammar_for(virtualPath != NULL_STR ? virtualPath : path);

	if(res == NULL_STR && bytes)
		res = file_type_from_bytes(bytes);

	if(res == NULL_STR)
		res = file_type_from_path(virtualPath != NULL_STR ? virtualPath : path);

	if(res == NULL_STR)
		res = settings_for_path(path, "attr.file.unknown-type " + file::path_attributes(path)).get("fileType", NULL_STR);

	return res;
}

namespace file
{
	std::string type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath)
	{
		return find_file_type(path, bytes, virtualPath);
	}

	void set_type (std::string const& path, std::string const& fileType)
	{
		if(path != NULL_STR && fileType != NULL_STR)
			user_bindings().learn(path, fileType);
	}

} /* file */
