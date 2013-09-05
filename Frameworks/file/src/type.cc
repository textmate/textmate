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
				ordering.emplace(-m.end(), *item);
		}
	}
	return ordering.empty() ? NULL_STR : file_type_from_grammars(std::vector<bundles::item_ptr>(1, ordering.begin()->second));
}

static std::string find_file_type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath = NULL_STR)
{
	// FIXME we need to use “current” folder when getting (fileType) settings
	std::string const effectivePath     = virtualPath != NULL_STR ? virtualPath : path;
	std::string const settingsDirectory = path == NULL_STR && virtualPath != NULL_STR ? path::home() : path::parent(path);
	std::string const pathAttributes    = file::path_attributes(effectivePath);

	std::string res = NULL_STR;

	// check if user has an explicit binding for this path (e.g. *.md → Markdown)
	if(res == NULL_STR && effectivePath != NULL_STR)
		res = settings_for_path(effectivePath, pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if a grammar recognize the content (e.g. #!/usr/bin/ruby → Ruby)
	if(res == NULL_STR && bytes)
		res = file_type_from_bytes(bytes);

	// check if a grammar recognize the path extension (.git/config → Git Config)
	if(res == NULL_STR && effectivePath != NULL_STR)
		res = file_type_from_path(effectivePath);

	// check if there is a setting for untitled files (pathAttributes include ‘attr.untitled’)
	if(res == NULL_STR && effectivePath == NULL_STR)
		res = settings_for_path(effectivePath, pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if user has a fallback for unrecognized files (to override the “What is the file type of XXX?” prompt)
	if(res == NULL_STR && effectivePath != NULL_STR)
		res = settings_for_path(effectivePath, "attr.file.unknown-type " + pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if file has no extension, if so, treat it as plain text
	if(res == NULL_STR && path::extension(effectivePath).empty())
		res = "text.plain";

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
		{
			std::string const name = path::name(path);
			std::string const ext  = path::extensions(name);
			settings_t::set(kSettingsFileTypeKey, fileType, NULL_STR, ext.empty() || ext == name ? name : "*." + ext.substr(1));
		}
	}

} /* file */
