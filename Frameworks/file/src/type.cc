#include "type.h"
#include "path_info.h"
#include <settings/settings.h>
#include <bundles/bundles.h>
#include <regexp/regexp.h>
#include <regexp/glob.h>
#include <cf/cf.h>
#include <oak/debug.h>

// =====================
// = File Type Support =
// =====================

static std::string file_type_from_grammars (std::vector<bundles::item_ptr> const& grammars)
{
	for(auto const& grammar : grammars)
	{
		std::string const& scopeName = grammar->value_for_field(bundles::kFieldGrammarScope);
		if(scopeName != NULL_STR)
			return scopeName;
	}
	return NULL_STR;
}

static size_t lines_matched_by_regexp (std::string const& pattern)
{
	size_t newlines = 1;

	bool esc = false;
	for(auto const& ch : pattern)
	{
		if(ch == '\\')
		{
			esc = !esc;
			continue;
		}

		if(esc && ch == 'n' || ch == '\n')
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

static std::vector<bundles::item_ptr> grammars_for_path (std::string const& path)
{
	std::multimap<ssize_t, bundles::item_ptr> ordering;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		for(auto const& ext : item->values_for_field(bundles::kFieldGrammarExtension))
		{
			if(ssize_t rank = path::rank(path, ext))
				ordering.emplace(-rank, item);
		}
	}
	return ordering.empty() ? std::vector<bundles::item_ptr>() : std::vector<bundles::item_ptr>{ ordering.begin()->second };
}

static bool unknown_file_type (std::string const& fileType)
{
	return fileType == NULL_STR || bundles::query(bundles::kFieldGrammarScope, fileType, scope::wildcard, bundles::kItemTypeGrammar).empty();
}

static std::string find_file_type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath = NULL_STR)
{
	// FIXME we need to use “current” folder when getting (fileType) settings
	std::string const effectivePath     = virtualPath != NULL_STR ? virtualPath : path;
	std::string const settingsDirectory = path == NULL_STR && virtualPath != NULL_STR ? path::home() : path::parent(path);
	std::string const pathAttributes    = file::path_attributes(effectivePath);

	std::string res = NULL_STR;

	// check if user has an explicit binding for this path (e.g. *.md → Markdown)
	if(effectivePath != NULL_STR)
		res = settings_for_path(effectivePath, pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if a grammar recognize the content (e.g. #!/usr/bin/ruby → Ruby)
	if(unknown_file_type(res) && bytes)
		res = file::type_from_bytes(bytes);

	// check if a grammar recognize the path extension (.git/config → Git Config)
	if(unknown_file_type(res) && effectivePath != NULL_STR)
		res = file::type_from_path(effectivePath);

	// check if there is a setting for untitled files (pathAttributes include ‘attr.untitled’)
	if(unknown_file_type(res) && effectivePath == NULL_STR)
		res = settings_for_path(effectivePath, pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if user has a fallback for unrecognized files (to override the “What is the file type of XXX?” prompt)
	if(unknown_file_type(res) && effectivePath != NULL_STR)
		res = settings_for_path(effectivePath, "attr.file.unknown-type " + pathAttributes, settingsDirectory).get(kSettingsFileTypeKey, NULL_STR);

	// check if file has no extension, if so, treat it as plain text
	if(unknown_file_type(res) && path::extension(effectivePath).empty())
		res = "text.plain";

	return res;
}

namespace file
{
	std::string type_from_bytes (io::bytes_ptr const& bytes)
	{
		if(!bytes)
			return NULL_STR;

		std::multimap<ssize_t, bundles::item_ptr> ordering;
		for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		{
			for(auto const& pattern : item->values_for_field(bundles::kFieldGrammarFirstLineMatch))
			{
				char const* first = bytes->begin();
				char const* last  = bytes->end();
				if(pattern.find("(?m)") == std::string::npos)
					last = first_n_lines(first, last, lines_matched_by_regexp(pattern));

				if(regexp::match_t const& m = regexp::search(pattern, first, last))
					ordering.emplace(-m.end(), item);
			}
		}
		return ordering.empty() ? NULL_STR : file_type_from_grammars(std::vector<bundles::item_ptr>(1, ordering.begin()->second));
	}

	std::string type_from_path (std::string const& path)
	{
		return path != NULL_STR ? file_type_from_grammars(grammars_for_path(path)) : NULL_STR;
	}

	std::string type (std::string const& path, io::bytes_ptr const& bytes, std::string const& virtualPath)
	{
		return find_file_type(path, bytes, virtualPath);
	}

	void set_type (std::string const& path, std::string const& fileType)
	{
		if(path == NULL_STR || fileType == NULL_STR)
			return;

		std::multimap<ssize_t, std::string> ordering;

		std::string const name = path::name(path);
		std::string const ext  = path::extensions(name);
		if(!ext.empty())
			ordering.emplace(-ext.size(), (name.size() > ext.size() ? "*" : "") + path::glob_t::escape(ext));

		for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
		{
			for(std::string const& suffix : item->values_for_field(bundles::kFieldGrammarExtension))
			{
				std::string::size_type n = path.size() - suffix.size();
				if(path.size() < suffix.size() || path.compare(n, suffix.size(), suffix) != 0 || (n && !strchr("._/", path[n-1])))
					continue;

				if(n && strchr("._", path[n-1]))
					--n;

				ordering.emplace(-(path.size() - n), (n && path[n-1] != '/' ? "*" : "") + path::glob_t::escape(path.substr(n)));
			}
		}

		std::string const glob = ordering.empty() ? name : ordering.begin()->second;
		settings_t::set(kSettingsFileTypeKey, fileType, NULL_STR, glob);
	}

} /* file */
