#include "fs_cache.h"
#include <io/entries.h>
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Plist_Cache);

static std::string read_link (std::string const& path)
{
	D(DBF_Plist_Cache, bug("%s\n", path.c_str()););

	char buf[PATH_MAX];
	ssize_t len = readlink(path.c_str(), buf, sizeof(buf));
	if(0 < len && len < PATH_MAX)
	{
		return std::string(buf, buf + len);
	}
	else
	{
		std::string errStr = len == -1 ? strerror(errno) : text::format("Result outside allowed range %zd", len);
		fprintf(stderr, "*** readlink(‘%s’) failed: %s\n", path.c_str(), errStr.c_str());
	}
	return NULL_STR;
}

namespace plist
{
	int32_t const cache_t::kPropertyCacheFormatVersion = 2;

	void cache_t::load (std::string const& path, plist::dictionary_t (*prune_dictionary)(plist::dictionary_t const&))
	{
		_prune_dictionary = prune_dictionary;

		int32_t version;
		auto plist = plist::load(path);
		if(plist::get_key_path(plist, "version", version) && version == kPropertyCacheFormatVersion)
		{
			for(auto pair : plist)
			{
				if(plist::dictionary_t const* node = boost::get<plist::dictionary_t>(&pair.second))
				{
					entry_type_t type = entry_type_t::directory;
					plist::dictionary_t content;
					std::string link = NULL_STR;
					bool flag;

					if(plist::get_key_path(*node, "content", content))
						type = entry_type_t::file;
					else if(plist::get_key_path(*node, "link", link))
						type = entry_type_t::link;
					else if(plist::get_key_path(*node, "missing", flag) && flag)
						type = entry_type_t::missing;

					entry_t entry(pair.first, type, link);
					entry.set_content(content);
					oak::date_t modified;
					if(plist::get_key_path(*node, "modified", modified))
						entry.set_modified(modified.time_value());

					plist::array_t array;
					std::string globString;
					if(plist::get_key_path(*node, "entries", array) && plist::get_key_path(*node, "glob", globString))
					{
						std::vector<std::string> v;
						for(auto path : array)
							v.push_back(boost::get<std::string>(path));
						entry.set_entries(v, globString);
					}

					uint64_t eventId;
					if(plist::get_key_path(*node, "eventId", eventId))
						entry.set_event_id(eventId);

					_cache.emplace(pair.first, entry);
				}
			}
		}
	}

	void cache_t::save (std::string const& path) const
	{
		D(DBF_Plist_Cache, bug("%s\n", path.c_str()););

		plist::dictionary_t plist;
		for(auto pair : _cache)
		{
			plist::dictionary_t node;

			plist::array_t entries;
			for(auto path : pair.second.entries())
				entries.push_back(path);

			if(pair.second.is_file())
			{
				node["content"] = pair.second.content();
				node["modified"] = oak::date_t(pair.second.modified());
			}
			else if(pair.second.is_link())
				node["link"] = pair.second.link();
			else if(pair.second.is_missing())
				node["missing"] = true;
			else if(pair.second.is_directory())
			{
				node["entries"] = entries;
				node["glob"] = pair.second.glob_string();
				if(pair.second.event_id())
					node["eventId"] = pair.second.event_id();
			}

			plist.emplace(pair.first, node);
		}
		plist["version"] = kPropertyCacheFormatVersion;
		plist::save(path, plist);
	}

	uint64_t cache_t::event_id_for_path (std::string const& path) const
	{
		auto it = _cache.find(path);
		return it == _cache.end() ? 0 : it->second.event_id();
	}

	void cache_t::set_event_id_for_path (uint64_t eventId, std::string const& path)
	{
		auto it = _cache.find(path);
		if(it != _cache.end() && it->second.event_id() != eventId)
		{
			it->second.set_event_id(eventId);
			_dirty = true;
		}
	}

	plist::dictionary_t cache_t::content (std::string const& path)
	{
		auto it = _cache.find(path);
		if(it != _cache.end() && it->second.type() == entry_type_t::missing)
		{
			fprintf(stderr, "content requested for missing item: ‘%s’\n", path.c_str());
			_cache.erase(it);
		}
		return resolved(path).content();
	}

	std::vector<std::string> cache_t::entries (std::string const& path, std::string const& globString)
	{
		entry_t& entry = resolved(path, globString);

		std::vector<std::string> res;
		for(auto path : entry.entries())
			res.emplace_back(path::join(entry.path(), path));
		return res;
	}

	bool cache_t::erase (std::string const& path)
	{
		auto first = _cache.find(path);
		D(DBF_Plist_Cache, bug("%s (in cache %s)\n", path.c_str(), BSTR(first != _cache.end())););
		if(first == _cache.end())
			return false;

		if(first->second.is_directory())
		{
			auto parent = _cache.find(path::parent(path));
			if(parent != _cache.end() && parent->second.is_directory())
			{
				D(DBF_Plist_Cache, bug("remove from parent (%s)\n", parent->first.c_str()););
				std::vector<std::string> entries = parent->second.entries();
				auto name = std::find(entries.begin(), entries.end(), path::name(path));
				if(name != entries.end())
				{
					entries.erase(name);
					parent->second.set_entries(entries, parent->second.glob_string());
				}
			}
			_cache.erase(first, _cache.lower_bound(path + "0")); // path + "0" is the first non-descendent
		}
		else
		{
			_cache.erase(first);
		}

		_dirty = true;
		return true;
	}

	bool cache_t::reload (std::string const& path, bool recursive)
	{
		bool dirty = false;
		auto it = _cache.find(path);
		if(it == _cache.end())
		{
			D(DBF_Plist_Cache, bug("no entry for ‘%s’\n", path.c_str()););
			return dirty;
		}

		struct stat buf;
		D(DBF_Plist_Cache, bug("lstat ‘%s’\n", path.c_str()););
		if(lstat(path.c_str(), &buf) == 0)
		{
			if(S_ISDIR(buf.st_mode) && it->second.is_directory())
			{
				auto oldEntries = recursive ? std::vector<std::string>() : it->second.entries();
				update_entries(it->second, it->second.glob_string());
				auto newEntries = it->second.entries();
				dirty = oldEntries != newEntries;
				D(DBF_Plist_Cache, bug("entries changed ‘%s’\n", BSTR(oldEntries != newEntries)););
				for(auto name : newEntries)
				{
					auto entryIter = _cache.find(path::join(path, name));
					if(entryIter != _cache.end() && (entryIter->second.is_file() || recursive))
						dirty = reload(path::join(path, name), recursive) || dirty;
				}
			}
			else if(!(it->second.is_file() && S_ISREG(buf.st_mode) && it->second.modified() == buf.st_mtimespec.tv_sec))
			{
				D(DBF_Plist_Cache, bug("erase ‘%s’ (path changed)\n", path.c_str()););
				_cache.erase(it);
				dirty = true;
			}
		}
		else if(!it->second.is_missing())
		{
			D(DBF_Plist_Cache, bug("erase ‘%s’ (path missing)\n", path.c_str()););
			_cache.erase(it);
			dirty = true;
		}

		_dirty = _dirty || dirty;
		return dirty;
	}

	bool cache_t::cleanup (std::vector<std::string> const& rootPaths)
	{
		std::set<std::string> allPaths, reachablePaths;
		std::transform(_cache.begin(), _cache.end(), std::inserter(allPaths, allPaths.end()), [](std::pair<std::string, entry_t> const& pair){ return pair.first; });
		for(auto path : rootPaths)
			copy_all(path, std::inserter(reachablePaths, reachablePaths.end()));

		std::vector<std::string> toRemove;
		std::set_difference(allPaths.begin(), allPaths.end(), reachablePaths.begin(), reachablePaths.end(), back_inserter(toRemove));

		D(DBF_Plist_Cache, if(!toRemove.empty()) bug("drop:\n - %s\n", text::join(toRemove, "\n - ").c_str()););
		for(auto path : toRemove)
			_cache.erase(path);
		_dirty = _dirty || !toRemove.empty();
		return !toRemove.empty();
	}

	// ============================
	// = Private Member Functions =
	// ============================

	cache_t::entry_t& cache_t::resolved (std::string const& path, std::string const& globString)
	{
		auto it = _cache.find(path);
		if(it == _cache.end())
		{
			entry_type_t type = entry_type_t::missing;

			struct stat buf;
			D(DBF_Plist_Cache, bug("lstat ‘%s’\n", path.c_str()););
			if(lstat(path.c_str(), &buf) == 0)
			{
				if(S_ISREG(buf.st_mode))
					type = entry_type_t::file;
				else if(S_ISLNK(buf.st_mode))
					type = entry_type_t::link;
				else if(S_ISDIR(buf.st_mode))
					type = entry_type_t::directory;
			}

			entry_t entry(path, type, type == entry_type_t::link ? read_link(path) : NULL_STR);
			if(entry.is_file())
			{
				D(DBF_Plist_Cache, bug("load ‘%s’\n", path.c_str()););
				entry.set_content(_prune_dictionary(plist::load(path)));
				entry.set_modified(buf.st_mtimespec.tv_sec);
			}
			else if(entry.is_directory())
			{
				update_entries(entry, globString);
			}

			it = _cache.emplace(path, entry).first;
			_dirty = true;
		}
		return it->second.is_link() ? resolved(it->second.resolved(), globString) : it->second;
	}

	void cache_t::update_entries (entry_t& entry, std::string const& globString)
	{
		D(DBF_Plist_Cache, bug("scan-dir ‘%s’\n", entry.path().c_str()););
		std::vector<std::string> entries;
		for(auto dirEntry : path::entries(entry.path(), globString))
			entries.emplace_back(dirEntry->d_name);
		std::sort(entries.begin(), entries.end());
		entry.set_entries(entries, globString);
	}

} /* plist */
