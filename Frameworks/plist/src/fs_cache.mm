#include "fs_cache.h"
#import <Foundation/Foundation.h>
#include <io/entries.h>
#include <text/format.h>
#include <oak/debug.h>

static std::string read_link (std::string const& path)
{
	char buf[PATH_MAX];
	ssize_t len = readlink(path.c_str(), buf, sizeof(buf));
	if(0 < len && len < PATH_MAX)
	{
		return std::string(buf, buf + len);
	}
	else
	{
		std::string errStr = len == -1 ? strerror(errno) : text::format("Result outside allowed range %zd", len);
		os_log_error(OS_LOG_DEFAULT, "readlink(\"%{public}s\"): %{public}s", path.c_str(), errStr.c_str());
	}
	return NULL_STR;
}

namespace plist
{
	void cache_t::load (std::string const& path)
	{
		try {
			real_load(path);
		}
		catch(std::exception const& e) {
			os_log_error(OS_LOG_DEFAULT, "Exception thrown while loading ‘%{public}s’: %{public}s", path.c_str(), e.what());
		}
	}

	void cache_t::real_load (std::string const& path)
	{
		NSData* data = [NSData dataWithContentsOfFile:@(path.c_str())];
		if(!data)
			return;

		NSError* error = nil;
		NSSet* classes = [NSSet setWithObjects:NSDictionary.class, NSString.class, NSNumber.class, NSData.class, NSArray.class, nil];
		NSDictionary* root = [NSKeyedUnarchiver unarchivedObjectOfClasses:classes fromData:data error:&error];
		if(!root)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to load ‘%{public}s’: %{public}@", path.c_str(), error);
			return;
		}
		if([root[@"version"] unsignedIntegerValue] != 2)
			return;

		NSDictionary* entries = root[@"entries"];
		for(NSString* pathKey in entries)
		{
			NSDictionary* node = entries[pathKey];
			entry_t entry(pathKey.UTF8String);

			NSString* type = node[@"type"];
			if([type isEqualToString:@"file"])
			{
				entry.set_type(entry_type_t::file);
				entry.set_modified([node[@"modified"] unsignedLongLongValue]);

				plist::dictionary_t plist;
				NSDictionary* content = node[@"content"];
				for(NSString* key in content)
				{
					id value = content[key];
					if([value isKindOfClass:NSString.class])
					{
						plist.emplace(key.UTF8String, std::string(((NSString*)value).UTF8String));
					}
					else if([value isKindOfClass:NSData.class])
					{
						plist.emplace(key.UTF8String, plist::parse(std::string((char const*)((NSData*)value).bytes, ((NSData*)value).length)));
					}
				}
				entry.set_content(plist);
			}
			else if([type isEqualToString:@"directory"])
			{
				entry.set_type(entry_type_t::directory);
				entry.set_event_id([node[@"eventId"] unsignedLongLongValue]);
				entry.set_glob_string(((NSString*)node[@"glob"]).UTF8String ?: "");

				std::vector<std::string> v;
				for(NSString* item in node[@"items"])
					v.push_back(item.UTF8String);
				entry.set_entries(v);
			}
			else if([type isEqualToString:@"link"])
			{
				entry.set_type(entry_type_t::link);
				// .UTF8String on nil returns NULL, which crashes std::string construction
			NSString* link = node[@"link"];
			if(link)
				entry.set_link(link.UTF8String);
			else
				continue;
			}
			else if([type isEqualToString:@"missing"])
			{
				entry.set_type(entry_type_t::missing);
			}

			if(entry.type() != entry_type_t::unknown)
				_cache.emplace(pathKey.UTF8String, entry);
		}
	}

	void cache_t::save (std::string const& path) const
	{
		NSMutableDictionary* entries = [NSMutableDictionary dictionary];
		for(auto const& pair : _cache)
		{
			NSMutableDictionary* node = [NSMutableDictionary dictionary];
			auto const& e = pair.second;

			if(e.is_file())
			{
				node[@"type"] = @"file";
				node[@"modified"] = @(e.modified());

				NSMutableDictionary* content = [NSMutableDictionary dictionary];
				for(auto const& kv : e.content())
				{
					if(std::string const* str = ::plist::get<std::string>(&kv.second))
					{
						content[@(kv.first.c_str())] = @(str->c_str());
					}
					else
					{
						if(CFPropertyListRef cfPlist = plist::create_cf_property_list(kv.second))
						{
							if(CFDataRef cfData = CFPropertyListCreateData(kCFAllocatorDefault, cfPlist, kCFPropertyListBinaryFormat_v1_0, 0, nullptr))
							{
								content[@(kv.first.c_str())] = (__bridge_transfer NSData*)cfData;
							}
							CFRelease(cfPlist);
						}
					}
				}
				node[@"content"] = content;
			}
			else if(e.is_directory())
			{
				node[@"type"] = @"directory";
				node[@"glob"] = @(e.glob_string().c_str());
				node[@"eventId"] = @(e.event_id());

				NSMutableArray* items = [NSMutableArray array];
				for(auto const& s : e.entries())
					[items addObject:@(s.c_str())];
				node[@"items"] = items;
			}
			else if(e.is_link())
			{
				node[@"type"] = @"link";
				node[@"link"] = @(e.link().c_str());
			}
			else if(e.is_missing())
			{
				node[@"type"] = @"missing";
			}

			entries[@(pair.first.c_str())] = node;
		}

		NSDictionary* root = @{ @"version": @2, @"entries": entries };
		NSError* error = nil;
		NSData* data = [NSKeyedArchiver archivedDataWithRootObject:root requiringSecureCoding:YES error:&error];
		if(!data)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to save '%{public}s': %{public}@", path.c_str(), error);
			return;
		}
		[data writeToFile:@(path.c_str()) atomically:YES];
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
			os_log_error(OS_LOG_DEFAULT, "Content requested for missing item: ‘%{public}s’", path.c_str());
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
		if(first == _cache.end())
			return false;

		if(first->second.is_directory())
		{
			auto parent = _cache.find(path::parent(path));
			if(parent != _cache.end() && parent->second.is_directory())
			{
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
			return path::is_absolute(path) && path != "/" ? reload(path::parent(path), recursive) : dirty;

		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			if(S_ISDIR(buf.st_mode) && it->second.is_directory())
			{
				auto oldEntries = recursive ? std::vector<std::string>() : it->second.entries();
				update_entries(it->second, it->second.glob_string());
				auto newEntries = it->second.entries();
				dirty = oldEntries != newEntries;
				for(auto name : newEntries)
				{
					auto entryIter = _cache.find(path::join(path, name));
					if(entryIter != _cache.end() && (entryIter->second.is_file() || recursive))
						dirty = reload(path::join(path, name), recursive) || dirty;
				}
			}
			else if(!(it->second.is_file() && S_ISREG(buf.st_mode) && it->second.modified() == buf.st_mtimespec.tv_sec))
			{
				_cache.erase(it);
				dirty = true;
			}
		}
		else if(!it->second.is_missing())
		{
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
			entry_t entry(path);
			entry.set_type(entry_type_t::missing);

			struct stat buf;
			if(lstat(path.c_str(), &buf) == 0)
			{
				if(S_ISREG(buf.st_mode))
				{
					entry.set_type(entry_type_t::file);
				}
				else if(S_ISLNK(buf.st_mode))
				{
					entry.set_type(entry_type_t::link);
					entry.set_link(read_link(path));
				}
				else if(S_ISDIR(buf.st_mode))
				{
					entry.set_type(entry_type_t::directory);
				}
			}

			if(entry.is_file())
			{
				auto const content = plist::load(path);
				entry.set_content(_prune_dictionary ? _prune_dictionary(content) : content);
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
		std::vector<std::string> entries;
		for(auto dirEntry : path::entries(entry.path(), globString))
			entries.emplace_back(dirEntry->d_name);
		std::sort(entries.begin(), entries.end());
		entry.set_entries(entries, globString);
	}

} /* plist */
