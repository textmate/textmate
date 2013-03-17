#include "fs_controller.h"
#include <plist/plist.h>
#include <plist/stl.h>
#include <cf/timer.h>
#include <io/events.h>

namespace fs
{
	static char const* kDefaultDeviceUUID = "CDE933DC-5C68-407C-964F-22FFC8EB87A7";

	struct directory_info_t
	{
		directory_info_t (std::string const& path, uint64_t eventId) : _device_uuid(kDefaultDeviceUUID), _inode(0), _event_id(eventId)
		{
			struct stat buf;
			if(stat(path.c_str(), &buf) == 0)
			{
				_inode = buf.st_ino;
				if(CFUUIDRef uuidRef = FSEventsCopyUUIDForDevice(buf.st_dev))
				{
					_device_uuid = CFUUIDGetUUIDBytes(uuidRef);
					CFRelease(uuidRef);
				}
			}
		}

		oak::uuid_t _device_uuid;
		ino_t _inode;
		uint64_t _event_id;
	};

	plist::dictionary_t to_plist (directory_info_t const& dirInfo)
	{
		plist::dictionary_t res;
		res["deviceIdentifier"] = to_s(dirInfo._device_uuid);
		res["directoryNode"]    = uint64_t(dirInfo._inode);
		res["lastEvent"]        = dirInfo._event_id;
		return res;
	}

	struct watch_info_t : fs::event_callback_t
	{
		watch_info_t (std::set<std::string> rootPaths, callback_t* callback, path::glob_t const& dirGlob, path::glob_t const& fileGlob, std::string const& cacheFile) : _in_replay(false), _callback(callback), _dir_glob(dirGlob), _file_glob(fileGlob), _cache_file(cacheFile)
		{
			plist::dictionary_t oldEventIds;
			std::map<std::string, node_t> oldHeads;

			citerate(pair, plist::load(_cache_file))
			{
				if(pair->first == "fsEventIds")
				{
					if(plist::dictionary_t* tmp = boost::get<plist::dictionary_t>(&pair->second))
						oldEventIds = *tmp;
				}
				else if(pair->first == "fsTrees")
				{
					if(plist::dictionary_t* tmp = boost::get<plist::dictionary_t>(&pair->second))
					{
						iterate(node, *tmp)
							oldHeads.insert(std::make_pair(node->first, from_plist(node->second)));
					}
				}
			}

			while(!rootPaths.empty())
			{
				std::map<std::string, node_t> newHeads;
				iterate(path, rootPaths)
				{
					if(_heads.find(*path) != _heads.end())
						continue;

					directory_info_t dirInfo(*path, FSEventsGetCurrentEventId());

					std::map<std::string, node_t>::iterator oldNode = oldHeads.find(*path);
					if(oldNode != oldHeads.end())
					{
						oak::uuid_t oldDeviceId;
						uint64_t oldDirNode, oldEventId;

						if(plist::get_key_path(oldEventIds, *path + ".deviceIdentifier", oldDeviceId) && plist::get_key_path(oldEventIds, *path + ".directoryNode", oldDirNode) && plist::get_key_path(oldEventIds, *path + ".lastEvent", oldEventId) && oldDeviceId == dirInfo._device_uuid && oldDirNode == dirInfo._inode)
						{
							dirInfo._event_id = oldEventId;
							_event_ids.insert(std::make_pair(*path, dirInfo));
							newHeads.insert(*oldNode);
							oldHeads.erase(oldNode);
							continue;
						}
					}

					_event_ids.insert(std::make_pair(*path, dirInfo));
					newHeads.insert(std::make_pair(*path, node_t(*path).rescan(path::parent(*path), _dir_glob, _file_glob)));
				}
				rootPaths.clear();

				iterate(node, newHeads)
					collect_links(node->second, path::parent(node->first), rootPaths);

				_heads.insert(newHeads.begin(), newHeads.end());
			}

			// ===================
			// = Setup FS Events =
			// ===================

			std::string lastDir = NULL_STR;
			iterate(pair, _event_ids)
			{
				if(pair->first.find(lastDir) == 0)
					continue;
				lastDir = pair->first;
				fs::watch(pair->first, this, pair->second._event_id);
			}

			save();
		}

		void set_replaying_history (bool flag, std::string const& observedPath, uint64_t eventId)
		{
			_in_replay = flag;
			update_event_id(observedPath, eventId);
			schedule_save();
		}

		void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive)
		{
			std::set<std::string> linksBefore, linksAfter;
			iterate(pair, _heads)
			{
				if(path.find(pair->first) == 0)
				{
					collect_links(pair->second, path::parent(pair->first), linksBefore);
					rescan(path::parent(pair->first), path, pair->second);
					collect_links(pair->second, path::parent(pair->first), linksAfter);
				}
			}

			std::vector<std::string> linksAdded, linksRemoved;
			std::set_difference(linksBefore.begin(), linksBefore.end(), linksAfter.begin(), linksAfter.end(), back_inserter(linksRemoved));
			std::set_difference(linksAfter.begin(), linksAfter.end(), linksBefore.begin(), linksBefore.end(), back_inserter(linksAdded));

			iterate(link, linksRemoved)
			{
				if(_heads.find(*link) == _heads.end())
					continue;

				_heads.erase(*link);
				_event_ids.erase(*link);
				fs::unwatch(*link, this);
			}

			iterate(link, linksAdded)
			{
				if(_heads.find(*link) != _heads.end())
					continue;

				directory_info_t dirInfo(*link, FSEventsGetCurrentEventId());
				_event_ids.insert(std::make_pair(*link, dirInfo));
				_heads.insert(std::make_pair(*link, node_t(*link).rescan(path::parent(*link), _dir_glob, _file_glob)));
				fs::watch(*link, this, dirInfo._event_id);
			}

			update_event_id(observedPath, eventId, path);
			schedule_save();
		}

	private:
		static void collect_links (node_t const& node, std::string const& cwd, std::set<std::string>& res)
		{
			if(node.type() == node_t::kNodeTypeDirectory)
			{
				citerate(it, *node.entries())
					collect_links(*it, node.path(cwd), res);
			}
			else if(node.type() == node_t::kNodeTypeLink)
			{
				res.insert(node.real_path(cwd));
			}
		}

		void update_event_id (std::string const& path, uint64_t eventId, std::string const& changedPath = NULL_STR)
		{
			std::map<std::string, fs::directory_info_t>::iterator it = _event_ids.find(path);
			if(it == _event_ids.end())
			{
				fprintf(stderr, "*** no event ID for ‘%s’ (changed path ‘%s’)\n", path.c_str(), changedPath.c_str());
				return;
			}

			if(changedPath == NULL_STR)
			{
				it->second = directory_info_t(it->first, eventId);
			}
			else
			{
				iterate(it, _event_ids)
				{
					if(changedPath.find(it->first) == 0)
						it->second = directory_info_t(it->first, eventId);
				}
			}
		}

		void save ()
		{
			_save_timer.reset();

			plist::dictionary_t cache;

			cache["fsEventIds"] = plist::dictionary_t();
			plist::dictionary_t& eventInfoPlist = boost::get<plist::dictionary_t>(cache["fsEventIds"]);
			iterate(dirInfo, _event_ids)
				eventInfoPlist.insert(std::make_pair(dirInfo->first, to_plist(dirInfo->second)));

			cache["fsTrees"]    = plist::dictionary_t();
			plist::dictionary_t& headsPlist = boost::get<plist::dictionary_t>(cache["fsTrees"]);
			iterate(node, _heads)
				headsPlist.insert(std::make_pair(node->first, to_plist(node->second)));

			plist::save(_cache_file, cache);

			_callback->did_change(_heads, _changes);
			_changes.clear();
		}

		void schedule_save ()
		{
			_save_timer = cf::setup_timer(1, std::bind(&watch_info_t::save, this));
		}

		void rescan (std::string cwd, std::string const& path, fs::node_t& node)
		{
			cwd = node.path(cwd);
			if(cwd == path)
			{
				fs::node_t newNode(cwd);
				if(node._type != newNode._type || node._modified != newNode._modified)
				{
					if(node._type == fs::node_t::kNodeTypeMissing)         _changes["created"].push_back(cwd);
					else if(newNode._type == fs::node_t::kNodeTypeMissing) _changes["deleted"].push_back(cwd);
					else                                                   _changes["changes"].push_back(cwd);

					if(node._type == newNode._type && node._type == fs::node_t::kNodeTypeDirectory)
						node._entries.swap(newNode._entries);
					node = newNode;
				}
				node.rescan(path::parent(cwd), _dir_glob, _file_glob, &_changes);
			}
			else if(node.type() == fs::node_t::kNodeTypeDirectory)
			{
				std::string const& toBeFound = path::relative_to(path, cwd);
				iterate(n, *node.entries())
				{
					if(toBeFound.find(n->name()) == 0)
						rescan(cwd, path, *n);
				}
			}
		}

		bool _in_replay;
		fs::callback_t* _callback;
		path::glob_t _dir_glob;
		path::glob_t _file_glob;
		std::string _cache_file;

		std::map<std::string, fs::node_t> _heads;
		std::map<std::string, fs::directory_info_t> _event_ids;
		cf::timer_ptr _save_timer;
		std::map< std::string, std::vector<std::string> > _changes;
	};

	watch_info_ptr watch_paths (std::set<std::string> rootPaths, callback_t* callback, path::glob_t const& dirGlob, path::glob_t const& fileGlob, std::string const& cacheFile)
	{
		return watch_info_ptr(new watch_info_t(rootPaths, callback, dirGlob, fileGlob, cacheFile == NULL_STR ? "/tmp/fs_cache.plist" : cacheFile));
	}

} /* fs */
