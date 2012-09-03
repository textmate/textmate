#include "drivers/api.h"
#include "scm.h"
#include "server.h"
#include <io/path.h>
#include <cf/cf.h>
#include <oak/oak.h>
#include <text/format.h>
#include <settings/settings.h>

OAK_DEBUG_VAR(SCM);

namespace scm
{
	static void callback_function (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[]);

	struct watcher_t
	{
		watcher_t (std::string const& path, info_t* info) : path(path), info(info), stream(NULL)
		{
			struct statfs buf;
			if(statfs(path.c_str(), &buf) != 0)
				return;

			mount_point            = buf.f_mntonname;
			dev_t device           = buf.f_fsid.val[0];
			std::string devicePath = path::relative_to(path, mount_point);

			FSEventStreamContext contextInfo = { 0, this, NULL, NULL, NULL };
			if(stream = FSEventStreamCreateRelativeToDevice(kCFAllocatorDefault, &callback_function, &contextInfo, device, cf::wrap(std::vector<std::string>(1, devicePath)), kFSEventStreamEventIdSinceNow, 1.0, kFSEventStreamCreateFlagNone))
			{
				FSEventStreamScheduleWithRunLoop(stream, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
				FSEventStreamStart(stream);
				FSEventStreamFlushSync(stream);
			}
			else
			{
				fprintf(stderr, "can’t observe ‘%s’\n", path.c_str());
			}
		}

		void callback (std::set<std::string> const& changedPaths)
		{
			info->callback(changedPaths);
		}

		~watcher_t ()
		{
			if(!stream)
				return;

			FSEventStreamStop(stream);
			FSEventStreamInvalidate(stream);
			FSEventStreamRelease(stream);
		}

		std::string path;
		info_t* info;

		std::string mount_point;
		FSEventStreamRef stream;
	};

	static void callback_function (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[])
	{
		watcher_t& watcher = *(watcher_t*)clientCallBackInfo;

		std::set<std::string> changedPaths;

		for(size_t i = 0; i < numEvents; ++i)
		{
			std::string const& file = ((char const* const*)eventPaths)[i];
			std::string const& path = path::join(watcher.mount_point, "./" + file);
			changedPaths.insert(path);
		}
		watcher.callback(changedPaths);
	}
}

namespace scm
{
	static std::map<std::string, info_ptr>& cache () { static std::map<std::string, info_ptr> cache; return cache; }

	// ==========
	// = info_t =
	// ==========

	info_t::info_t (std::string const& wcPath, driver_t const* driver) : _wc_path(wcPath), _driver(driver)
	{
		ASSERTF(path::is_directory(_wc_path) || !path::exists(_wc_path) || _wc_path == NULL_STR, "Path: %s\n", _wc_path.c_str());
	}

	std::string info_t::scm_name () const    { return _driver->name(); }
	std::string info_t::path () const        { return _wc_path; }
	std::string info_t::branch () const      { return _driver->branch_name(_wc_path); }
	bool info_t::tracks_directories () const { return _driver->tracks_directories(); }

	status::type info_t::status (std::string const& path)
	{
		D(DBF_SCM, bug("%s\n", path.c_str()););
		if(_file_status.empty())
			_file_status = _driver->status(_wc_path);

		if(!_watcher)
			_watcher.reset(new scm::watcher_t(_wc_path, this));

		status_map_t::const_iterator res = _file_status.find(path);
		return res != _file_status.end() ? res->second : status::none;
	}

	status_map_t info_t::files_with_status (int mask)
	{
		if(_file_status.empty())
			_file_status = _driver->status(path());

		if(!_watcher)
			_watcher.reset(new scm::watcher_t(_wc_path, this));

		status_map_t res;
		iterate(status, _file_status)
		{
			if((status->second & mask) == status->second)
				res.insert(*status);
		}
		return res;
	}

	// ====================
	// = Callback Related =
	// ====================

	void info_t::add_callback (callback_t* cb)
	{
		D(DBF_SCM, bug("%s / %p\n", path().c_str(), cb););
		_callbacks.add(cb);
		cb->status_changed(*this, std::set<std::string>());
	}

	void info_t::remove_callback (callback_t* cb)
	{
		_callbacks.remove(cb);
	}

	void info_t::callback (std::set<std::string> const& pathsChangedOnDisk)
	{
		D(DBF_SCM, bug("( %s )\n", text::join(pathsChangedOnDisk, ", ").c_str()););
		background_status(_wc_path, _driver, _updated, _snapshot, &update_status);
	}

	void info_t::update_status (bool didUpdate, std::string const& path, fs::snapshot_t const& snapshot, scm::status_map_t const& newStatus)
	{
		if(!didUpdate)
			return;

		auto it = cache().find(path);
		if(it != cache().end())
		{
			std::set<std::string> changedPaths;

			auto const oldStatus = it->second->_file_status;
			auto oldStatusIter = oldStatus.begin();
			auto newStatusIter = newStatus.begin();

			while(oldStatusIter != oldStatus.end() && newStatusIter != newStatus.end())
			{
				if(newStatusIter == newStatus.end() || oldStatusIter->first < newStatusIter->first)
				{
					changedPaths.insert(oldStatusIter->first);
					++oldStatusIter;
					continue;
				}

				if(oldStatusIter == oldStatus.end() || newStatusIter->first < oldStatusIter->first)
				{
					changedPaths.insert(newStatusIter->first);
					++newStatusIter;
					continue;
				}

				if(oldStatusIter->second != newStatusIter->second)
					changedPaths.insert(newStatusIter->first);

				++oldStatusIter;
				++newStatusIter;
			}

			it->second->_updated     = oak::date_t::now();
			it->second->_snapshot    = snapshot;
			it->second->_file_status = newStatus;

			it->second->_callbacks(&callback_t::status_changed, *it->second, changedPaths);
		}
	}

	// ==============
	// = Public API =
	// ==============

	info_ptr info (std::string const& path)
	{
		if(!settings_for_path(path).get(kSettingsSCMStatusKey, true))
			return info_ptr();

		std::string wcPath;
		if(driver_t const* driver = driver_for_path(path::parent(path), &wcPath))
		{
			if(wcPath == NULL_STR || wcPath == "/" || wcPath == path::home() || !path::is_local(wcPath))
				return info_ptr();

			std::map<std::string, info_ptr>::iterator it = cache().find(wcPath);
			if(it == cache().end())
				it = cache().insert(std::make_pair(wcPath, info_ptr(new info_t(wcPath, driver)))).first;
			return it->second;
		}
		return info_ptr();
	}

	status_map_t tracked_files (std::string const& dir, int mask)
	{
		if(info_ptr const& driver = info(path::join(dir, ".scm-kludge")))
			return driver->files_with_status(mask);
		return status_map_t();
	}
}
