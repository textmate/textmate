#include "drivers/api.h"
#include "scm.h"
#include "fs_events.h"
#include <io/path.h>
#include <cf/cf.h>
#include <oak/oak.h>
#include <text/format.h>
#include <settings/settings.h>

OAK_DEBUG_VAR(SCM);

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

	std::string info_t::scm_name () const    { auto vars = variables(); auto iter = vars.find("TM_SCM_NAME"); return iter != vars.end() ? iter->second : NULL_STR; }
	std::string info_t::path () const        { return _wc_path; }
	std::string info_t::branch () const      { auto vars = variables(); auto iter = vars.find("TM_SCM_BRANCH"); return iter != vars.end() ? iter->second : NULL_STR; }
	bool info_t::tracks_directories () const { return _driver->tracks_directories(); }

	void info_t::setup ()
	{
		if(_did_setup)
			return;

		_file_status = _driver->status(_wc_path);
		_variables = std::map<std::string, std::string>{
			{ "TM_SCM_NAME",   _driver->name() },
			{ "TM_SCM_BRANCH", _driver->branch_name(_wc_path) }
		};
		_watcher.reset(new scm::watcher_t(_wc_path, this));
		_did_setup = true;
	}

	status::type info_t::status (std::string const& path)
	{
		D(DBF_SCM, bug("%s\n", path.c_str()););
		setup();
		status_map_t::const_iterator res = _file_status.find(path);
		return res != _file_status.end() ? res->second : status::none;
	}

	std::map<std::string, std::string> info_t::variables () const
	{
		const_cast<info_t*>(this)->setup();
		return _variables;
	}

	status_map_t info_t::files_with_status (int mask)
	{
		setup();

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

	static bool test_and_set (std::string const& key, bool flag)
	{
		static dispatch_queue_t queue = dispatch_queue_create("org.textmate.scm.coordination", DISPATCH_QUEUE_SERIAL);
		static std::set<std::string> keys;

		__block bool foundKey = false;
		dispatch_sync(queue, ^{
			auto it = keys.find(key);
			foundKey = it != keys.end();
			if(flag && !foundKey)
				keys.insert(key);
			else if(!flag && foundKey)
				keys.erase(it);
		});
		return foundKey;
	}

	void info_t::callback (std::set<std::string> const& pathsChangedOnDisk)
	{
		D(DBF_SCM, bug("( %s )\n", text::join(pathsChangedOnDisk, ", ").c_str()););
		if(test_and_set(_wc_path, true))
			return;

		static dispatch_queue_t queue = dispatch_queue_create("org.textmate.scm.status", DISPATCH_QUEUE_SERIAL);
		dispatch_async(queue, ^{
			double elapsed = oak::date_t::now() - _updated;
			if(elapsed < 3)
				usleep((3 - elapsed) * 1000000);

			bool shouldCheck = !_driver->may_touch_filesystem() || _snapshot != fs::snapshot_t(_wc_path);
			test_and_set(_wc_path, false);
			if(shouldCheck)
			{
				scm::status_map_t const status = _driver->status(_wc_path);
				std::map<std::string, std::string> const variables{
					{ "TM_SCM_NAME",   _driver->name() },
					{ "TM_SCM_BRANCH", _driver->branch_name(_wc_path) }
				};
				fs::snapshot_t const snapshot = _driver->may_touch_filesystem() ? fs::snapshot_t(_wc_path) : fs::snapshot_t();
				dispatch_async(dispatch_get_main_queue(), ^{
					update_status(_wc_path, snapshot, status, variables);
				});
			}
		});
	}

	void info_t::update_status (std::string const& path, fs::snapshot_t const& snapshot, scm::status_map_t const& newStatus, std::map<std::string, std::string> const& newVariables)
	{
		auto it = cache().find(path);
		if(it != cache().end())
		{
			std::set<std::string> changedPaths;

			auto const oldStatus = it->second->_file_status;
			auto oldStatusIter = oldStatus.begin();
			auto newStatusIter = newStatus.begin();

			while(oldStatusIter != oldStatus.end() || newStatusIter != newStatus.end())
			{
				if(newStatusIter == newStatus.end() || oldStatusIter != oldStatus.end() && oldStatusIter->first < newStatusIter->first)
				{
					changedPaths.insert(oldStatusIter->first);
					++oldStatusIter;
					continue;
				}

				if(oldStatusIter == oldStatus.end() || newStatusIter != newStatus.end() && newStatusIter->first < oldStatusIter->first)
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
			it->second->_variables   = newVariables;

			it->second->_callbacks(&callback_t::status_changed, *it->second, changedPaths);
		}
	}

	// ==============
	// = Public API =
	// ==============

	info_ptr info (std::string const& dir)
	{
		if(!settings_for_path(NULL_STR, scope::scope_t(), dir).get(kSettingsSCMStatusKey, true))
			return info_ptr();

		std::string wcPath;
		if(driver_t const* driver = driver_for_path(dir, &wcPath))
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
