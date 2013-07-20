#include "scm.h"
#include "drivers/api.h"
#include "snapshot.h"
#include "fs_events.h"
#include <io/path.h>
#include <text/format.h>
#include <settings/settings.h>

namespace scm
{
	driver_t* git_driver ();
	driver_t* hg_driver ();
	driver_t* p4_driver ();
	driver_t* svn_driver ();

	static bool Disabled = false;
	static std::map<std::string, shared_info_weak_ptr> PendingUpdates;

	// =================
	// = shared_info_t =
	// =================

	struct shared_info_t : std::enable_shared_from_this<shared_info_t>
	{
		shared_info_t (std::string const& rootPath, scm::driver_t const* driver);
		~shared_info_t ();

		std::string const& root_path () const                           { return _root_path; }
		std::map<std::string, std::string> const& variables () const    { return _variables; }
		std::map<std::string, scm::status::type> const& status () const { return _status; }
		bool tracks_directories () const                                { return _driver->tracks_directories(); }

		void add_client (info_t* client);
		void remove_client (info_t* client);

	private:
		friend void enable ();

		void schedule_update ();
		static void async_update (shared_info_weak_ptr weakThis);
		void fs_did_change (std::set<std::string> const& changedPaths);

		void update (std::map<std::string, std::string> const& variables, std::map<std::string, scm::status::type> const& status, fs::snapshot_t const& fsSnapshot);

		std::string const _root_path;
		scm::driver_t const* const _driver;

		std::map<std::string, std::string> _variables;
		std::map<std::string, scm::status::type> _status;
		fs::snapshot_t _fs_snapshot;

		bool _pending_update = false;
		std::chrono::steady_clock::time_point _no_check_before = std::chrono::steady_clock::now();
		std::shared_ptr<watcher_t> _watcher;
		std::set<info_t*> _clients;
	};

	// ==========
	// = info_t =
	// ==========

	info_t::info_t (std::string const& path)
	{
	}

	info_t::~info_t ()
	{
		if(_shared_info)
			_shared_info->remove_client(this);

		while(!_callbacks.empty())
			pop_callback();
	}

	void info_t::set_shared_info (shared_info_ptr sharedInfo)
	{
		if(_shared_info)
			_shared_info->remove_client(this);
		if(_shared_info = sharedInfo)
			_shared_info->add_client(this);
	}

	bool info_t::dry () const
	{
		return !_shared_info;
	}

	std::string info_t::root_path () const
	{
		return dry() ? NULL_STR : _shared_info->root_path();
	}

	std::map<std::string, std::string> info_t::scm_variables () const
	{
		return dry() ? std::map<std::string, std::string>() : _shared_info->variables();
	}

	std::map<std::string, scm::status::type> const& info_t::status () const
	{
		static std::map<std::string, scm::status::type> const EmptyMap;
		return dry() ? EmptyMap : _shared_info->status();
	}

	scm::status::type info_t::status (std::string const& path) const
	{
		auto const& map = status();
		auto it = map.find(path);
		return dry() || path == NULL_STR ? scm::status::unknown : (it != map.end() ? it->second : scm::status::none);
	}

	bool info_t::tracks_directories () const
	{
		return dry() ? false : _shared_info->tracks_directories();
	}

	void info_t::add_callback (void (^block)(info_t const&))
	{
		_callbacks.push_back(Block_copy(block));
		if(!dry())
			did_update_shared_info();
	}

	void info_t::pop_callback ()
	{
		ASSERT(!_callbacks.empty());
		Block_release(_callbacks.back());
		_callbacks.pop_back();
	}

	void info_t::did_update_shared_info ()
	{
		ASSERT(!dry());
		for(auto callback : _callbacks)
			callback(*this);
	}

	// =================
	// = shared_info_t =
	// =================

	shared_info_t::shared_info_t (std::string const& rootPath, scm::driver_t const* driver) : _root_path(rootPath), _driver(driver)
	{
	}

	shared_info_t::~shared_info_t ()
	{
	}

	void shared_info_t::add_client (info_t* client)
	{
		_clients.insert(client);
		if(_clients.size() == 1)
		{
			_watcher.reset(new scm::watcher_t(_root_path, std::bind(&shared_info_t::fs_did_change, this, std::placeholders::_1)));
			schedule_update();
		}
		else
		{
			client->did_update_shared_info();
		}
	}

	void shared_info_t::remove_client (info_t* client)
	{
		if(_clients.size() == 1)
			_watcher.reset();
		_clients.erase(client);
	}

	void shared_info_t::update (std::map<std::string, std::string> const& variables, std::map<std::string, scm::status::type> const& status, fs::snapshot_t const& fsSnapshot)
	{
		bool shouldNotify = _variables != variables || _status != status;

		_variables   = variables;
		_status      = status;
		_fs_snapshot = fsSnapshot;

		if(shouldNotify)
		{
			for(info_t* client : _clients)
				client->did_update_shared_info();
		}
	}

	// =================
	// = Update Status =
	// =================

	void shared_info_t::async_update (shared_info_weak_ptr weakThis)
	{
		if(shared_info_ptr info = weakThis.lock())
		{
			if(!info->_driver->may_touch_filesystem() || info->_fs_snapshot != fs::snapshot_t(info->_root_path))
			{
				auto const status    = info->_driver->status(info->_root_path);
				auto const variables = info->_driver->variables(info->_root_path);
				auto const snapshot  = info->_driver->may_touch_filesystem() ? fs::snapshot_t(info->_root_path) : fs::snapshot_t();
				dispatch_async(dispatch_get_main_queue(), ^{
					if(shared_info_ptr info = weakThis.lock())
						info->update(variables, status, snapshot);
				});
			}

			dispatch_async(dispatch_get_main_queue(), ^{
				if(shared_info_ptr info = weakThis.lock())
				{
					info->_pending_update  = false;
					info->_no_check_before = std::chrono::steady_clock::now() + std::chrono::seconds(3);
				}
			});
		}
	}

	void shared_info_t::schedule_update ()
	{
		if(Disabled)
		{
			PendingUpdates[_root_path] = shared_from_this();
			return;
		}

		if(_pending_update)
			return;
		_pending_update = true;

		auto now = std::chrono::steady_clock::now();
		dispatch_time_t delay = now < _no_check_before ? dispatch_time(DISPATCH_TIME_NOW, std::chrono::duration_cast<std::chrono::duration<int64_t, std::nano>>(_no_check_before - now).count()) : DISPATCH_TIME_NOW;

		shared_info_weak_ptr weakThis = shared_from_this();
		static dispatch_queue_t queue = dispatch_queue_create("org.textmate.scm.status", DISPATCH_QUEUE_SERIAL);
		dispatch_after(delay, queue, ^{
			async_update(weakThis);
		});
	}

	void shared_info_t::fs_did_change (std::set<std::string> const& changedPaths)
	{
		schedule_update();
	}

	// =========
	// = Other =
	// =========

	static std::map<std::string, shared_info_weak_ptr>& cache ()
	{
		static auto res = new std::map<std::string, shared_info_weak_ptr>;
		return *res;
	}

	static dispatch_queue_t cache_access_queue ()
	{
		static dispatch_queue_t res = dispatch_queue_create("org.textmate.scm.info-cache", DISPATCH_QUEUE_SERIAL);
		return res;
	}

	static std::vector<driver_t*> const& drivers ()
	{
		static auto const res = new std::vector<driver_t*>{ scm::git_driver(), scm::hg_driver(), scm::p4_driver(), scm::svn_driver() };
		return *res;
	}

	static shared_info_ptr find_shared_info_for (std::string const& path)
	{
		for(std::string cwd = path; cwd != "/"; cwd = path::parent(cwd))
		{
			auto it = cache().find(cwd);
			if(it != cache().end())
			{
				if(shared_info_ptr res = it->second.lock())
					return res;
			}

			for(driver_t* driver : drivers())
			{
				if(driver && driver->has_info_for_directory(cwd))
				{
					shared_info_ptr res(new shared_info_t(cwd, driver));
					cache()[cwd] = res;
					return res;
				}
			}
		}
		return shared_info_ptr();
	}

	static bool scm_enabled_for_path (std::string const& path)
	{
		return path::is_absolute(path) && path != "/" && path != path::home() && path::is_local(path) && settings_for_path(NULL_STR, "", path).get(kSettingsSCMStatusKey, true);
	}

	void disable ()
	{
		Disabled = true;
	}

	void enable ()
	{
		Disabled = false;
		for(auto pair : PendingUpdates)
		{
			if(shared_info_ptr info = pair.second.lock())
				info->schedule_update();
		}
		PendingUpdates.clear();
	}

	std::string root_for_path (std::string const& path)
	{
		if(!scm_enabled_for_path(path))
			return NULL_STR;

		__block std::string res = NULL_STR;
		dispatch_sync(cache_access_queue(), ^{
			for(std::string cwd = path; res == NULL_STR && cwd != "/"; cwd = path::parent(cwd))
			{
				auto it = cache().find(cwd);
				if(it != cache().end())
				{
					res = cwd;
				}
				else
				{
					for(driver_t* driver : drivers())
					{
						if(driver && driver->has_info_for_directory(cwd))
						{
							res = cwd;
							break;
						}
					}
				}
			}
		});
		return res;
	}

	info_ptr info (std::string path)
	{
		if(!scm_enabled_for_path(path))
			return info_ptr();

		info_ptr res(new info_t(path));

		__block bool performBackgroundDriverSearch = true;
		dispatch_sync(cache_access_queue(), ^{
			for(std::string cwd = path; cwd != "/"; cwd = path::parent(cwd))
			{
				auto it = cache().find(cwd);
				if(it != cache().end())
				{
					if(shared_info_ptr sharedInfo = it->second.lock())
					{
						res->set_shared_info(sharedInfo);
						performBackgroundDriverSearch = cwd != path;
					}
					break;
				}
			}
		});

		if(performBackgroundDriverSearch)
		{
			weak_info_ptr weakInfo = res;
			dispatch_async(cache_access_queue(), ^{
				if(shared_info_ptr sharedInfo = find_shared_info_for(path))
				{
					dispatch_async(dispatch_get_main_queue(), ^{
						if(info_ptr info = weakInfo.lock())
							info->set_shared_info(sharedInfo);
					});
				}
			});
		}

		return res;
	}

	void wait_for_status (info_ptr info)
	{
		dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
		info->add_callback(^(scm::info_t const& unused){
			dispatch_semaphore_signal(semaphore);
		});
		dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
		dispatch_release(semaphore);
		info->pop_callback();
	}

} /* scm */
