#ifndef TRACK_PATHS_H_38DE4GVD
#define TRACK_PATHS_H_38DE4GVD

#include <io/path.h>
#include <oak/compat.h>

struct track_paths_t
{
	track_paths_t () { };
	track_paths_t (track_paths_t const& rhs) = delete;
	track_paths_t& operator= (track_paths_t const& rhs) = delete;

	void add (std::string const& path)
	{
		bool exists = true;
		int fd = open_file(path, &exists);
		if(fd != -1)
		{
			_open_files.emplace(path, std::make_pair(fd, exists));
			_track_fds.watch(fd);
		}
	}

	void remove (std::string const& path)
	{
		auto it = _open_files.find(path);
		if(it != _open_files.end())
		{
			_track_fds.unwatch(it->second.first);
			_open_files.erase(it);
		}
	}

	bool is_changed (std::string const& path)
	{
		bool res = false;
		auto it = _open_files.find(path);
		if(it != _open_files.end())
		{
			if(_track_fds.is_changed(it->second.first))
			{
				_track_fds.unwatch(it->second.first);

				bool didExist = it->second.second, exists = true;
				it->second.first = open_file(path, &exists);
				it->second.second = exists;
				_track_fds.watch(it->second.first);

				res = didExist || exists;
			}
		}
		else
		{
			add(path);
			res = true;
		}
		return res;
	}

private:
	struct track_fds_t
	{
		~track_fds_t ()
		{
			for(auto pair : _records)
			{
				dispatch_source_cancel(pair.second->source);
				dispatch_release(pair.second->source);
			}
			_records.clear();
		}

		void watch (int fd)
		{
			dispatch_source_t source = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, fd, DISPATCH_VNODE_DELETE|DISPATCH_VNODE_WRITE|DISPATCH_VNODE_EXTEND|DISPATCH_VNODE_RENAME|DISPATCH_VNODE_REVOKE, dispatch_get_main_queue());
			dispatch_source_set_cancel_handler(source, ^{
				close(fd);
			});

			auto record = std::make_shared<record_t>(source);
			dispatch_source_set_event_handler(source, ^{
				record->changed = true;
			});

			_records.emplace(fd, record);
			dispatch_resume(source);
		}

		void unwatch (int fd)
		{
			auto pair = _records.find(fd);
			if(pair != _records.end())
			{
				dispatch_source_cancel(pair->second->source);
				dispatch_release(pair->second->source);
				_records.erase(pair);
			}
		}

		bool is_changed (int fd)
		{
			auto pair = _records.find(fd);
			return pair != _records.end() && pair->second->changed;
		}

	private:
		struct record_t
		{
			record_t (dispatch_source_t source) : source(source) { }
			dispatch_source_t source;
			bool changed = false;
		};

		typedef std::shared_ptr<record_t> record_ptr;
		std::map<int, record_ptr> _records;
	};

	static int open_file (std::string const& path, bool* exists)
	{
		int fd = open(path.c_str(), O_EVTONLY|O_CLOEXEC, 0);
		return fd == -1 && errno == ENOENT ? (*exists = false), open_file(path::parent(path), exists) : fd;
	}

	track_fds_t _track_fds;
	std::map<std::string, std::pair<int, bool>> _open_files;
};

#endif /* end of include guard: TRACK_PATHS_H_38DE4GVD */
