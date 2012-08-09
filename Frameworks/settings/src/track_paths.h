#ifndef TRACK_PATHS_H_38DE4GVD
#define TRACK_PATHS_H_38DE4GVD

#include <io/path.h>
#include <oak/compat.h>

struct track_fds_t
{
	track_fds_t ()
	{
		pthread_mutex_init(&_file_descriptors_mutex, NULL);
		_queue = kqueue();
		pthread_create(&_thread, NULL, &track_fds_t::server_run_stub, this);
	}

	~track_fds_t ()
	{
		close(_queue);
		pthread_join(_thread, NULL);
		pthread_mutex_destroy(&_file_descriptors_mutex);
	}

	void watch (int fd, uint32_t fflags = NOTE_WRITE|NOTE_ATTRIB|NOTE_RENAME|NOTE_DELETE)
	{
		pthread_mutex_lock(&_file_descriptors_mutex);
		_file_descriptors.insert(std::make_pair(fd, false));
		pthread_mutex_unlock(&_file_descriptors_mutex);

		struct kevent changeList;
		struct timespec timeout = { };
		EV_SET(&changeList, fd, EVFILT_VNODE, EV_ADD | EV_ENABLE | EV_CLEAR, fflags, 0, NULL);
		if(kevent(_queue, &changeList, 1 /* number of changes */, NULL /* event list */, 0 /* number of events */, &timeout) == -1)
			fprintf(stderr, "%s: error observing fd %d: %s\n", getprogname(), fd, strerror(errno));
	}

	void unwatch (int fd)
	{
		pthread_mutex_lock(&_file_descriptors_mutex);
		auto it = _file_descriptors.find(fd);
		if(it != _file_descriptors.end())
		{
			_file_descriptors.erase(it);

			struct kevent changeList;
			struct timespec timeout = { };
			EV_SET(&changeList, fd, EVFILT_VNODE, EV_DELETE, 0, 0, NULL);
			if(kevent(_queue, &changeList, 1 /* number of changes */, NULL /* event list */, 0 /* number of events */, &timeout) == -1)
				fprintf(stderr, "%s: error unobserving fd %d: %s\n", getprogname(), fd, strerror(errno));
		}
		pthread_mutex_unlock(&_file_descriptors_mutex);
	}

	bool is_changed (int fd) const
	{
		bool res = false;
		pthread_mutex_lock(&_file_descriptors_mutex);
		auto it = _file_descriptors.find(fd);
		if(it != _file_descriptors.end())
			res = it->second;
		pthread_mutex_unlock(&_file_descriptors_mutex);
		return res;
	}

	void set_changed (int fd, bool flag)
	{
		pthread_mutex_lock(&_file_descriptors_mutex);
		auto it = _file_descriptors.find(fd);
		if(it != _file_descriptors.end())
			it->second = flag;
		pthread_mutex_unlock(&_file_descriptors_mutex);
	}

private:
	static void* server_run_stub (void* arg)
	{
		oak::set_thread_name("track_fds_t");
		((track_fds_t*)arg)->server_run();
		return NULL;
	}

	void server_run ()
	{
		struct kevent changed;
		while(kevent(_queue, NULL /* change list */, 0 /* number of changes */, &changed /* event list */, 1 /* number of events */, NULL) == 1)
		{
			if(changed.filter == EVFILT_VNODE)
			{
				pthread_mutex_lock(&_file_descriptors_mutex);
				int fd = (int)changed.ident;
				auto it = _file_descriptors.find(fd);
				if(it != _file_descriptors.end())
						it->second = true;
				else	fprintf(stderr, "%s: received kevent from untracked file descriptor: %d\n", getprogname(), fd);
				pthread_mutex_unlock(&_file_descriptors_mutex);
			}
			else
			{
				fprintf(stderr, "%s: received unknown kevent\n", getprogname());
			}
		}
	}

	int _queue;
	pthread_t _thread = NULL;

	mutable pthread_mutex_t _file_descriptors_mutex;
	std::map<int, bool> _file_descriptors;
};

struct track_paths_t
{
	void add (std::string const& path)
	{
		bool exists = true;
		int fd = open_file(path, &exists);
		if(fd != -1)
		{
			_open_files.insert(std::make_pair(path, std::make_pair(fd, exists)));
			_track_fds.watch(fd, NOTE_WRITE|NOTE_RENAME|NOTE_DELETE);
		}
	}

	void remove (std::string const& path)
	{
		auto it = _open_files.find(path);
		if(it != _open_files.end())
		{
			_track_fds.unwatch(it->second.first);
			close(it->second.first);
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
				close(it->second.first);

				bool didExist = it->second.second, exists = true;
				it->second.first = open_file(path, &exists);
				it->second.second = exists;
				_track_fds.watch(it->second.first, NOTE_WRITE|NOTE_RENAME|NOTE_DELETE);

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
	static int open_file (std::string const& path, bool* exists)
	{
		int fd = open(path.c_str(), O_EVTONLY/*|O_CLOEXEC*/, 0);
		return fd == -1 && errno == ENOENT ? (*exists = false), open_file(path::parent(path), exists) : fd;
	}

	track_fds_t _track_fds;
	std::map<std::string, std::pair<int, bool> > _open_files;
};

#endif /* end of include guard: TRACK_PATHS_H_38DE4GVD */
