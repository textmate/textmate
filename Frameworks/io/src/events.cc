#include "events.h"
#include "path.h"
#include <cf/src/cf.h>
#include <oak/debug.h>

namespace
{
	bool operator== (timespec const& lhs, timespec const& rhs)
	{
		return lhs.tv_sec == rhs.tv_sec && lhs.tv_nsec == rhs.tv_nsec;
	}

	struct file_info_t
	{
		file_info_t (std::string const& path) : _path(path), _exists(false)
		{
			struct stat buf;
			if(lstat(path.c_str(), &buf) == 0)
			{
				_exists    = true;
				_mode      = buf.st_mode;
				_mtimespec = buf.st_mtimespec;
				_ctimespec = buf.st_ctimespec;
			}
		}

		std::string const& path () const               { return _path; }
		bool exists () const                           { return _exists; }
		bool is_directory () const                     { return _exists && S_ISDIR(_mode); }
		bool operator!= (file_info_t const& rhs) const { return !(*this == rhs); }

		bool operator== (file_info_t const& rhs) const
		{
			return _path == rhs._path && (_exists ? rhs._exists && _mode == rhs._mode && _mtimespec == rhs._mtimespec && _ctimespec == rhs._ctimespec : !rhs._exists);
		}

	private:
		std::string _path;
		bool _exists;
		mode_t _mode;
		timespec _mtimespec; // { tv_sec, tv_nsec }
		timespec _ctimespec;
	};

	static std::string real_path (std::string const& path)
	{
		if(char* tmp = realpath(path.c_str(), nullptr))
		{
			std::string str = tmp;
			free(tmp);
			return str;
		}
		return path;
	}

	struct events_t
	{
		struct stream_t
		{
			stream_t (std::string const& path, fs::event_callback_t* calback, uint64_t eventId, CFTimeInterval latency) : _requested(path), _observed(real_path(path)), _callback(calback), _event_id(eventId), _replay(false)
			{
				_requestedExists = true;

				while(!_observed.is_directory() && _observed.path() != "/")
				{
					_observed = file_info_t(path::parent(_observed.path()));
					_requestedExists = false;
				}

				FSEventStreamContext contextInfo = { 0, this, nullptr, nullptr, nullptr };
				if(!(_stream = FSEventStreamCreate(kCFAllocatorDefault, &events_t::callback, &contextInfo, cf::wrap(std::vector<std::string>(1, _observed.path())), eventId, latency, kFSEventStreamCreateFlagNone)))
					os_log_error(OS_LOG_DEFAULT, "Can’t observe ‘%{public}s’", path.c_str());

				_event_id = FSEventsGetCurrentEventId();
			}

			~stream_t ()
			{
				if(!_stream)
					return;

				FSEventStreamStop(_stream);
				FSEventStreamInvalidate(_stream);
				FSEventStreamRelease(_stream);
			}

			void set_replaying_history (bool flag, std::string const& observedPath, uint64_t eventId)
			{
				if(flag != _replay)
				{
					_replay = flag;
					_event_id = std::max(eventId, _event_id);
					_callback->set_replaying_history(flag, observedPath, flag ? eventId : _event_id);
				}
			}

			explicit operator bool () const    { return _stream; }
			operator FSEventStreamRef () const { return _stream; }

			FSEventStreamRef _stream;
			file_info_t _requested;
			file_info_t _observed;
			fs::event_callback_t* _callback;
			uint64_t _event_id;
			bool _replay;
			bool _requestedExists;
		};

		typedef std::shared_ptr<stream_t> stream_ptr;
		std::vector<stream_ptr> streams;
		std::mutex streams_mutex;

		void watch (std::string const& path, fs::event_callback_t* cb, uint64_t eventId, CFTimeInterval latency)
		{
			auto stream = std::make_shared<stream_t>(path, cb, eventId, latency);

			streams_mutex.lock();
			streams.push_back(stream);
			streams_mutex.unlock();

			ASSERT(*stream);
			if(*stream)
			{
				FSEventStreamScheduleWithRunLoop(*stream, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
				stream->set_replaying_history(eventId != kFSEventStreamEventIdSinceNow, path, eventId);
				FSEventStreamStart(*stream);
				FSEventStreamFlushSync(*stream);
			}
		}

		void unwatch (std::string const& path, fs::event_callback_t* cb)
		{
			std::lock_guard<std::mutex> lock(streams_mutex);
			iterate(stream, streams)
			{
				if((*stream)->_requested.path() == path && (*stream)->_callback == cb)
					return (void)streams.erase(stream);
			}
			os_log_error(OS_LOG_DEFAULT, "Not watching ‘%{public}s’", path.c_str());
		}

		static void callback (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[])
		{
			stream_t& stream = *static_cast<stream_t*>(clientCallBackInfo);
			uint64_t lastEventId = 0;
			for(size_t i = 0; i < numEvents; ++i)
			{
				std::string path = ((char const* const*)eventPaths)[i];
				std::string const& requestedPath = stream._requested.path();

				if(stream._requestedExists && requestedPath != stream._observed.path())
					path = path::join(requestedPath, path::relative_to(path, stream._observed.path()));

				if(path.size() > 1 && path.back() == '/')
					path.erase(path.size()-1, 1);

				if(eventFlags[i] & kFSEventStreamEventFlagHistoryDone)
				{
					ASSERT(stream._replay);
					stream.set_replaying_history(false, requestedPath, eventIds[i]);
				}
				else
				{
					if(!stream._requested.exists()) // check if it has been created
					{
						std::string const parentPath = path::parent(requestedPath);
						if(path.compare(0, parentPath.size(), parentPath) == 0)
						{
							stream._requested = file_info_t(requestedPath);
							if(stream._requested.exists())
							{
								if(!stream._requested.is_directory())
									stream._callback->did_change(requestedPath, requestedPath, eventIds[i], eventFlags[i] & kFSEventStreamEventFlagMustScanSubDirs);
								else if(path.compare(0, requestedPath.size(), requestedPath) == 0)
									stream._callback->did_change(path, requestedPath, eventIds[i], eventFlags[i] & kFSEventStreamEventFlagMustScanSubDirs);
							}
						}
					}
					else if(!stream._requested.is_directory()) // check if file was modified
					{
						if(path == path::parent(requestedPath))
						{
							file_info_t newInfo(requestedPath);
							if(stream._requested != newInfo)
							{
								stream._requested = newInfo;
								stream._callback->did_change(requestedPath, requestedPath, eventIds[i], eventFlags[i] & kFSEventStreamEventFlagMustScanSubDirs);
							}
							else
							{
								// fprintf(stderr, "file not changed: %s\n", requestedPath.c_str());
							}
						}
						else
						{
							// fprintf(stderr, "skipping %s (watching %s, requested %s)\n", path.c_str(), stream._observed.path().c_str(), requestedPath.c_str());
						}
					}
					else if(path.compare(0, requestedPath.size(), requestedPath) == 0) // make sure the event is in our observed directory
					{
						stream._callback->did_change(path, requestedPath, eventIds[i], eventFlags[i] & kFSEventStreamEventFlagMustScanSubDirs);
					}
					else
					{
						// this happens if we setup observing for a non-existing directory which is later created
						// fprintf(stderr, "skipping %s (watching %s, requested %s)\n", path.c_str(), stream._observed.path().c_str(), requestedPath.c_str());
					}

					lastEventId = eventIds[i];
				}
			}

			if(!stream._replay && lastEventId)
				stream._event_id = lastEventId;
		}
	};
}

namespace fs
{
	static events_t& events ()
	{
		static events_t events;
		return events;
	}

	void watch (std::string const& path, event_callback_t* callback, uint64_t eventId, CFTimeInterval latency)
	{
		events().watch(path, callback, eventId, latency);
	}

	void unwatch (std::string const& path, event_callback_t* callback)
	{
		events().unwatch(path, callback);
	}

} /* fs */
