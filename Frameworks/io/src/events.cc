#include "events.h"
#include "path.h"
#include <cf/cf.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(FS_Events);

namespace
{
	struct device_info_t
	{
		dev_t device;
		std::string mount_point;

		device_info_t (std::string path)
		{
			while(true)
			{
				struct statfs buf;
				if(statfs(path.c_str(), &buf) == 0)
				{
					device      = buf.f_fsid.val[0];
					mount_point = buf.f_mntonname;
					break;
				}

				ASSERT_EQ(errno, ENOENT);
				path = path::parent(path);
			}
		}
	};

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

	struct events_t
	{
		struct stream_t
		{
			stream_t (std::string const& path, fs::event_callback_t* calback, uint64_t eventId, CFTimeInterval latency) : _requested(path), _observed(_requested), _callback(calback), _event_id(eventId), _replay(false)
			{
				while(!_observed.is_directory() && _observed.path() != "/")
					_observed = file_info_t(path::parent(_observed.path()));

				device_info_t devInfo(_observed.path());
				_mount_point = devInfo.mount_point;
				std::string devPath = path::relative_to(_observed.path(), devInfo.mount_point);
				D(DBF_FS_Events, bug("watch ‘%s’ / ‘%s’ (device %x, event 0x%llx)\n", devInfo.mount_point.c_str(), devPath.c_str(), devInfo.device, _event_id););

				FSEventStreamContext contextInfo = { 0, this, nullptr, nullptr, nullptr };
				if(!(_stream = FSEventStreamCreateRelativeToDevice(kCFAllocatorDefault, &events_t::callback, &contextInfo, devInfo.device, cf::wrap(std::vector<std::string>(1, devPath)), eventId, latency, kFSEventStreamCreateFlagNone)))
					fprintf(stderr, "can’t observe ‘%s’\n", path.c_str());

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
					D(DBF_FS_Events, bug("%s, 0x%llx\n", BSTR(flag), eventId););

					_replay = flag;
					_event_id = std::max(eventId, _event_id);
					_callback->set_replaying_history(flag, observedPath, flag ? eventId : _event_id);
				}
			}

			explicit operator bool () const    { return _stream; }
			operator FSEventStreamRef () const { return _stream; }

			std::string _mount_point;
			FSEventStreamRef _stream;
			file_info_t _requested;
			file_info_t _observed;
			fs::event_callback_t* _callback;
			uint64_t _event_id;
			bool _replay;
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
			fprintf(stderr, "*** not watching ‘%s’\n", path.c_str());
		}

		static void callback (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[])
		{
			stream_t& stream = *static_cast<stream_t*>(clientCallBackInfo);
			D(DBF_FS_Events, bug("%zu events\n", numEvents););

			uint64_t lastEventId = 0;
			for(size_t i = 0; i < numEvents; ++i)
			{
				std::string path = path::join(stream._mount_point, ((char const* const*)eventPaths)[i]);
				std::string const& requestedPath = stream._requested.path();
				D(DBF_FS_Events, bug("%zu/%zu) 0x%llx: %s%s%s%s\n", i+1, numEvents, eventIds[i], path.c_str(), (eventFlags[i] & kFSEventStreamEventFlagMustScanSubDirs) ? ", recursive" : "", (eventFlags[i] & kFSEventStreamEventFlagHistoryDone) ? ", history done" : "", stream._replay ? " (replay history)" : ""););
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
		D(DBF_FS_Events, bug("%s, %p\n", path.c_str(), callback););
		events().watch(path, callback, eventId, latency);
	}

	void unwatch (std::string const& path, event_callback_t* callback)
	{
		D(DBF_FS_Events, bug("%s, %p\n", path.c_str(), callback););
		events().unwatch(path, callback);
	}

} /* fs */
