#ifndef FS_EVENTS_H_E5F4DSTS
#define FS_EVENTS_H_E5F4DSTS

namespace fs
{
	struct event_callback_t
	{
		virtual ~event_callback_t () { }
		virtual void set_replaying_history (bool flag, std::string const& observedPath, uint64_t eventId) { }
		virtual void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive) = 0;
	};

	void watch (std::string const& path, event_callback_t* callback, uint64_t eventId = kFSEventStreamEventIdSinceNow, CFTimeInterval latency = 1);
	void unwatch (std::string const& path, event_callback_t* callback);

} /* fs */

#endif /* end of include guard: FS_EVENTS_H_E5F4DSTS */
