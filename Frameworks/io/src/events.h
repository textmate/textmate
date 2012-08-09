#ifndef FS_EVENTS_H_E5F4DSTS
#define FS_EVENTS_H_E5F4DSTS

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5

#include <oak/misc.h>

namespace fs
{
	struct event_callback_t
	{
		virtual ~event_callback_t () { }
		virtual void set_replaying_history (bool flag, std::string const& observedPath, uint64_t eventId) { }
		virtual void did_change (std::string const& path, std::string const& observedPath, uint64_t eventId, bool recursive) = 0;
	};

	PUBLIC void watch (std::string const& path, event_callback_t* callback, uint64_t eventId = kFSEventStreamEventIdSinceNow, CFTimeInterval latency = 1.0);
	PUBLIC void unwatch (std::string const& path, event_callback_t* callback);

} /* fs */

#endif /* MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5 */

#endif /* end of include guard: FS_EVENTS_H_E5F4DSTS */
