#include "fs_events.h"
#include <io/path.h>
#include <cf/cf.h>

namespace scm
{
	// =============
	// = watcher_t =
	// =============

	watcher_t::watcher_t (std::string const& path, std::function<void(std::set<std::string> const&)> const& callback) : path(path), callback(callback), stream(nullptr)
	{
		struct statfs buf;
		if(statfs(path.c_str(), &buf) != 0)
			return;

		mount_point            = buf.f_mntonname;
		dev_t device           = buf.f_fsid.val[0];
		std::string devicePath = path::relative_to(path, mount_point);

		FSEventStreamContext contextInfo = { 0, this, nullptr, nullptr, nullptr };
		if(stream = FSEventStreamCreateRelativeToDevice(kCFAllocatorDefault, &callback_function, &contextInfo, device, cf::wrap(std::vector<std::string>(1, devicePath)), kFSEventStreamEventIdSinceNow, 1, kFSEventStreamCreateFlagNone))
		{
			FSEventStreamScheduleWithRunLoop(stream, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
			FSEventStreamStart(stream);
			FSEventStreamFlushSync(stream);
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Can’t observe ‘%{public}s’", path.c_str());
		}
	}

	watcher_t::~watcher_t ()
	{
		if(!stream)
			return;

		FSEventStreamStop(stream);
		FSEventStreamInvalidate(stream);
		FSEventStreamRelease(stream);
	}

	void watcher_t::invoke_callback (std::set<std::string> const& changedPaths)
	{
		callback(changedPaths);
	}

	void watcher_t::callback_function (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[])
	{
		watcher_t& watcher = *(watcher_t*)clientCallBackInfo;

		std::set<std::string> changedPaths;

		for(size_t i = 0; i < numEvents; ++i)
		{
			std::string const& file = ((char const* const*)eventPaths)[i];
			std::string const& path = path::join(watcher.mount_point, "./" + file);
			changedPaths.insert(path);
		}
		watcher.invoke_callback(changedPaths);
	}
}
