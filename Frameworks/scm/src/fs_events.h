#ifndef FS_EVENTS_H_QDH73MIO
#define FS_EVENTS_H_QDH73MIO

namespace scm
{
	struct info_t;

	struct watcher_t
	{
		watcher_t (std::string const& path, info_t* info);
		~watcher_t ();

	private:
		static void callback_function (ConstFSEventStreamRef streamRef, void* clientCallBackInfo, size_t numEvents, void* eventPaths, FSEventStreamEventFlags const eventFlags[], FSEventStreamEventId const eventIds[]);
		void callback (std::set<std::string> const& changedPaths);

		std::string path;
		info_t* info;

		std::string mount_point;
		FSEventStreamRef stream;
	};

} /* scm */

#endif /* end of include guard: FS_EVENTS_H_QDH73MIO */
