#include "../src/track_paths.h"
#include <io/src/path.h>
#include <text/src/format.h>
#include <test/jail.h>

static bool test_range (track_paths_t& tracker, std::vector<std::string> const& paths, size_t first, size_t last)
{
	bool res = true;
	for(size_t i = 0; i < first; ++i)
		res = res && (tracker.is_changed(paths[i]) == false);
	for(size_t i = first; i < last; ++i)
		res = res && (tracker.is_changed(paths[i]) == true);
	for(size_t i = last; i < paths.size(); ++i)
		res = res && (tracker.is_changed(paths[i]) == false);
	return res;
}

void test_track_file ()
{
	// created → deleted
	// created → updated
	// created → created
	// missing → created
	// missing → missing

	test::jail_t jail;
	std::vector<std::string> paths;
	for(size_t i = 0; i < 50; ++i)
		paths.push_back(jail.path(text::format("%02zu.txt", i)));

	for(size_t i = 0; i < 30; ++i)
		path::set_content(paths[i], "");

	track_paths_t tracker;
	for(auto const& path : paths)
		tracker.add(path);
	usleep(100000);
	OAK_ASSERT(test_range(tracker, paths, 50, 50));

	for(size_t i = 0; i < 10; ++i)
		unlink(paths[i].c_str());
	usleep(100000);
	OAK_ASSERT(test_range(tracker, paths,  0, 10));

	for(size_t i = 10; i < 20; ++i)
		path::set_content(paths[i], "");
	usleep(100000);
	OAK_ASSERT(test_range(tracker, paths, 10, 20));

	for(size_t i = 30; i < 40; ++i)
		path::set_content(paths[i], "");
	usleep(100000);
	OAK_ASSERT(test_range(tracker, paths, 30, 40));
}
