#include "../src/gutter_diff.h"
#include "../src/drivers/api.h"
#include <io/exec.h>
#include <text/format.h>
#include <test/jail.h>

#include <CoreFoundation/CoreFoundation.h>

using scm::gutter_diff::change;
using scm::gutter_diff::compute;
using scm::gutter_diff::invalidate_repo;
using scm::gutter_diff::result_t;

namespace
{
	// Spin a sub-runloop until `compute`'s completion fires. The
	// component dispatches its result onto the main queue, so the
	// test thread (which is the main thread under --no-parallel for
	// .cc tests as well, since cxx-test main is single-threaded by
	// default) needs to pump or we deadlock.
	result_t await_compute (std::string const& root, std::string const& rel,
	                        std::string const& current)
	{
		__block bool done = false;
		__block result_t out;
		CFRunLoopRef runLoop = CFRunLoopGetCurrent();

		compute(root, rel, current, ^(result_t r){
			out  = r;
			done = true;
			CFRunLoopStop(runLoop);
		});

		while(!done)
			CFRunLoopRun();

		return out;
	}

	void bootstrap_repo (test::jail_t const& jail, std::string const& git,
	                     std::string const& path, std::string const& content)
	{
		std::string const script = text::format(
			"{ cd '%1$s' "
			"&& '%2$s' init -b master "
			"&& '%2$s' config user.email 'test@example.com' "
			"&& '%2$s' config user.name 'Test Test' "
			"&& '%2$s' config commit.gpgsign false "
			"&& printf %%s '%3$s' > '%4$s' "
			"&& '%2$s' add '%4$s' "
			"&& '%2$s' commit -m initial "
			"; } >/dev/null",
			jail.path().c_str(), git.c_str(), content.c_str(), path.c_str());
		io::exec("/bin/sh", "-c", script.c_str(), nullptr);
	}
}

void test_gutter_diff_integration_modified_buffer ()
{
	static std::string const git = scm::find_executable("git", "TM_GIT");
	if(git == NULL_STR)
		return;

	test::jail_t jail;
	bootstrap_repo(jail, git, "file.txt", "a\nb\nc\n");
	invalidate_repo(jail.path());

	result_t r = await_compute(jail.path(), "file.txt", "a\nB\nc\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[2], change::modified);
}

void test_gutter_diff_integration_untracked_file_all_added ()
{
	static std::string const git = scm::find_executable("git", "TM_GIT");
	if(git == NULL_STR)
		return;

	test::jail_t jail;
	bootstrap_repo(jail, git, "file.txt", "x\n");
	invalidate_repo(jail.path());

	// new.txt was never committed; HEAD blob fetch fails → empty
	// blob → diff against the buffer text shows everything as added.
	result_t r = await_compute(jail.path(), "new.txt", "alpha\nbeta\n");
	OAK_ASSERT_EQ(r.size(), 2);
	OAK_ASSERT_EQ(r[1], change::added);
	OAK_ASSERT_EQ(r[2], change::added);
}

void test_gutter_diff_integration_clean_buffer_no_marks ()
{
	static std::string const git = scm::find_executable("git", "TM_GIT");
	if(git == NULL_STR)
		return;

	test::jail_t jail;
	bootstrap_repo(jail, git, "file.txt", "a\nb\nc\n");
	invalidate_repo(jail.path());

	result_t r = await_compute(jail.path(), "file.txt", "a\nb\nc\n");
	OAK_ASSERT(r.empty());
}
