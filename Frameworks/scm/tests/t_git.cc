#include "../src/drivers/api.h"
#include <scm/src/scm.h>
#include <text/src/format.h>
#include <io/src/io.h>
#include <io/src/exec.h>
#include <test/jail.h>

struct setup_t
{
	setup_t (std::string const& cmd)
	{
		static std::string const git = scm::find_executable("git", "TM_GIT");
		std::string const script = text::format("{ cd '%1$s' && '%2$s' init && '%2$s' config user.email 'test@example.com' && '%2$s' config user.name 'Test Test' && '%2$s' config commit.gpgsign false && touch .dummy && '%2$s' add .dummy && '%2$s' commit .dummy -mGetHead && %3$s ; } >/dev/null", jail.path().c_str(), git.c_str(), cmd.c_str());
		if(io::exec("/bin/sh", "-c", script.c_str(), nullptr) != NULL_STR)
		{
			if(info = scm::info(jail.path()))
			{
				wait_for_status(info);
			}
			else
			{
				OAK_FAIL("no SCM info for path: " + jail.path());
			}
		}
		else
		{
			OAK_FAIL("error in setup: " + script);
		}
	}

	scm::status::type status (std::string const& path) const
	{
		return info->status(jail.path(path));
	}

	std::string variable (std::string const& var) const
	{
		auto vars = info->scm_variables();
		return vars.find(var) != vars.end() ? vars[var] : NULL_STR;
	}

private:
	test::jail_t jail;
	scm::info_ptr info;
};

// =================
// = Folder Status =
// =================

void test_variables ()
{
	setup_t wc("true");
	OAK_ASSERT_EQ(wc.variable("TM_SCM_NAME"),   "git");
	OAK_ASSERT_EQ(wc.variable("TM_SCM_BRANCH"), "master");
}

void test_empty_folder ()
{
	setup_t wc("mkdir folder");
	OAK_ASSERT_EQ(wc.status("folder"), scm::status::none);
}

void test_folder_with_untracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::unversioned);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::unversioned);
}

void test_folder_with_ignored_file ()
{
	setup_t wc("echo a > .git/info/exclude && mkdir folder && touch folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
}

void test_folder_with_untracked_and_ignored_file ()
{
	setup_t wc("echo a > .git/info/exclude && mkdir folder && touch folder/{a,b}");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::unversioned);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/b"), scm::status::unversioned);
}

void test_folder_with_untracked_and_folder ()
{
	setup_t wc("mkdir -p folder/b && touch folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::unversioned);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::unversioned);
	OAK_ASSERT_EQ(wc.status("folder/b"), scm::status::none);
}

void test_folder_with_added_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::added);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::added);
}

void test_folder_with_added_and_untracked_file ()
{
	setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::mixed);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::added);
	OAK_ASSERT_EQ(wc.status("folder/b"), scm::status::unversioned);
}

void test_folder_with_added_and_ignored_file ()
{
	setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a && echo b > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::added);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::added);
	OAK_ASSERT_EQ(wc.status("folder/b"), scm::status::none);
}

void test_folder_with_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
}

void test_folder_with_modified_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && echo update > folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::modified);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::modified);
}

void test_folder_with_tracked_and_untracked_file ()
{
	setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a && git commit -mInitial");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::mixed);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/b"), scm::status::unversioned);
}

void test_folder_with_missing_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::deleted);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::deleted);
}

void test_missing_folder_with_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a && rmdir folder");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::deleted);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::deleted);
}

void test_ignored_folder_with_untracked_file ()
{
	setup_t wc("echo folder > .git/info/exclude && mkdir folder && touch folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
}

void test_ignored_folder_with_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && echo folder > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::none);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::none);
}

void test_ignored_folder_with_missing_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::deleted);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::deleted);
}

void test_missing_ignored_folder_with_tracked_file ()
{
	setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a && rmdir folder && echo folder > .git/info/exclude && echo folder > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("folder"),   scm::status::deleted);
	OAK_ASSERT_EQ(wc.status("folder/a"), scm::status::deleted);
}

// ===============
// = File Status =
// ===============

void test_untracked_file ()
{
	setup_t wc("touch file");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::unversioned);
}

void test_added_file ()
{
	setup_t wc("touch file && git add file");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::added);
}

void test_tracked_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::none);
}

void test_modified_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial && echo update > file");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::modified);
}

void test_deleted_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial && rm file");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::deleted);
}

// =============================
// = Also mark file as ignored =
// =============================

void test_ignored_file ()
{
	setup_t wc("touch file && echo file > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::none);
}

void test_ignored_added_file ()
{
	setup_t wc("touch file && git add file && echo file > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::added);
}

void test_ignored_tracked_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial && echo file > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::none);
}

void test_ignored_modified_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial && echo update > file && echo file > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::modified);
}

void test_ignored_deleted_file ()
{
	setup_t wc("touch file && git add file && git commit -mInitial && rm file && echo file > .git/info/exclude");
	OAK_ASSERT_EQ(wc.status("file"), scm::status::deleted);
}
