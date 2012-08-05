#include <scm/scm.h>
#include <text/format.h>
#include <io/io.h>
#include <test/jail.h>

CXXTEST_ENUM_TRAITS(scm::status::type,
	CXXTEST_ENUM_MEMBER(scm::status::none);
	CXXTEST_ENUM_MEMBER(scm::status::unversioned);
	CXXTEST_ENUM_MEMBER(scm::status::versioned);
	CXXTEST_ENUM_MEMBER(scm::status::modified);
	CXXTEST_ENUM_MEMBER(scm::status::added);
	CXXTEST_ENUM_MEMBER(scm::status::deleted);
	CXXTEST_ENUM_MEMBER(scm::status::conflicted);
	CXXTEST_ENUM_MEMBER(scm::status::ignored);
);

class git_tests : public CxxTest::TestSuite
{
	struct setup_t
	{
		setup_t (std::string const& cmd)
		{
			std::string const script = text::format("{ cd '%1$s' && git init && touch .dummy && git add .dummy && git commit .dummy -mGetHead && %2$s ; } >/dev/null", jail.path().c_str(), cmd.c_str());
			if(system(script.c_str()) != 0 || !(info = scm::info(jail.path(".dummy"))))
				TS_FAIL(("error in setup: " + script).c_str());
		}

		scm::status::type status (std::string const& path) const
		{
			return info->status(jail.path(path));
		}

	private:
		test::jail_t jail;
		scm::info_ptr info;
	};

public:
	// =================
	// = Folder Status =
	// =================

	void test_empty_folder ()
	{
		setup_t wc("mkdir folder");
		TS_ASSERT_EQUALS(wc.status("folder"), scm::status::none);
	}

	void test_folder_with_untracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::unversioned);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::unversioned);
	}

	void test_folder_with_ignored_file ()
	{
		setup_t wc("echo a > .git/info/exclude && mkdir folder && touch folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::none);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::none);
	}

	void test_folder_with_untracked_and_ignored_file ()
	{
		setup_t wc("echo a > .git/info/exclude && mkdir folder && touch folder/{a,b}");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::unversioned);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::none);
		TS_ASSERT_EQUALS(wc.status("folder/b"), scm::status::unversioned);
	}

	void test_folder_with_untracked_and_folder ()
	{
		setup_t wc("mkdir -p folder/b && touch folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::unversioned);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::unversioned);
		TS_ASSERT_EQUALS(wc.status("folder/b"), scm::status::none);
	}

	void test_folder_with_added_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::added);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::added);
	}

	void test_folder_with_added_and_untracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::mixed);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::added);
		TS_ASSERT_EQUALS(wc.status("folder/b"), scm::status::unversioned);
	}

	void test_folder_with_added_and_ignored_file ()
	{
		setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a && echo b > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::added);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::added);
		TS_ASSERT_EQUALS(wc.status("folder/b"), scm::status::none);
	}

	void test_folder_with_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::versioned);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::versioned);
	}

	void test_folder_with_modified_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && echo update > folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::modified);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::modified);
	}

	void test_folder_with_tracked_and_untracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/{a,b} && git add folder/a && git commit -mInitial");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::mixed);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::versioned);
		TS_ASSERT_EQUALS(wc.status("folder/b"), scm::status::unversioned);
	}

	void test_folder_with_missing_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::deleted);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::deleted);
	}

	void test_missing_folder_with_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a && rmdir folder");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::deleted);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::deleted);
	}

	void test_ignored_folder_with_untracked_file ()
	{
		setup_t wc("echo folder > .git/info/exclude && mkdir folder && touch folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::none);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::none);
	}

	void test_ignored_folder_with_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && echo folder > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::versioned);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::versioned);
	}

	void test_ignored_folder_with_missing_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::deleted);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::deleted);
	}

	void test_missing_ignored_folder_with_tracked_file ()
	{
		setup_t wc("mkdir folder && touch folder/a && git add folder/a && git commit -mInitial && rm folder/a && rmdir folder && echo folder > .git/info/exclude && echo folder > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("folder"),   scm::status::deleted);
		TS_ASSERT_EQUALS(wc.status("folder/a"), scm::status::deleted);
	}

	// ===============
	// = File Status =
	// ===============

	void test_untracked_file ()
	{
		setup_t wc("touch file");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::unversioned);
	}

	void test_added_file ()
	{
		setup_t wc("touch file && git add file");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::added);
	}

	void test_tracked_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::versioned);
	}

	void test_modified_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial && echo update > file");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::modified);
	}

	void test_deleted_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial && rm file");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::deleted);
	}

	// =============================
	// = Also mark file as ignored =
	// =============================

	void test_ignored_file ()
	{
		setup_t wc("touch file && echo file > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::none);
	}

	void test_ignored_added_file ()
	{
		setup_t wc("touch file && git add file && echo file > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::added);
	}

	void test_ignored_tracked_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial && echo file > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::versioned);
	}

	void test_ignored_modified_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial && echo update > file && echo file > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::modified);
	}

	void test_ignored_deleted_file ()
	{
		setup_t wc("touch file && git add file && git commit -mInitial && rm file && echo file > .git/info/exclude");
		TS_ASSERT_EQUALS(wc.status("file"), scm::status::deleted);
	}
};
