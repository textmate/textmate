#include <io/io.h>
#include <test/jail.h>

class unique_tests : public CxxTest::TestSuite
{
	void touch (std::string const& path)
	{
		path::set_content(path, "");
	}

public:
	void test_unique ()
	{
		test::jail_t jail;

		TS_ASSERT_EQUALS(path::unique(jail.path("foo.txt"), " copy"),          jail.path("foo.txt"));
		touch(path::unique(jail.path("foo.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo.txt"), " copy"),          jail.path("foo copy.txt"));
		touch(path::unique(jail.path("foo.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo.txt"), " copy"),          jail.path("foo copy 2.txt"));
		touch(path::unique(jail.path("foo.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo.txt"), " copy"),          jail.path("foo copy 3.txt"));
		touch(path::unique(jail.path("foo.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo copy.txt"), " copy"),     jail.path("foo copy 4.txt"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo copy 2.txt"), " copy"),   jail.path("foo copy 4.txt"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo copy 3.txt"), " copy"),   jail.path("foo copy 4.txt"));
		TS_ASSERT_EQUALS(path::unique(jail.path("foo copy 4.txt"), " copy"),   jail.path("foo copy 4.txt"));

		TS_ASSERT_EQUALS(path::unique(jail.path("untitled folder")),           jail.path("untitled folder"));
		path::make_dir(path::unique(jail.path("untitled folder")));
		TS_ASSERT_EQUALS(path::unique(jail.path("untitled folder")),           jail.path("untitled folder 2"));
		path::make_dir(path::unique(jail.path("untitled folder")));
		TS_ASSERT_EQUALS(path::unique(jail.path("untitled folder")),           jail.path("untitled folder 3"));
		TS_ASSERT_EQUALS(path::unique(jail.path("untitled folder 2")),         jail.path("untitled folder 3"));

		TS_ASSERT_EQUALS(path::unique(jail.path("bar.txt")),                   jail.path("bar.txt"));
		touch(path::unique(jail.path("bar.txt")));
		TS_ASSERT_EQUALS(path::unique(jail.path("bar.txt")),                   jail.path("bar 2.txt"));
		touch(path::unique(jail.path("bar.txt")));
		TS_ASSERT_EQUALS(path::unique(jail.path("bar.txt")),                   jail.path("bar 3.txt"));
		touch(path::unique(jail.path("bar.txt")));
		TS_ASSERT_EQUALS(path::unique(jail.path("bar.txt")),                   jail.path("bar 4.txt"));
		TS_ASSERT_EQUALS(path::unique(jail.path("bar 2.txt")),                 jail.path("bar 4.txt"));
		TS_ASSERT_EQUALS(path::unique(jail.path("bar 3.txt")),                 jail.path("bar 4.txt"));
	}

	void test_edge_cases ()
	{
		test::jail_t jail;
		touch(path::unique(jail.path("abcd.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path("abcd.txt"), " copy"), jail.path("abcd copy.txt"));

		TS_ASSERT_EQUALS(path::unique(jail.path(" copy.txt"), " copy"), jail.path(" copy.txt"));
		touch(path::unique(jail.path(" copy.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path(" copy.txt"), " copy"), jail.path(" copy copy.txt"));
		touch(path::unique(jail.path(" copy.txt"), " copy"));
		TS_ASSERT_EQUALS(path::unique(jail.path(" copy.txt"), " copy"), jail.path(" copy copy 2.txt"));
	}
};
