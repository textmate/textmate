#include <io/src/path.h>
#include <text/src/format.h>
#include <test/jail.h>

template <size_t N> void run_test (std::string const (&path_list)[N], size_t expected_parents)
{
	std::vector<std::string> paths(path_list, path_list + N);
	std::vector<size_t> const& parents = path::disambiguate(paths);
	std::string name = path::display_name(paths[0], parents[0]);
	OAK_ASSERT_EQ(name, path::display_name(paths[0], expected_parents));
}

template <size_t N> void run_test (std::string const (&path_list)[N], std::string expected)
{
	std::vector<std::string> paths(path_list, path_list + N);
	std::vector<size_t> const& parents = path::disambiguate(paths);
	std::string name = path::display_name(paths[0], parents[0]);
	OAK_ASSERT_EQ(name, expected);
}

void test_disambiguate_paths ()
{
	std::string const paths1[] = { "/foo/bar/baz/qux", "/bar/bar/baz/qux" };
	run_test(paths1, "qux — foo/bar/baz");
	std::string const paths2[] = { "/a/b/c/d", "/e/f/g/h/i/j" };
	run_test(paths2, 0);
	std::string const paths3[] = { "/foo/bar/a/baz/qux", "/foo/bar/b/baz/qux" };
	run_test(paths3, "qux — a/baz");
}

void test_disambiguate_paths_duplicates ()
{
	std::vector<std::string> paths{
		"/Users/duff/Projects/Avian",
		"/Users/duff/Projects/TextMate",
		"/Users/duff/public/macromates_com",
		"/Users/duff/Projects/macromates_com",
		"/Users/duff/public/macromates_com",
		"/Users/duff/Projects/api_textmate_org",
		"/Users/duff/Library/Application Support/TextMate",
		"/Users/duff/Projects/Avian",
		"/Users/duff/build/public/macromates_com",
	};

	std::vector<size_t> parents = path::disambiguate(paths);
	OAK_ASSERT_EQ(parents.size(), paths.size());

	OAK_ASSERT_EQ(parents[0], 0);
	OAK_ASSERT_EQ(parents[1], 1);
	OAK_ASSERT_EQ(parents[2], 2);
	OAK_ASSERT_EQ(parents[3], 1);
	OAK_ASSERT_EQ(parents[4], 2);
	OAK_ASSERT_EQ(parents[5], 0);
	OAK_ASSERT_EQ(parents[6], 1);
	OAK_ASSERT_EQ(parents[7], 0);
	OAK_ASSERT_EQ(parents[8], 2);
}
