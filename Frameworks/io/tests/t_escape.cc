#include <io/src/path.h>

void test_escape ()
{
	OAK_ASSERT_EQ(path::escape("It's working"),  "It\\'s\\ working");
	OAK_ASSERT_EQ(path::escape("It's\nworking"), "It\\'s'\n'working");
	OAK_ASSERT_EQ(path::escape("/foo/bar.txt"),  "/foo/bar.txt");
	OAK_ASSERT_EQ(path::escape("Æblegrød.txt"),  "Æblegrød.txt");
}

void test_unescape ()
{
	OAK_ASSERT_EQ(path::unescape("It\\'s\\ working").front(),  "It's working");
	OAK_ASSERT_EQ(path::unescape("It\\'s'\n'working").front(), "It's\nworking");
	OAK_ASSERT_EQ(path::unescape("/foo/bar.txt").front(),      "/foo/bar.txt");
	OAK_ASSERT_EQ(path::unescape("Æblegrød.txt").front(),      "Æblegrød.txt");

	auto v1 = path::unescape("/path/to/binary arg1 arg2 \"arg3 has multiple words\"");
	OAK_ASSERT_EQ(v1.size(), 4);
	OAK_ASSERT_EQ(v1[0], "/path/to/binary");
	OAK_ASSERT_EQ(v1[1], "arg1");
	OAK_ASSERT_EQ(v1[2], "arg2");
	OAK_ASSERT_EQ(v1[3], "arg3 has multiple words");

	auto v2 = path::unescape("/path/to/binary  arg1  'arg2 could be single-quoted'\\ also''");
	OAK_ASSERT_EQ(v2.size(), 3);
	OAK_ASSERT_EQ(v2[0], "/path/to/binary");
	OAK_ASSERT_EQ(v2[1], "arg1");
	OAK_ASSERT_EQ(v2[2], "arg2 could be single-quoted also");
}
