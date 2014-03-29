#include <io/path.h>

void test_escape ()
{
	OAK_ASSERT_EQ(path::escape("It's working"),  "It\\'s\\ working");
	OAK_ASSERT_EQ(path::escape("It's\nworking"), "It\\'s'\n'working");
	OAK_ASSERT_EQ(path::escape("/foo/bar.txt"),  "/foo/bar.txt");
	OAK_ASSERT_EQ(path::escape("Æblegrød.txt"),  "Æblegrød.txt");
}
