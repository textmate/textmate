#include <text/encode.h>

void test_url ()
{
	OAK_ASSERT_EQ("http://host/" + encode::url_part("æblegrød.html"), "http://host/%C3%A6blegr%C3%B8d.html");
	OAK_ASSERT_EQ(encode::url_part("http://example?a=b&c=d"), "http%3A%2F%2Fexample%3Fa%3Db%26c%3Dd");
	OAK_ASSERT_EQ(encode::url_part("me@example.org"), "me%40example.org");
	OAK_ASSERT_EQ("file://localhost" + encode::url_part("/foo/bar/file name.txt", "/"), "file://localhost/foo/bar/file%20name.txt");
}
