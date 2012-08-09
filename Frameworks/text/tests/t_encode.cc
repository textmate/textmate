#include <text/encode.h>

class EncodeTests : public CxxTest::TestSuite
{
public:
	void test_url ()
	{
		TS_ASSERT_EQUALS("http://host/" + encode::url_part("æblegrød.html"), "http://host/%C3%A6blegr%C3%B8d.html");
		TS_ASSERT_EQUALS(encode::url_part("http://example?a=b&c=d"), "http%3A%2F%2Fexample%3Fa%3Db%26c%3Dd");
		TS_ASSERT_EQUALS(encode::url_part("me@example.org"), "me%40example.org");
		TS_ASSERT_EQUALS("file://localhost" + encode::url_part("/foo/bar/file name.txt", "/"), "file://localhost/foo/bar/file%20name.txt");
	}
};
