#include <text/decode.h>

class DecodeTests : public CxxTest::TestSuite
{
public:
	void test_decode ()
	{
		TS_ASSERT_EQUALS(decode::entities("Hello world"),                   "Hello world");
		TS_ASSERT_EQUALS(decode::entities("Hello&nbsp;world"),              "Hello world");
		TS_ASSERT_EQUALS(decode::entities("Hello &quot;world&quot;"),       "Hello \"world\"");
		TS_ASSERT_EQUALS(decode::entities("Hello &lt;world&gt;"),           "Hello <world>");
		TS_ASSERT_EQUALS(decode::entities("Hello &lt-world&gt;"),           "Hello &lt-world>");
		TS_ASSERT_EQUALS(decode::entities("Hello &lt;world&gt-"),           "Hello <world&gt-");
		TS_ASSERT_EQUALS(decode::entities("&AElig;blegr&oslash;d&hellip;"), "Æblegrød…");
	}

	void test_decode_url ()
	{
		TS_ASSERT_EQUALS(decode::url_part("ActionScript%203%2BR.tbz"), "ActionScript 3+R.tbz");
		TS_ASSERT_EQUALS(decode::url_part("%C3%86blegr%C3%B8d"),       "Æblegrød");
	}
};
