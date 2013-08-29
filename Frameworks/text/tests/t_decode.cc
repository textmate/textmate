#include <text/decode.h>

void test_decode ()
{
	OAK_ASSERT_EQ(decode::entities("Hello world"),                   "Hello world");
	OAK_ASSERT_EQ(decode::entities("Hello&nbsp;world"),              "Hello world");
	OAK_ASSERT_EQ(decode::entities("Hello &quot;world&quot;"),       "Hello \"world\"");
	OAK_ASSERT_EQ(decode::entities("Hello &lt;world&gt;"),           "Hello <world>");
	OAK_ASSERT_EQ(decode::entities("Hello &lt-world&gt;"),           "Hello &lt-world>");
	OAK_ASSERT_EQ(decode::entities("Hello &lt;world&gt-"),           "Hello <world&gt-");
	OAK_ASSERT_EQ(decode::entities("&AElig;blegr&oslash;d&hellip;"), "Æblegrød…");
}

void test_decode_url ()
{
	OAK_ASSERT_EQ(decode::url_part("ActionScript%203%2BR.tbz"), "ActionScript 3+R.tbz");
	OAK_ASSERT_EQ(decode::url_part("%C3%86blegr%C3%B8d"),       "Æblegrød");
	OAK_ASSERT_EQ(decode::url_part("foo%2Bbar"),                "foo+bar");
	OAK_ASSERT_EQ(decode::url_part("foo+bar"),                  "foo bar");
}
