#include <settings/settings.h>
#include <test/jail.h>

class CascadingTests : public CxxTest::TestSuite
{
public:
	void test_scope_selector_ranking ()
	{
		test::jail_t jail;

		jail.set_content(".tm_properties",         "mySetting = 1\n[ text ]\nmySetting = 2\n[ text.markup ]\nmySetting = 3\n[ text.markup.html ]\nmySetting = 4\n");
		jail.set_content("foo/.tm_properties",     "mySetting = 5\n[ text.markup ]\nmySetting = 6\n");
		jail.set_content("foo/bar/.tm_properties", "[ text ]\nmySetting = 7\n[ text.markup.html ]\nmySetting = 8\n");

		TS_ASSERT_EQUALS(settings_for_path(jail.path("file")                            ).get("mySetting"), "1");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file"),                     "text").get("mySetting"), "2");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file"),              "text.markup").get("mySetting"), "3");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file"),         "text.markup.html").get("mySetting"), "4");

		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/file")                        ).get("mySetting"), "5");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/file"),                 "text").get("mySetting"), "2");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/file"),          "text.markup").get("mySetting"), "6");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/file"),     "text.markup.html").get("mySetting"), "4");

		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/bar/file")                    ).get("mySetting"), "5");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/bar/file"),             "text").get("mySetting"), "7");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/bar/file"),      "text.markup").get("mySetting"), "6");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("foo/bar/file"), "text.markup.html").get("mySetting"), "8");
	}
};
