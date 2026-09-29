#include <settings/src/settings.h>
#include <test/jail.h>

void test_scope_selector_ranking ()
{
	test::jail_t jail;

	jail.set_content(".tm_properties",         "mySetting = 1\n[ text ]\nmySetting = 2\n[ text.markup ]\nmySetting = 3\n[ text.markup.html ]\nmySetting = 4\n");
	jail.set_content("foo/.tm_properties",     "mySetting = 5\n[ text.markup ]\nmySetting = 6\n");
	jail.set_content("foo/bar/.tm_properties", "[ text ]\nmySetting = 7\n[ text.markup.html ]\nmySetting = 8\n");

	OAK_ASSERT_EQ(settings_for_path(jail.path("file")                            ).get("mySetting"), "1");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file"),                     "text").get("mySetting"), "2");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file"),              "text.markup").get("mySetting"), "3");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file"),         "text.markup.html").get("mySetting"), "4");

	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/file")                        ).get("mySetting"), "5");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/file"),                 "text").get("mySetting"), "5");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/file"),          "text.markup").get("mySetting"), "6");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/file"),     "text.markup.html").get("mySetting"), "6");

	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/bar/file")                    ).get("mySetting"), "5");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/bar/file"),             "text").get("mySetting"), "7");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/bar/file"),      "text.markup").get("mySetting"), "7");
	OAK_ASSERT_EQ(settings_for_path(jail.path("foo/bar/file"), "text.markup.html").get("mySetting"), "8");
}

void test_glob_and_scope_selector_ranking ()
{
	test::jail_t jail;

	jail.set_content(".tm_properties",       "mySetting = 1\n[ text ]\nmySetting = 2\n");
	jail.set_content("local/.tm_properties", "mySetting = 3\n[ text ]\nmySetting = 4\n[ *.txt ]\nmySetting = 5\n");

	OAK_ASSERT_EQ(settings_for_path(jail.path("file.foo")              ).get("mySetting"), "1");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.txt")              ).get("mySetting"), "1");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.foo"),       "text").get("mySetting"), "2");
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.txt"),       "text").get("mySetting"), "2");
	OAK_ASSERT_EQ(settings_for_path(jail.path("local/file.foo")        ).get("mySetting"), "3");
	OAK_ASSERT_EQ(settings_for_path(jail.path("local/file.txt")        ).get("mySetting"), "5");
	OAK_ASSERT_EQ(settings_for_path(jail.path("local/file.foo"), "text").get("mySetting"), "4");
	OAK_ASSERT_EQ(settings_for_path(jail.path("local/file.txt"), "text").get("mySetting"), "5");
}
