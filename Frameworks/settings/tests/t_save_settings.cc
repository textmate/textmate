#include <settings/settings.h>
#include <test/jail.h>

class SaveSettingsTests : public CxxTest::TestSuite
{
	test::jail_t jail;

	void setUp ()
	{
		settings_t::set_default_settings_path(jail.path("default"));
		settings_t::set_global_settings_path(jail.path("global"));

		settings_t::set("testKey_1", "set");
		settings_t::set("testKey_2", "set",   "attr.untitled");
		settings_t::set("testKey_3", "set",          NULL_STR, "*.md");
		settings_t::set("testKey_4", "other",      "source.c");
		settings_t::set("testKey_4", "set",      "text.plain");
		settings_t::set("testKey_5", "other",              "", "*.txt");
		settings_t::set("testKey_5", "set",                "", "*.md");
	}

public:
	void test_save_settings ()
	{
		TS_ASSERT_EQUALS(settings_for_path(                                ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.md"                 ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.txt"                ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text"         ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source"       ).get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_1", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr"         ).get("testKey_1", "unset"),   "set");

		TS_ASSERT_EQUALS(settings_for_path(                                ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.md"                 ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.txt"                ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text"         ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source"       ).get("testKey_2", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_2", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr"         ).get("testKey_2", "unset"), "unset");

		TS_ASSERT_EQUALS(settings_for_path(                                ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.md"                 ).get("testKey_3", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.txt"                ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text"         ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source"       ).get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_3", "unset"), "unset");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr"         ).get("testKey_3", "unset"), "unset");

		TS_ASSERT_EQUALS(settings_for_path(                                ).get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("dummy.md"                      ).get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("dummy.txt"                     ).get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "text"         ).get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_4", "unset"), "other");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "source"       ).get("testKey_4", "unset"), "other");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_4", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path(NULL_STR,        "attr"         ).get("testKey_4", "unset"),   "set");

		TS_ASSERT_EQUALS(settings_for_path(                                ).get("testKey_5", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.md"                 ).get("testKey_5", "unset"),   "set");
		TS_ASSERT_EQUALS(settings_for_path("/tmp/dummy.txt"                ).get("testKey_5", "unset"), "other");
	}
};
