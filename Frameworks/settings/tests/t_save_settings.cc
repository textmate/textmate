#include <settings/src/settings.h>
#include <test/jail.h>

void test_save_settings ()
{
	test::jail_t jail;
	settings_t::set_default_settings_path(jail.path("default"));
	settings_t::set_global_settings_path(jail.path("global"));

	settings_t::set("testKey_1", "set");
	settings_t::set("testKey_2", "set",   "attr.untitled");
	settings_t::set("testKey_3", "set",          NULL_STR, "*.md");
	settings_t::set("testKey_4", "other",      "source.c");
	settings_t::set("testKey_4", "set",      "text.plain");
	settings_t::set("testKey_5", "other",              "", "*.txt");
	settings_t::set("testKey_5", "set",                "", "*.md");

	settings_t::set("fileType", "source.ruby",       NULL_STR, "*.rb");
	settings_t::set("fileType", "source.rspec",      NULL_STR, "*_spec.rb");
	settings_t::set("fileType", "text.plain",        NULL_STR, "*.txt");
	settings_t::set("fileType", "source.cmake",      NULL_STR, "CMakeLists.txt");
	settings_t::set("fileType", "source.config",     NULL_STR, "*.config");
	settings_t::set("fileType", "source.git-config", NULL_STR, ".config");

	// ==================
	// = Setup Complete =
	// ==================

	OAK_ASSERT_EQ(settings_for_path("/path/to/foo.rb"         ).get("fileType", "unset"), "source.ruby");
	OAK_ASSERT_EQ(settings_for_path("/path/to/foo_spec.rb"    ).get("fileType", "unset"), "source.rspec");
	OAK_ASSERT_EQ(settings_for_path("/path/to/foo.txt"        ).get("fileType", "unset"), "text.plain");
	OAK_ASSERT_EQ(settings_for_path("/path/to/CMakeLists.txt" ).get("fileType", "unset"), "source.cmake");
	OAK_ASSERT_EQ(settings_for_path("/path/to/foo.config"     ).get("fileType", "unset"), "source.config");
	OAK_ASSERT_EQ(settings_for_path("/path/git/.config"       ).get("fileType", "unset"), "source.git-config");

	OAK_ASSERT_EQ(settings_for_path(                                ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.md"                 ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.txt"                ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text"         ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source"       ).get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_1", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr"         ).get("testKey_1", "unset"),   "set");

	OAK_ASSERT_EQ(settings_for_path(                                ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.md"                 ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.txt"                ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text"         ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source"       ).get("testKey_2", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_2", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr"         ).get("testKey_2", "unset"), "unset");

	OAK_ASSERT_EQ(settings_for_path(                                ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.md"                 ).get("testKey_3", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.txt"                ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text"         ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source"       ).get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_3", "unset"), "unset");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr"         ).get("testKey_3", "unset"), "unset");

	OAK_ASSERT_EQ(settings_for_path(                                ).get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("dummy.md"                      ).get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("dummy.txt"                     ).get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text.plain"   ).get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "text"         ).get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source.c"     ).get("testKey_4", "unset"), "other");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "source"       ).get("testKey_4", "unset"), "other");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr.untitled").get("testKey_4", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR,        "attr"         ).get("testKey_4", "unset"),   "set");

	OAK_ASSERT_EQ(settings_for_path(                                ).get("testKey_5", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.md"                 ).get("testKey_5", "unset"),   "set");
	OAK_ASSERT_EQ(settings_for_path("/tmp/dummy.txt"                ).get("testKey_5", "unset"), "other");
}
