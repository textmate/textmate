#include <test/jail.h>
#include <settings/src/settings.h>

void test_settings ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "testSetting = Hello");
	jail.set_content("dir/.tm_properties", "[ *.cc ]\ntestSetting = '${testSetting}, world!'");

	OAK_ASSERT_EQ(settings_for_path(jail.path("dir/file.cc")).get("testSetting"), "Hello, world!");
	OAK_ASSERT_EQ(settings_for_path(jail.path("dir/file.h")).get("testSetting"), "Hello");
}

void test_sections ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "testSetting = 7\n[ *.cc; *.h ]\ntestSetting = 6\n[ *.mm ]\ntestSetting = 5\n");

	OAK_ASSERT_EQ(settings_for_path(jail.path("file.m")).get("testSetting",  3), 7);
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.h")).get("testSetting",  3), 6);
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.cc")).get("testSetting", 3), 6);
	OAK_ASSERT_EQ(settings_for_path(jail.path("file.mm")).get("testSetting", 3), 5);
}

void test_sections_with_only_directory ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "testSetting = parent\n[ folder/** ]\ntestSetting = child\n");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR, "", jail.path()).get("testSetting"), "parent");
	OAK_ASSERT_EQ(settings_for_path(NULL_STR, "", jail.path("folder")).get("testSetting"), "child");
}

void test_conversions ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "bool = true\nint = 42\nfloat = 5.5\nstring = 'charlie'\n");
	settings_t s = settings_for_path(jail.path("file.cc"));

	OAK_ASSERT_EQ(s.get("bool",        false),      true);
	OAK_ASSERT_EQ(s.get("int",             7),        42);
	OAK_ASSERT_EQ(s.get("float",         1.1),       5.5);
	OAK_ASSERT_EQ(s.get("string",    "sheen"), "charlie");

	OAK_ASSERT_EQ(s.get("Nonbool",     false),     false);
	OAK_ASSERT_EQ(s.get("Nonint",          7),         7);
	OAK_ASSERT_EQ(s.get("Nonfloat",      1.1),       1.1);
	OAK_ASSERT_EQ(s.get("Nonstring", "sheen"),   "sheen");
}

void test_coercion ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "int = 42\nfloat = 42.0\nstring_1 = '42'\nstring_2 = '42.0'\n");
	settings_t s = settings_for_path(jail.path("file.cc"));

	OAK_ASSERT_EQ(s.get("int",    0),   42);
	OAK_ASSERT_EQ(s.get("int",  0.0), 42.0);
	OAK_ASSERT_EQ(s.get("int",  "0"), "42");

	OAK_ASSERT_EQ(s.get("float",    0),     42);
	OAK_ASSERT_EQ(s.get("float",  0.0),   42.0);
	OAK_ASSERT_EQ(s.get("float",  "0"), "42.0");

	OAK_ASSERT_EQ(s.get("string_1",    0),     42);
	OAK_ASSERT_EQ(s.get("string_1",  0.0),   42.0);
	OAK_ASSERT_EQ(s.get("string_1",  "0"),   "42");

	OAK_ASSERT_EQ(s.get("string_2",    0),     42);
	OAK_ASSERT_EQ(s.get("string_2",  0.0),   42.0);
	OAK_ASSERT_EQ(s.get("string_2",  "0"), "42.0");
}
