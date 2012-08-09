#include <test/jail.h>
#include <settings/settings.h>

class SettingsTests : public CxxTest::TestSuite
{
public:
	void test_settings ()
	{
		test::jail_t jail;
		jail.set_content(".tm_properties", "testSetting = Hello");
		jail.set_content("dir/.tm_properties", "[ *.cc ]\ntestSetting = '${testSetting}, world!'");

		TS_ASSERT_EQUALS(settings_for_path(jail.path("dir/file.cc")).get("testSetting"), "Hello, world!");
		TS_ASSERT_EQUALS(settings_for_path(jail.path("dir/file.h")).get("testSetting"), "Hello");
	}

	void test_sections ()
	{
		test::jail_t jail;
		jail.set_content(".tm_properties", "testSetting = 7\n[ *.cc; *.h ]\ntestSetting = 6\n[ *.mm ]\ntestSetting = 5\n");

		TS_ASSERT_EQUALS(settings_for_path(jail.path("file.m")).get("testSetting",  3), 7);
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file.h")).get("testSetting",  3), 6);
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file.cc")).get("testSetting", 3), 6);
		TS_ASSERT_EQUALS(settings_for_path(jail.path("file.mm")).get("testSetting", 3), 5);
	}

	void test_conversions ()
	{
		test::jail_t jail;
		jail.set_content(".tm_properties", "bool = true\nint = 42\nfloat = 5.5\nstring = 'charlie'\n");
		settings_t s = settings_for_path(jail.path("file.cc"));

		TS_ASSERT_EQUALS(s.get("bool",        false),      true);
		TS_ASSERT_EQUALS(s.get("int",             7),        42);
		TS_ASSERT_EQUALS(s.get("float",         1.1),       5.5);
		TS_ASSERT_EQUALS(s.get("string",    "sheen"), "charlie");

		TS_ASSERT_EQUALS(s.get("Nonbool",     false),     false);
		TS_ASSERT_EQUALS(s.get("Nonint",          7),         7);
		TS_ASSERT_EQUALS(s.get("Nonfloat",      1.1),       1.1);
		TS_ASSERT_EQUALS(s.get("Nonstring", "sheen"),   "sheen");
	}

	void test_coercion ()
	{
		test::jail_t jail;
		jail.set_content(".tm_properties", "int = 42\nfloat = 42.0\nstring_1 = '42'\nstring_2 = '42.0'\n");
		settings_t s = settings_for_path(jail.path("file.cc"));

		TS_ASSERT_EQUALS(s.get("int",    0),   42);
		TS_ASSERT_EQUALS(s.get("int",  0.0), 42.0);
		TS_ASSERT_EQUALS(s.get("int",  "0"), "42");

		TS_ASSERT_EQUALS(s.get("float",    0),     42);
		TS_ASSERT_EQUALS(s.get("float",  0.0),   42.0);
		TS_ASSERT_EQUALS(s.get("float",  "0"), "42.0");

		TS_ASSERT_EQUALS(s.get("string_1",    0),     42);
		TS_ASSERT_EQUALS(s.get("string_1",  0.0),   42.0);
		TS_ASSERT_EQUALS(s.get("string_1",  "0"),   "42");

		TS_ASSERT_EQUALS(s.get("string_2",    0),     42);
		TS_ASSERT_EQUALS(s.get("string_2",  0.0),   42.0);
		TS_ASSERT_EQUALS(s.get("string_2",  "0"), "42.0");
	}
};
