#include <regexp/format_string.h>

class FormatStringTests : public CxxTest::TestSuite
{
public:
	void test_format_string ()
	{
		using format_string::replace;
		TS_ASSERT_EQUALS(replace("æbleGRØD", ".+", "»${0:/upcase}«"),             "»ÆBLEGRØD«");
		TS_ASSERT_EQUALS(replace("æbleGRØD", ".+", "»${0:/downcase}«"),           "»æblegrød«");
		TS_ASSERT_EQUALS(replace("æbleGRØD", ".+", "»${0:/capitalize}«"),         "»Æblegrød«");
		TS_ASSERT_EQUALS(replace("æbleGRØD", ".+", "»${0:/asciify}«"),            "»aebleGROD«");
		TS_ASSERT_EQUALS(replace("æbleGRØD", ".+", "»${0:/capitalize/asciify}«"), "»AEblegrod«");
	}

	void test_legacy_conditions ()
	{
		using format_string::replace;
		TS_ASSERT_EQUALS(replace("foo bar", "(foo)? bar", "(?1:baz)"),             "baz");
		TS_ASSERT_EQUALS(replace("fud bar", "(foo)? bar", "(?1:baz)"),             "fud");
		TS_ASSERT_EQUALS(replace("fud bar", "(foo)? bar", "(?1:baz: buz)"),        "fud buz");
		TS_ASSERT_EQUALS(replace("foo bar", "(foo)? bar", "(?1:baz"),              "(?1:baz");
		TS_ASSERT_EQUALS(replace("foo bar", "(foo)? bar", "(?1:baz:"),             "(?1:baz:");
		TS_ASSERT_EQUALS(replace("foo bar", "(foo)? bar", "(?n:baz)"),             "(?n:baz)");
		TS_ASSERT_EQUALS(replace("foo bar", "(foo)? bar", "(?n:baz:)"),            "(?n:baz:)");
	}
};
