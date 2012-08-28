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

	void test_escape_format_string ()
	{
		using format_string::escape;
		using format_string::expand;
		TS_ASSERT_EQUALS(escape("\t\n\r\\q"),               "\\t\\n\\r\\q");
		TS_ASSERT_EQUALS(expand(escape("${var}")),                "${var}");
		TS_ASSERT_EQUALS(expand(escape("foo\n")),                  "foo\n");
		TS_ASSERT_EQUALS(expand(escape("foo\\n")),                "foo\\n");
		TS_ASSERT_EQUALS(expand(escape("\\No-Escape")),      "\\No-Escape");
		TS_ASSERT_EQUALS(expand(escape("(?bla)")),                "(?bla)");
		TS_ASSERT_EQUALS(expand(escape("Escape\\\\me")),    "Escape\\\\me");
		TS_ASSERT_EQUALS(expand(escape("(?1:baz: buz)")),  "(?1:baz: buz)");
	}
};
