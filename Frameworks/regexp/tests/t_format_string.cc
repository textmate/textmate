#include <regexp/format_string.h>

void test_format_string ()
{
	using format_string::replace;
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/upcase}«"),             "»ÆBLEGRØD«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/downcase}«"),           "»æblegrød«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/capitalize}«"),         "»Æblegrød«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/asciify}«"),            "»aebleGROD«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/capitalize/asciify}«"), "»AEblegrod«");
}

void test_legacy_conditions ()
{
	using format_string::replace;
	OAK_ASSERT_EQ(replace("foo bar", "(foo)? bar", "(?1:baz)"),             "baz");
	OAK_ASSERT_EQ(replace("fud bar", "(foo)? bar", "(?1:baz)"),             "fud");
	OAK_ASSERT_EQ(replace("fud bar", "(foo)? bar", "(?1:baz: buz)"),        "fud buz");
	OAK_ASSERT_EQ(replace("foo bar", "(foo)? bar", "(?1:baz"),              "(?1:baz");
	OAK_ASSERT_EQ(replace("foo bar", "(foo)? bar", "(?1:baz:"),             "(?1:baz:");
	OAK_ASSERT_EQ(replace("foo bar", "(foo)? bar", "(?n:baz)"),             "(?n:baz)");
	OAK_ASSERT_EQ(replace("foo bar", "(foo)? bar", "(?n:baz:)"),            "(?n:baz:)");
}

void test_escape_format_string ()
{
	using format_string::escape;
	using format_string::expand;
	OAK_ASSERT_EQ(escape("\t\n\r\\q"),               "\\t\\n\\r\\q");
	OAK_ASSERT_EQ(expand(escape("${var}")),                "${var}");
	OAK_ASSERT_EQ(expand(escape("foo\n")),                  "foo\n");
	OAK_ASSERT_EQ(expand(escape("foo\\n")),                "foo\\n");
	OAK_ASSERT_EQ(expand(escape("\\No-Escape")),      "\\No-Escape");
	OAK_ASSERT_EQ(expand(escape("(?bla)")),                "(?bla)");
	OAK_ASSERT_EQ(expand(escape("Escape\\\\me")),    "Escape\\\\me");
	OAK_ASSERT_EQ(expand(escape("(?1:baz: buz)")),  "(?1:baz: buz)");
}
