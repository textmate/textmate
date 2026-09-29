#include <regexp/src/format_string.h>

void test_format_string ()
{
	using format_string::replace;
	OAK_ASSERT_EQ(replace("Résumé",   ".+", "»${0:/asciify}«"),            "»Resume«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/upcase}«"),             "»ÆBLEGRØD«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/downcase}«"),           "»æblegrød«");
	OAK_ASSERT_EQ(replace("æbleGRØD", ".+", "»${0:/asciify}«"),            "»aebleGROD«");
	OAK_ASSERT_EQ(replace("æblegrød", ".+", "»${0:/capitalize}«"),         "»Æblegrød«");
	OAK_ASSERT_EQ(replace("æblegrød", ".+", "»${0:/capitalize/asciify}«"), "»AEblegrod«");
}

void test_capitalize ()
{
	using format_string::replace;
	OAK_ASSERT_EQ(replace("this is a title", ".+", "${0:/capitalize}"),                    "This is a Title");
	OAK_ASSERT_EQ(replace("word-based capitalization", ".+", "${0:/capitalize}"),          "Word-based Capitalization");
	OAK_ASSERT_EQ(replace("# 2014-08-22: it works now #", ".+", "${0:/capitalize}"),       "# 2014-08-22: It Works Now #");
	OAK_ASSERT_EQ(replace("# 2014-08-22: it works now again #", ".+", "${0:/capitalize}"), "# 2014-08-22: It Works now Again #");
	OAK_ASSERT_EQ(replace("my NSTableView subclass", ".+", "${0:/capitalize}"),            "My NSTableView Subclass");
	OAK_ASSERT_EQ(replace("This Is The Wrong", ".+", "${0:/capitalize}"),                  "This is the Wrong");
	OAK_ASSERT_EQ(replace("THIS IS THE WRONG", ".+", "${0:/capitalize}"),                  "This is the Wrong");
	OAK_ASSERT_EQ(replace("RSA", ".+", "${0:/capitalize}"),                                "Rsa");
}

void test_variables ()
{
	std::map<std::string, std::string> variables{
		{ "a",    "hello"         },
		{ "b",    " "             },
		{ "c",    "world"         },
		{ "d",    "hell"          },
		{ "dir",  "/path/to"      },
		{ "path", "/path/to/file" },
	};
	OAK_ASSERT_EQ("hello world", format_string::expand("$a$b$c",                  variables));
	OAK_ASSERT_EQ("hello world", format_string::expand("${a}${b}${c}",            variables));
	OAK_ASSERT_EQ("hi world",    format_string::expand("${a/hello/hi/}${b}${c}",  variables));
	OAK_ASSERT_EQ("hi’yo world", format_string::expand("${a/${d}/hi’y/}${b}${c}", variables));
	OAK_ASSERT_EQ("file",        format_string::expand("${path/^.*\\///}",        variables));
	OAK_ASSERT_EQ("file",        format_string::expand("${path/${dir}.//}",       variables));
	OAK_ASSERT_EQ("file",        format_string::expand("${path/${dir}\\///}",     variables));
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

void test_control_codes ()
{
	using format_string::expand;
	OAK_ASSERT_EQ("\t",         expand("\\t"));
	OAK_ASSERT_EQ("\r",         expand("\\r"));
	OAK_ASSERT_EQ("\n",         expand("\\n"));
	OAK_ASSERT_EQ("\\x",        expand("\\x"));
	OAK_ASSERT_EQ("\\x{foo}",   expand("\\x{foo}"));
	OAK_ASSERT_EQ("\u2014",     expand("\\x{2014}"));
	OAK_ASSERT_EQ("\U00020EF5", expand("\\x{20EF5}"));
	OAK_ASSERT_EQ("\U0010FFFF", expand("\\x{0010FFFF}"));
	OAK_ASSERT_EQ("Æblegrød",   expand("\\xc3\\x86blegr\\xC3\\xB8d"));
}

void test_implicit_variables ()
{
	// We do not want $1 from root match to be inherited by format string
	using format_string::replace;
	OAK_ASSERT_EQ(replace("=",   "(=+)", "${1/=(=)?(=)?/${2:?2:${1:?1:0}}/}"), "0");
	OAK_ASSERT_EQ(replace("==",  "(=+)", "${1/=(=)?(=)?/${2:?2:${1:?1:0}}/}"), "1");
	OAK_ASSERT_EQ(replace("===", "(=+)", "${1/=(=)?(=)?/${2:?2:${1:?1:0}}/}"), "2");
}
