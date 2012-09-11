#include <regexp/glob.h>
#include <text/format.h>

class GlobTests : public CxxTest::TestSuite
{
	static std::string expand (std::string const& str)
	{
		return text::join(path::expand_braces(str), ":");
	}

public:
	void test_glob ()
	{
		TS_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.cc"  ));
		TS_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.mm"  ));
		TS_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.h"   ));
		TS_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.cch" ));
		TS_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.hh"  ));
		TS_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.hcc" ));

		TS_ASSERT( path::glob_t("*.[ch]").does_match("test.c"   ));
		TS_ASSERT( path::glob_t("*.[ch]").does_match("test.h"   ));
		TS_ASSERT(!path::glob_t("*.[ch]").does_match("test.cc"  ));
		TS_ASSERT(!path::glob_t("*.[ch]").does_match("test.d"   ));

		TS_ASSERT( path::glob_t("*.[c-h]").does_match("test.d"  ));
		TS_ASSERT(!path::glob_t("*.[c-h]").does_match("test.i"  ));

		TS_ASSERT( path::glob_t("test.?").does_match("test.d"   ));
		TS_ASSERT(!path::glob_t("test.?").does_match("test.cc"  ));

		TS_ASSERT( path::glob_t("test.\\?").does_match("test.?" ));
		TS_ASSERT(!path::glob_t("test.\\?").does_match("test.c" ));

		TS_ASSERT( path::glob_t("test.").does_match("test."     ));
		TS_ASSERT(!path::glob_t("test.").does_match("testA"     ));

		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cc"    ));
		TS_ASSERT(!path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hh"    ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.c"     ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cpp"   ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cxx"   ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.h"     ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hpp"   ));
		TS_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hxx"   ));

		TS_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("test.cc"      ));
		TS_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("test.h"       ));
		TS_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("Makefile"     ));
		TS_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("test.txt"     ));
		TS_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("Makefile.txt" ));
		TS_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("test.Makefile"));
		TS_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match(".test.cc"     ));
	}

	void test_glob_hidden_files ()
	{
		TS_ASSERT( path::glob_t(".*").does_match(".htaccess"));
		TS_ASSERT(!path::glob_t("*" ).does_match(".htaccess"));
		TS_ASSERT( path::glob_t("{,.}*").does_match("test"));
		TS_ASSERT( path::glob_t("{,.}*").does_match(".htaccess"));

		TS_ASSERT( path::glob_t("*"        ).does_match("foo.txt"));
		TS_ASSERT( path::glob_t("foo*"     ).does_match("foo.txt"));
		TS_ASSERT( path::glob_t("foo/*"    ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("foo/bar*" ).does_match("foo/bar.txt"));
		TS_ASSERT(!path::glob_t("*"        ).does_match(".txt"));
		TS_ASSERT(!path::glob_t("foo/*"    ).does_match("foo/.txt"));
		TS_ASSERT(!path::glob_t("foo/bar/*").does_match("foo/bar/.txt"));

		TS_ASSERT( path::glob_t("cache/*"   ).does_match("cache/test.cc"));
		TS_ASSERT( path::glob_t("cache/**"  ).does_match("cache/test.cc"));
		TS_ASSERT( path::glob_t("cache/**"  ).does_match("cache/foo/test.cc"));
		TS_ASSERT( path::glob_t("cache/**/*").does_match("cache/foo/test.cc"));
		TS_ASSERT(!path::glob_t("cache/*"   ).does_match("cache/.htaccess"));
		TS_ASSERT(!path::glob_t("cache/**"  ).does_match("cache/.htaccess"));
		TS_ASSERT(!path::glob_t("cache/**"  ).does_match("cache/foo/.htaccess"));
		TS_ASSERT(!path::glob_t("cache/**/*").does_match("cache/foo/.htaccess"));

		TS_ASSERT(!path::glob_t("!cache/*"   ).does_match("cache/test.cc"));
		TS_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/test.cc"));
		TS_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/foo/test.cc"));
		TS_ASSERT(!path::glob_t("!cache/**/*").does_match("cache/foo/test.cc"));
		TS_ASSERT(!path::glob_t("!cache/*"   ).does_match("cache/.htaccess"));
		TS_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/.htaccess"));
		TS_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/foo/.htaccess"));
		TS_ASSERT(!path::glob_t("!cache/**/*").does_match("cache/foo/.htaccess"));
	}

	void test_glob_anchoring ()
	{
		TS_ASSERT( path::glob_t("foo").does_match("foo"));
		TS_ASSERT(!path::glob_t("foo").does_match("foo.txt"));
		TS_ASSERT(!path::glob_t("foo").does_match("me foo"));
		TS_ASSERT( path::glob_t("foo").does_match("me/foo"));
		TS_ASSERT(!path::glob_t("foo").does_match("foo/bar"));
		TS_ASSERT( path::glob_t("foo.*").does_match("foo.txt"));
		TS_ASSERT(!path::glob_t("foo.*").does_match("me foo.txt"));
		TS_ASSERT( path::glob_t("foo.*").does_match("me/foo.txt"));
	}

	void test_glob_paths ()
	{
		TS_ASSERT( path::glob_t("*"       ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("f*/*"    ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("*.txt"   ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("*/*.txt" ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("f*/*.txt").does_match("foo/bar.txt"));
		TS_ASSERT(!path::glob_t("f*.txt"  ).does_match("foo/bar.txt"));
		TS_ASSERT(!path::glob_t("f*"      ).does_match("foo/bar.txt"));
	}

	void test_glob_dir_matching ()
	{
		TS_ASSERT( path::glob_t("**"          ).does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("**/*"        ).does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("f**"         ).does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("foo/**"      ).does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("foo/**/*.txt").does_match("foo/fud.txt"));
		TS_ASSERT( path::glob_t("foo/**/*.txt").does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("**.txt"      ).does_match("fud.txt"));
		TS_ASSERT( path::glob_t("**.txt"      ).does_match("foo/bar/fud.txt"));
		TS_ASSERT( path::glob_t("**/*.txt"    ).does_match("fud.txt"));
		TS_ASSERT( path::glob_t("f**.txt"     ).does_match("foo/bar.txt"));
		TS_ASSERT(!path::glob_t("f*.txt"      ).does_match("foo/bar.txt"));
		TS_ASSERT( path::glob_t("f**bar.txt"  ).does_match("fbar.txt"));
		TS_ASSERT( path::glob_t("f**bar.txt"  ).does_match("foo/bar.txt"));
		TS_ASSERT(!path::glob_t("f**bar.txt"  ).does_match("foo/.bar.txt"));
	}

	void test_brace_expansion ()
	{
		using path::expand_braces;

		TS_ASSERT_EQUALS(expand_braces("{foo}").size(),             1);
		TS_ASSERT_EQUALS(expand_braces("{foo}").at(0),        "{foo}");

		TS_ASSERT_EQUALS(expand_braces("{foo,bar}").size(),         2);
		TS_ASSERT_EQUALS(expand_braces("{foo,bar}").at(0),      "foo");
		TS_ASSERT_EQUALS(expand_braces("{foo,bar}").at(1),      "bar");

		TS_ASSERT_EQUALS(expand_braces("base.{cc,h}-suffix").size(), 2);
		TS_ASSERT_EQUALS(expand_braces("base.{cc,h}-suffix")[0],     "base.cc-suffix");
		TS_ASSERT_EQUALS(expand_braces("base.{cc,h}-suffix")[1],     "base.h-suffix");

		TS_ASSERT_EQUALS(expand_braces("foo{bar")[0],                "foo{bar");
		TS_ASSERT_EQUALS(expand_braces("foo{bar}")[0],               "foo{bar}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar},cc}").size(),       1);
		TS_ASSERT_EQUALS(expand_braces("foo{bar},cc}")[0],           "foo{bar},cc}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar\\},cc}").size(),     2);
		TS_ASSERT_EQUALS(expand_braces("foo{bar\\},cc}")[0],         "foobar}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar\\},cc}")[1],         "foocc");
		TS_ASSERT_EQUALS(expand_braces("foo\\{bar}")[0],             "foo{bar}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar\\}")[0],             "foo{bar}");
		TS_ASSERT_EQUALS(expand_braces("foo\\{bar,cc}")[0],          "foo{bar,cc}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar\\,cc}")[0],          "foo{bar,cc}");
		TS_ASSERT_EQUALS(expand_braces("foo{bar,cc\\}")[0],          "foo{bar,cc}");
		TS_ASSERT_EQUALS(expand_braces("foo,bar")[0],                "foo,bar");
		TS_ASSERT_EQUALS(expand_braces("foo\\,bar")[0],              "foo,bar");
		TS_ASSERT_EQUALS(expand_braces("foo}")[0],                   "foo}");
		TS_ASSERT_EQUALS(expand_braces("foo\\}")[0],                 "foo}");

		TS_ASSERT_EQUALS(expand_braces("foo\\bar")[0],               "foo\\bar");
		TS_ASSERT_EQUALS(expand_braces("foo\\\\bar")[0],             "foo\\bar");

		TS_ASSERT_EQUALS(expand_braces("foo{,bar}").size(),          2);
		TS_ASSERT_EQUALS(expand_braces("foo{,bar}")[0],              "foo");
		TS_ASSERT_EQUALS(expand_braces("foo{,bar}")[1],              "foobar");

		TS_ASSERT_EQUALS(expand_braces("{a,b{c,d}}").size(),         3);
		TS_ASSERT_EQUALS(expand_braces("{a,b{c,d}}")[0],             "a");
		TS_ASSERT_EQUALS(expand_braces("{a,b{c,d}}")[1],             "bc");
		TS_ASSERT_EQUALS(expand_braces("{a,b{c,d}}")[2],             "bd");

		TS_ASSERT_EQUALS(expand_braces("{a,b}{c,d}").size(),         4);
		TS_ASSERT_EQUALS(expand_braces("{a,b}{c,d}")[0],             "ac");
		TS_ASSERT_EQUALS(expand_braces("{a,b}{c,d}")[1],             "ad");
		TS_ASSERT_EQUALS(expand_braces("{a,b}{c,d}")[2],             "bc");
		TS_ASSERT_EQUALS(expand_braces("{a,b}{c,d}")[3],             "bd");
	}

	void test_glob_brace_expansion ()
	{
		TS_ASSERT_EQUALS(expand("{a,b,{c},d}"),     "a,d}:b,d}:{c,d}");
		TS_ASSERT_EQUALS(expand("{a,b,\\{c\\},d}"), "a:b:{c}:d");
	}

	void test_glob_negating ()
	{
		TS_ASSERT(!path::glob_t("!html"          ).does_match("/path/to/html"));
		TS_ASSERT( path::glob_t("!html"          ).does_match("/path/to/html/foo"));
		TS_ASSERT(!path::glob_t("!html/*"        ).does_match("/path/to/html/fud.txt"));
		TS_ASSERT( path::glob_t("!html/*"        ).does_match("/path/to/html/foo/fud.txt"));
		TS_ASSERT(!path::glob_t("!html/**"       ).does_match("/path/to/html/foo/fud.txt"));
		TS_ASSERT(!path::glob_t("!html/**/*"     ).does_match("/path/to/html/foo/fud.txt"));
		TS_ASSERT(!path::glob_t("!html/**/*.txt" ).does_match("/path/to/html/foo/fud.txt"));
		TS_ASSERT( path::glob_t("!html/**/*.txt" ).does_match("/path/to/html/foo/fud.php"));
		TS_ASSERT( path::glob_t("!html"          ).does_match("/path/to/page"));
		TS_ASSERT( path::glob_t("!html/*"        ).does_match("/path/to/page/fud.txt"));
		TS_ASSERT( path::glob_t("!html/**"       ).does_match("/path/to/page/foo/fud.txt"));
		TS_ASSERT( path::glob_t("!html/**/*"     ).does_match("/path/to/page/foo/fud.txt"));
		TS_ASSERT( path::glob_t("!html/**/*.txt" ).does_match("/path/to/page/foo/fud.txt"));
	}
};
