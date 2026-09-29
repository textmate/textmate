#include <regexp/src/glob.h>
#include <text/src/format.h>

static std::string expand (std::string const& str)
{
	return text::join(path::expand_braces(str), ":");
}

static std::vector<std::string> sorted (std::string const& str)
{
	auto v = path::expand_braces(str);
	std::sort(v.begin(), v.end());
	return v;
}

void test_empty_glob ()
{
	OAK_ASSERT(!path::glob_t("").does_match("foo"));
}

void test_glob ()
{
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.cc"  ));
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.mm"  ));
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}").does_match("test.h"   ));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.cch" ));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.hh"  ));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}").does_match("test.hcc" ));

	OAK_ASSERT( path::glob_t("*.[ch]").does_match("test.c"   ));
	OAK_ASSERT( path::glob_t("*.[ch]").does_match("test.h"   ));
	OAK_ASSERT(!path::glob_t("*.[ch]").does_match("test.cc"  ));
	OAK_ASSERT(!path::glob_t("*.[ch]").does_match("test.d"   ));

	OAK_ASSERT( path::glob_t("*.[c-h]").does_match("test.d"  ));
	OAK_ASSERT(!path::glob_t("*.[c-h]").does_match("test.i"  ));

	OAK_ASSERT( path::glob_t("test.?").does_match("test.d"   ));
	OAK_ASSERT(!path::glob_t("test.?").does_match("test.cc"  ));

	OAK_ASSERT( path::glob_t("test.\\?").does_match("test.?" ));
	OAK_ASSERT(!path::glob_t("test.\\?").does_match("test.c" ));

	OAK_ASSERT( path::glob_t("test.").does_match("test."     ));
	OAK_ASSERT(!path::glob_t("test.").does_match("testA"     ));

	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cc"    ));
	OAK_ASSERT(!path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hh"    ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.c"     ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cpp"   ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.cxx"   ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.h"     ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hpp"   ));
	OAK_ASSERT( path::glob_t("*.{cc,{c,h}{,pp,xx}}").does_match("test.hxx"   ));

	OAK_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("test.cc"      ));
	OAK_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("test.h"       ));
	OAK_ASSERT( path::glob_t("{*.{cc,h},Makefile}").does_match("Makefile"     ));
	OAK_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("test.txt"     ));
	OAK_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("Makefile.txt" ));
	OAK_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match("test.Makefile"));
	OAK_ASSERT(!path::glob_t("{*.{cc,h},Makefile}").does_match(".test.cc"     ));
}

void test_glob_case_sensitive ()
{
	OAK_ASSERT( path::glob_t("Makefile", false,  true).does_match("Makefile"));
	OAK_ASSERT(!path::glob_t("Makefile", false,  true).does_match("makefile"));
	OAK_ASSERT( path::glob_t("Makefile", false, false).does_match("Makefile"));
	OAK_ASSERT( path::glob_t("Makefile", false, false).does_match("makefile"));
}

void test_glob_hidden_files ()
{
	OAK_ASSERT( path::glob_t(".*").does_match(".htaccess"));
	OAK_ASSERT(!path::glob_t("*" ).does_match(".htaccess"));
	OAK_ASSERT( path::glob_t("{,.}*").does_match("test"));
	OAK_ASSERT( path::glob_t("{,.}*").does_match(".htaccess"));

	OAK_ASSERT( path::glob_t("*file").does_match("file"));
	OAK_ASSERT( path::glob_t("fi*le").does_match("file"));
	OAK_ASSERT( path::glob_t("file*").does_match("file"));
	OAK_ASSERT(!path::glob_t("*.ext").does_match(".ext"));
	OAK_ASSERT( path::glob_t(".*ext").does_match(".ext"));
	OAK_ASSERT( path::glob_t(".ext*").does_match(".ext"));

	OAK_ASSERT( path::glob_t("*"        ).does_match("foo.txt"));
	OAK_ASSERT( path::glob_t("foo*"     ).does_match("foo.txt"));
	OAK_ASSERT( path::glob_t("foo/*"    ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("foo/bar*" ).does_match("foo/bar.txt"));
	OAK_ASSERT(!path::glob_t("*"        ).does_match(".txt"));
	OAK_ASSERT(!path::glob_t("foo/*"    ).does_match("foo/.txt"));
	OAK_ASSERT(!path::glob_t("foo/bar/*").does_match("foo/bar/.txt"));

	OAK_ASSERT( path::glob_t("cache/*"   ).does_match("cache/test.cc"));
	OAK_ASSERT( path::glob_t("cache/**"  ).does_match("cache/test.cc"));
	OAK_ASSERT( path::glob_t("cache/**"  ).does_match("cache/foo/test.cc"));
	OAK_ASSERT( path::glob_t("cache/**/*").does_match("cache/foo/test.cc"));
	OAK_ASSERT(!path::glob_t("cache/*"   ).does_match("cache/.htaccess"));
	OAK_ASSERT(!path::glob_t("cache/**"  ).does_match("cache/.htaccess"));
	OAK_ASSERT(!path::glob_t("cache/**"  ).does_match("cache/foo/.htaccess"));
	OAK_ASSERT(!path::glob_t("cache/**/*").does_match("cache/foo/.htaccess"));

	OAK_ASSERT(!path::glob_t("!cache/*"   ).does_match("cache/test.cc"));
	OAK_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/test.cc"));
	OAK_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/foo/test.cc"));
	OAK_ASSERT(!path::glob_t("!cache/**/*").does_match("cache/foo/test.cc"));
	OAK_ASSERT(!path::glob_t("!cache/*"   ).does_match("cache/.htaccess"));
	OAK_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/.htaccess"));
	OAK_ASSERT(!path::glob_t("!cache/**"  ).does_match("cache/foo/.htaccess"));
	OAK_ASSERT(!path::glob_t("!cache/**/*").does_match("cache/foo/.htaccess"));
}

void test_glob_anchoring ()
{
	OAK_ASSERT( path::glob_t("foo").does_match("foo"));
	OAK_ASSERT(!path::glob_t("foo").does_match("foo.txt"));
	OAK_ASSERT(!path::glob_t("foo").does_match("me foo"));
	OAK_ASSERT( path::glob_t("foo").does_match("me/foo"));
	OAK_ASSERT(!path::glob_t("foo").does_match("foo/bar"));
	OAK_ASSERT( path::glob_t("foo.*").does_match("foo.txt"));
	OAK_ASSERT(!path::glob_t("foo.*").does_match("me foo.txt"));
	OAK_ASSERT( path::glob_t("foo.*").does_match("me/foo.txt"));
}

void test_glob_paths ()
{
	OAK_ASSERT( path::glob_t("*"       ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("f*/*"    ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("*.txt"   ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("*/*.txt" ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("f*/*.txt").does_match("foo/bar.txt"));
	OAK_ASSERT(!path::glob_t("f*.txt"  ).does_match("foo/bar.txt"));
	OAK_ASSERT(!path::glob_t("f*"      ).does_match("foo/bar.txt"));
}

void test_glob_dir_matching ()
{
	OAK_ASSERT( path::glob_t("**"          ).does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("**/*"        ).does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("f**"         ).does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("foo/**"      ).does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("foo/**/*.txt").does_match("foo/fud.txt"));
	OAK_ASSERT( path::glob_t("foo/**/*.txt").does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("**.txt"      ).does_match("fud.txt"));
	OAK_ASSERT( path::glob_t("**.txt"      ).does_match("foo/bar/fud.txt"));
	OAK_ASSERT( path::glob_t("**/*.txt"    ).does_match("fud.txt"));
	OAK_ASSERT( path::glob_t("f**.txt"     ).does_match("foo/bar.txt"));
	OAK_ASSERT(!path::glob_t("f*.txt"      ).does_match("foo/bar.txt"));
	OAK_ASSERT( path::glob_t("f**bar.txt"  ).does_match("fbar.txt"));
	OAK_ASSERT( path::glob_t("f**bar.txt"  ).does_match("foo/bar.txt"));
	OAK_ASSERT(!path::glob_t("f**bar.txt"  ).does_match("foo/.bar.txt"));
}

void test_brace_expansion ()
{
	using path::expand_braces;

	OAK_ASSERT_EQ(expand_braces("{foo}").size(),             1);
	OAK_ASSERT_EQ(expand_braces("{foo}").at(0),        "{foo}");

	OAK_ASSERT_EQ(expand_braces("{foo,bar}").size(),         2);
	OAK_ASSERT_EQ(expand_braces("{foo,bar}").at(0),      "foo");
	OAK_ASSERT_EQ(expand_braces("{foo,bar}").at(1),      "bar");

	OAK_ASSERT_EQ(expand_braces("base.{cc,h}-suffix").size(), 2);
	OAK_ASSERT_EQ(expand_braces("base.{cc,h}-suffix")[0],     "base.cc-suffix");
	OAK_ASSERT_EQ(expand_braces("base.{cc,h}-suffix")[1],     "base.h-suffix");

	OAK_ASSERT_EQ(expand_braces("foo{bar")[0],                "foo{bar");
	OAK_ASSERT_EQ(expand_braces("foo{bar}")[0],               "foo{bar}");
	OAK_ASSERT_EQ(expand_braces("foo{bar},cc}").size(),       1);
	OAK_ASSERT_EQ(expand_braces("foo{bar},cc}")[0],           "foo{bar},cc}");
	OAK_ASSERT_EQ(expand_braces("foo{bar\\},cc}").size(),     2);
	OAK_ASSERT_EQ(expand_braces("foo{bar\\},cc}")[0],         "foobar}");
	OAK_ASSERT_EQ(expand_braces("foo{bar\\},cc}")[1],         "foocc");
	OAK_ASSERT_EQ(expand_braces("foo\\{bar}")[0],             "foo{bar}");
	OAK_ASSERT_EQ(expand_braces("foo{bar\\}")[0],             "foo{bar}");
	OAK_ASSERT_EQ(expand_braces("foo\\{bar,cc}")[0],          "foo{bar,cc}");
	OAK_ASSERT_EQ(expand_braces("foo{bar\\,cc}")[0],          "foo{bar,cc}");
	OAK_ASSERT_EQ(expand_braces("foo{bar,cc\\}")[0],          "foo{bar,cc}");
	OAK_ASSERT_EQ(expand_braces("foo,bar")[0],                "foo,bar");
	OAK_ASSERT_EQ(expand_braces("foo\\,bar")[0],              "foo,bar");
	OAK_ASSERT_EQ(expand_braces("foo}")[0],                   "foo}");
	OAK_ASSERT_EQ(expand_braces("foo\\}")[0],                 "foo}");

	OAK_ASSERT_EQ(expand_braces("foo\\bar")[0],               "foo\\bar");
	OAK_ASSERT_EQ(expand_braces("foo\\\\bar")[0],             "foo\\bar");

	OAK_ASSERT_EQ(expand_braces("foo{,bar}").size(),          2);
	OAK_ASSERT_EQ(expand_braces("foo{,bar}")[0],              "foo");
	OAK_ASSERT_EQ(expand_braces("foo{,bar}")[1],              "foobar");

	OAK_ASSERT_EQ(expand_braces("{a,b{c,d}}").size(),         3);
	OAK_ASSERT_EQ(expand_braces("{a,b{c,d}}")[0],             "a");
	OAK_ASSERT_EQ(expand_braces("{a,b{c,d}}")[1],             "bc");
	OAK_ASSERT_EQ(expand_braces("{a,b{c,d}}")[2],             "bd");

	OAK_ASSERT_EQ(sorted("{a,b}{c,d}").size(),                4);
	OAK_ASSERT_EQ(sorted("{a,b}{c,d}")[0],                    "ac");
	OAK_ASSERT_EQ(sorted("{a,b}{c,d}")[1],                    "ad");
	OAK_ASSERT_EQ(sorted("{a,b}{c,d}")[2],                    "bc");
	OAK_ASSERT_EQ(sorted("{a,b}{c,d}")[3],                    "bd");
}

void test_glob_brace_expansion ()
{
	OAK_ASSERT_EQ(expand("{a,b,{c},d}"),     "a:b:{c}:d");
	OAK_ASSERT_EQ(expand("{a,b,\\{c\\},d}"), "a:b:{c}:d");
}

void test_glob_negating ()
{
	OAK_ASSERT(!path::glob_t("!html"          ).does_match("/path/to/html"));
	OAK_ASSERT( path::glob_t("!html"          ).does_match("/path/to/html/foo"));
	OAK_ASSERT(!path::glob_t("!html/*"        ).does_match("/path/to/html/fud.txt"));
	OAK_ASSERT( path::glob_t("!html/*"        ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("!html/**"       ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("!html/**/*"     ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("!html/**/*.txt" ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("!html/**/*.txt" ).does_match("/path/to/html/foo/fud.php"));
	OAK_ASSERT( path::glob_t("!html"          ).does_match("/path/to/page"));
	OAK_ASSERT( path::glob_t("!html/*"        ).does_match("/path/to/page/fud.txt"));
	OAK_ASSERT( path::glob_t("!html/**"       ).does_match("/path/to/page/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("!html/**/*"     ).does_match("/path/to/page/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("!html/**/*.txt" ).does_match("/path/to/page/foo/fud.txt"));
}

void test_glob_exclusion ()
{
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/main.cc"));
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/main.mm"));
	OAK_ASSERT( path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/main.h"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/main.txt"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/main.cc"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/main.mm"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/main.h"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/src/main.cc"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/src/main.mm"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/src/main.h"));

	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/src/.main.cc"));
	OAK_ASSERT(!path::glob_t("*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/src/.main.cc"));

	OAK_ASSERT( path::glob_t("{,.}*.{cc,mm,h}~vendor/**" ).does_match("/path/to/src/.main.cc"));
	OAK_ASSERT(!path::glob_t("{,.}*.{cc,mm,h}~vendor/**" ).does_match("/path/to/vendor/src/.main.cc"));

	OAK_ASSERT( path::glob_t("src/**~*.o~*.s" ).does_match("/path/to/src/main.cc"));
	OAK_ASSERT( path::glob_t("src/**~*.o~*.s" ).does_match("/path/to/src/main.h"));
	OAK_ASSERT(!path::glob_t("src/**~*.o~*.s" ).does_match("/path/to/src/main.o"));
	OAK_ASSERT(!path::glob_t("src/**~*.o~*.s" ).does_match("/path/to/src/main.s"));
	OAK_ASSERT(!path::glob_t("src/**~*.o~*.s" ).does_match("/path/to/main.cc"));

	OAK_ASSERT(!path::glob_t("~html"          ).does_match("/path/to/html"));
	OAK_ASSERT( path::glob_t("~html"          ).does_match("/path/to/html/foo"));
	OAK_ASSERT(!path::glob_t("~html/*"        ).does_match("/path/to/html/fud.txt"));
	OAK_ASSERT( path::glob_t("~html/*"        ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("~html/**"       ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("~html/**/*"     ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT(!path::glob_t("~html/**/*.txt" ).does_match("/path/to/html/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("~html/**/*.txt" ).does_match("/path/to/html/foo/fud.php"));
	OAK_ASSERT( path::glob_t("~html"          ).does_match("/path/to/page"));
	OAK_ASSERT( path::glob_t("~html/*"        ).does_match("/path/to/page/fud.txt"));
	OAK_ASSERT( path::glob_t("~html/**"       ).does_match("/path/to/page/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("~html/**/*"     ).does_match("/path/to/page/foo/fud.txt"));
	OAK_ASSERT( path::glob_t("~html/**/*.txt" ).does_match("/path/to/page/foo/fud.txt"));
}

void test_glob_brace_expansion_match ()
{
	OAK_ASSERT(path::glob_t("foo{bar}.txt").does_match("foo{bar}.txt"));
	OAK_ASSERT(path::glob_t("foo{,bar}.txt").does_match("foo.txt"));
	OAK_ASSERT(path::glob_t("foo{,bar}.txt").does_match("foobar.txt"));
	OAK_ASSERT(path::glob_t("foo{\\,bar}.txt").does_match("foo{,bar}.txt"));
	OAK_ASSERT(path::glob_t("{foo,{bar},baz}.txt").does_match("foo.txt"));
	OAK_ASSERT(path::glob_t("{foo,{bar},baz}.txt").does_match("{bar}.txt"));
	OAK_ASSERT(path::glob_t("{foo,{bar},baz}.txt").does_match("baz.txt"));
}

void test_escape_glob ()
{
	OAK_ASSERT_EQ(path::glob_t::escape("Movie [2015].mp4~backup"), "Movie \\[2015].mp4\\~backup");
	OAK_ASSERT_EQ(path::glob_t::escape("Movie {***}.mp4"), "Movie \\{\\*\\*\\*}.mp4");
}
