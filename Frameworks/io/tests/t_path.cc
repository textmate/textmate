#include <io/io.h>

class path_tests : public CxxTest::TestSuite
{
public:
	void test_normalize ()
	{
		TS_ASSERT_EQUALS(path::normalize("//foo//"),                            "/foo");
		TS_ASSERT_EQUALS(path::normalize("/foo/././."),                         "/foo");
		TS_ASSERT_EQUALS(path::normalize("/foo/bar/fud/../../baz/.."),          "/foo");
		TS_ASSERT_EQUALS(path::normalize("//foo/bar//.//fud/../../baz/..//."),  "/foo");

		TS_ASSERT_EQUALS(path::normalize("foo/.."),                             "");
		TS_ASSERT_EQUALS(path::normalize("foo/../.."),                          "..");
		TS_ASSERT_EQUALS(path::normalize("foo/../../bar"),                      "../bar");
		TS_ASSERT_EQUALS(path::normalize("./bar"),                              "bar");
		TS_ASSERT_EQUALS(path::normalize("../bar"),                             "../bar");

		TS_ASSERT_EQUALS(path::normalize(NULL_STR),                             NULL_STR);
	}

	void test_extensions ()
	{
		TS_ASSERT_EQUALS(path::strip_extension("foo"),                   "foo");
		TS_ASSERT_EQUALS(path::strip_extension("foo.css.php"),           "foo.css");
		TS_ASSERT_EQUALS(path::strip_extension("/foo.bar/foo.css.php"),  "/foo.bar/foo.css");

		TS_ASSERT_EQUALS(path::strip_extensions("foo"),                   "foo");
		TS_ASSERT_EQUALS(path::strip_extensions("foo.css.php"),           "foo");
		TS_ASSERT_EQUALS(path::strip_extensions("/foo.bar/foo.css.php"),  "/foo.bar/foo");

		TS_ASSERT_EQUALS(path::extension("foo"),                   "");
		TS_ASSERT_EQUALS(path::extension("foo.css.php"),           ".php");
		TS_ASSERT_EQUALS(path::extension("/foo.bar/foo.css.php"),  ".php");

		TS_ASSERT_EQUALS(path::extensions("foo"),                   "");
		TS_ASSERT_EQUALS(path::extensions("foo.css.php"),           ".css.php");
		TS_ASSERT_EQUALS(path::extensions("/foo.bar/foo.css.php"),  ".css.php");
	}

	void test_dot_files ()
	{
		TS_ASSERT_EQUALS(path::extensions(".profile"),                ".profile");
		TS_ASSERT_EQUALS(path::strip_extensions(".profile"),          "");

		TS_ASSERT_EQUALS(path::extensions("/home/me/.profile"),       ".profile");
		TS_ASSERT_EQUALS(path::strip_extensions("/home/me/.profile"), "/home/me/");
	}

	void test_dot_in_basename ()
	{
		TS_ASSERT_EQUALS(path::extensions("(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2.limechat"),       ".limechat");
		TS_ASSERT_EQUALS(path::strip_extensions("(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2.limechat"), "(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2");

		TS_ASSERT_EQUALS(path::extensions("index.php4"),                    ".php4");
		TS_ASSERT_EQUALS(path::strip_extensions("index.php4"),              "index");

		TS_ASSERT_EQUALS(path::extensions("TextMate.tar.bz2"),              ".tar.bz2");
		TS_ASSERT_EQUALS(path::strip_extensions("TextMate.tar.bz2"),        "TextMate");

		TS_ASSERT_EQUALS(path::extensions("TextMate_1.5.10.dmg"),           ".dmg");
		TS_ASSERT_EQUALS(path::strip_extensions("TextMate_1.5.10.dmg"),     "TextMate_1.5.10");

		TS_ASSERT_EQUALS(path::extensions("TextMate_1.5.10.tar.bz2"),       ".tar.bz2");
		TS_ASSERT_EQUALS(path::strip_extensions("TextMate_1.5.10.tar.bz2"), "TextMate_1.5.10");
	}

	void test_rank ()
	{
		TS_ASSERT_EQUALS(path::rank("foo.css.php", "hp"),          0);
		TS_ASSERT_DIFFERS(path::rank("foo.css.php", "php"),        0);
		TS_ASSERT_EQUALS(path::rank("foo.css.php", "gphp"),        0);
		TS_ASSERT_DIFFERS(path::rank("foo.css.php", "css.php"),    0);
		TS_ASSERT_LESS_THAN(path::rank("foo.css.php", "css.php"),  path::rank("foo.css.php", "php"));
	}

	void test_relative_to ()
	{
		TS_ASSERT_EQUALS(path::relative_to("/foo/bar/fud", "/foo"),      "bar/fud");
		TS_ASSERT_EQUALS(path::relative_to("/foo/bar/fud", "/foo/bar"),  "fud");
		TS_ASSERT_EQUALS(path::relative_to("/foo/fud",     "/foo/bar"),  "../fud");
		TS_ASSERT_EQUALS(path::relative_to("/foo/baz/fud", "/foo/bar"),  "../baz/fud");
	}

	void test_path_components ()
	{
		TS_ASSERT_EQUALS(path::name("/foo/bar/fud"),     "fud");
		TS_ASSERT_EQUALS(path::parent("/foo/bar/fud"),   "/foo/bar");
		TS_ASSERT_EQUALS(path::join("/foo/bar", "fud"),  "/foo/bar/fud");

		TS_ASSERT_EQUALS(path::name("foo/bar/fud"),      "fud");
		TS_ASSERT_EQUALS(path::parent("foo/bar/fud"),    "foo/bar");
		TS_ASSERT_EQUALS(path::join("foo/bar", "fud"),   "foo/bar/fud");
	}

	void test_is_absolute ()
	{
		TS_ASSERT_EQUALS(path::is_absolute("../"),         false);
		TS_ASSERT_EQUALS(path::is_absolute("../foo"),      false);
		TS_ASSERT_EQUALS(path::is_absolute("./"),          false);
		TS_ASSERT_EQUALS(path::is_absolute("/."),          true);
		TS_ASSERT_EQUALS(path::is_absolute("/.."),         false);
		TS_ASSERT_EQUALS(path::is_absolute("/../"),        false);
		TS_ASSERT_EQUALS(path::is_absolute("/../tmp"),     false); // this path is actually valid, so might revise path::normalize()
		TS_ASSERT_EQUALS(path::is_absolute("/./.."),       false);
		TS_ASSERT_EQUALS(path::is_absolute("/./../tmp"),   false); // this path is actually valid, so might revise path::normalize()
		TS_ASSERT_EQUALS(path::is_absolute("/./foo"),      true);
		TS_ASSERT_EQUALS(path::is_absolute("//."),         true);
		TS_ASSERT_EQUALS(path::is_absolute("//../../foo"), false);
		TS_ASSERT_EQUALS(path::is_absolute("//./foo"),     true);
		TS_ASSERT_EQUALS(path::is_absolute("/foo/.."),     true);
		TS_ASSERT_EQUALS(path::is_absolute("/foo/../.."),  false);
		TS_ASSERT_EQUALS(path::is_absolute("foo"),         false);
	}

	void test_is_parent ()
	{
		TS_ASSERT_EQUALS(path::is_child("/foo/bar",     "/foo/bar"),    true);
		TS_ASSERT_EQUALS(path::is_child("/foo/bar/fud", "/foo/bar"),    true);
		TS_ASSERT_EQUALS(path::is_child("/foo/barry",   "/foo/bar"),   false);
		TS_ASSERT_EQUALS(path::is_child("/foo/bar",     "/foo/barry"), false);
	}

	void test_with_tilde ()
	{
		using namespace path;

		TS_ASSERT_EQUALS(path::with_tilde(home()),                    "~");
		TS_ASSERT_EQUALS(path::with_tilde(home() + "/"),              "~/");
		TS_ASSERT_EQUALS(path::with_tilde(home() + "//"),             "~/");
		TS_ASSERT_EQUALS(path::with_tilde(home() + "/./"),            "~/");
		TS_ASSERT_EQUALS(path::with_tilde(home() + "./"),             home() + "./");
		TS_ASSERT_EQUALS(path::with_tilde(join(home(), "foo")),       "~/foo");
		TS_ASSERT_EQUALS(path::with_tilde(join(home(), "foo") + "/"), "~/foo/");
		TS_ASSERT_EQUALS(path::with_tilde("foo" + home()),            "foo" + home());
		TS_ASSERT_EQUALS(path::with_tilde("/dummy"),                  "/dummy");
		TS_ASSERT_EQUALS(path::with_tilde(NULL_STR),                  NULL_STR);
	}

	void test_resolve ()
	{
		TS_ASSERT_EQUALS(path::resolve(NULL_STR), NULL_STR);
	}
};
