#include <io/src/io.h>

void test_normalize ()
{
	OAK_ASSERT_EQ(path::normalize("//foo//"),                            "/foo");
	OAK_ASSERT_EQ(path::normalize("/foo/././."),                         "/foo");
	OAK_ASSERT_EQ(path::normalize("/foo/bar/fud/../../baz/.."),          "/foo");
	OAK_ASSERT_EQ(path::normalize("//foo/bar//.//fud/../../baz/..//."),  "/foo");

	OAK_ASSERT_EQ(path::normalize("foo/.."),                             "");
	OAK_ASSERT_EQ(path::normalize("foo/../.."),                          "..");
	OAK_ASSERT_EQ(path::normalize("foo/../../bar"),                      "../bar");
	OAK_ASSERT_EQ(path::normalize("./bar"),                              "bar");
	OAK_ASSERT_EQ(path::normalize("../bar"),                             "../bar");

	OAK_ASSERT_EQ(path::normalize(NULL_STR),                             NULL_STR);
}

void test_extensions ()
{
	OAK_ASSERT_EQ(path::strip_extension("foo"),                   "foo");
	OAK_ASSERT_EQ(path::strip_extension("foo.css.php"),           "foo.css");
	OAK_ASSERT_EQ(path::strip_extension("/foo.bar/foo.css.php"),  "/foo.bar/foo.css");

	OAK_ASSERT_EQ(path::strip_extensions("foo"),                   "foo");
	OAK_ASSERT_EQ(path::strip_extensions("foo.css.php"),           "foo");
	OAK_ASSERT_EQ(path::strip_extensions("/foo.bar/foo.css.php"),  "/foo.bar/foo");

	OAK_ASSERT_EQ(path::extension("foo"),                   "");
	OAK_ASSERT_EQ(path::extension("foo.css.php"),           ".php");
	OAK_ASSERT_EQ(path::extension("/foo.bar/foo.css.php"),  ".php");

	OAK_ASSERT_EQ(path::extensions("foo"),                   "");
	OAK_ASSERT_EQ(path::extensions("foo.css.php"),           ".css.php");
	OAK_ASSERT_EQ(path::extensions("/foo.bar/foo.css.php"),  ".css.php");
}

void test_dot_files ()
{
	OAK_ASSERT_EQ(path::extensions(".profile"),                ".profile");
	OAK_ASSERT_EQ(path::strip_extensions(".profile"),          "");

	OAK_ASSERT_EQ(path::extensions("/home/me/.profile"),       ".profile");
	OAK_ASSERT_EQ(path::strip_extensions("/home/me/.profile"), "/home/me/");
}

void test_dot_in_basename ()
{
	OAK_ASSERT_EQ(path::extensions("(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2.limechat"),       ".limechat");
	OAK_ASSERT_EQ(path::strip_extensions("(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2.limechat"), "(allan) ##textmate (103,+nt) Issues with 1.5.10? See h… 2");

	OAK_ASSERT_EQ(path::extensions("index.php4"),                    ".php4");
	OAK_ASSERT_EQ(path::strip_extensions("index.php4"),              "index");

	OAK_ASSERT_EQ(path::extensions("TextMate.tar.bz2"),              ".tar.bz2");
	OAK_ASSERT_EQ(path::strip_extensions("TextMate.tar.bz2"),        "TextMate");

	OAK_ASSERT_EQ(path::extensions("TextMate_1.5.10.dmg"),           ".dmg");
	OAK_ASSERT_EQ(path::strip_extensions("TextMate_1.5.10.dmg"),     "TextMate_1.5.10");

	OAK_ASSERT_EQ(path::extensions("TextMate_1.5.10.tar.bz2"),       ".tar.bz2");
	OAK_ASSERT_EQ(path::strip_extensions("TextMate_1.5.10.tar.bz2"), "TextMate_1.5.10");
}

void test_rank ()
{
	OAK_ASSERT_EQ(path::rank("foo.css.php", "hp"),       0);
	OAK_ASSERT_GT(path::rank("foo.css.php", "php"),      0);
	OAK_ASSERT_EQ(path::rank("foo.css.php", "gphp"),     0);
	OAK_ASSERT_GT(path::rank("foo.css.php", "css.php"),  0);
	OAK_ASSERT_GT(path::rank("foo.css.php", "css.php"),  path::rank("foo.css.php", "php"));

	OAK_ASSERT_GT(path::rank("foo_spec.rb", "rb"),       0);
	OAK_ASSERT_GT(path::rank("foo_spec.rb", "spec.rb"),  0);
	OAK_ASSERT_GT(path::rank("foo_spec.rb", "spec.rb"),  path::rank("foo_spec.rb", "rb"));

	OAK_ASSERT_GT(path::rank("CMakeLists.txt", "txt"),             0);
	OAK_ASSERT_GT(path::rank("CMakeLists.txt", "CMakeLists.txt"),  0);
	OAK_ASSERT_GT(path::rank("CMakeLists.txt", "CMakeLists.txt"),  path::rank("CMakeLists.txt", "txt"));

	OAK_ASSERT_GT(path::rank("/CMakeLists.txt", "txt"),            0);
	OAK_ASSERT_GT(path::rank("/CMakeLists.txt", "CMakeLists.txt"), 0);
	OAK_ASSERT_GT(path::rank("/CMakeLists.txt", "CMakeLists.txt"), path::rank("/CMakeLists.txt", "txt"));
}

void test_relative_to ()
{
	OAK_ASSERT_EQ(path::relative_to("/foo/bar/fud", "/foo"),      "bar/fud");
	OAK_ASSERT_EQ(path::relative_to("/foo/bar/fud", "/foo/bar"),  "fud");
	OAK_ASSERT_EQ(path::relative_to("/foo/fud",     "/foo/bar"),  "../fud");
	OAK_ASSERT_EQ(path::relative_to("/foo/baz/fud", "/foo/bar"),  "../baz/fud");
}

void test_path_components ()
{
	OAK_ASSERT_EQ(path::name("/foo/bar/fud"),     "fud");
	OAK_ASSERT_EQ(path::parent("/foo/bar/fud"),   "/foo/bar");
	OAK_ASSERT_EQ(path::join("/foo/bar", "fud"),  "/foo/bar/fud");

	OAK_ASSERT_EQ(path::name("foo/bar/fud"),      "fud");
	OAK_ASSERT_EQ(path::parent("foo/bar/fud"),    "foo/bar");
	OAK_ASSERT_EQ(path::join("foo/bar", "fud"),   "foo/bar/fud");
}

void test_is_absolute ()
{
	OAK_ASSERT_EQ(path::is_absolute("../"),         false);
	OAK_ASSERT_EQ(path::is_absolute("../foo"),      false);
	OAK_ASSERT_EQ(path::is_absolute("./"),          false);
	OAK_ASSERT_EQ(path::is_absolute("/."),          true);
	OAK_ASSERT_EQ(path::is_absolute("/.."),         false);
	OAK_ASSERT_EQ(path::is_absolute("/../"),        false);
	OAK_ASSERT_EQ(path::is_absolute("/../tmp"),     false); // this path is actually valid, so might revise path::normalize()
	OAK_ASSERT_EQ(path::is_absolute("/./.."),       false);
	OAK_ASSERT_EQ(path::is_absolute("/./../tmp"),   false); // this path is actually valid, so might revise path::normalize()
	OAK_ASSERT_EQ(path::is_absolute("/./foo"),      true);
	OAK_ASSERT_EQ(path::is_absolute("//."),         true);
	OAK_ASSERT_EQ(path::is_absolute("//../../foo"), false);
	OAK_ASSERT_EQ(path::is_absolute("//./foo"),     true);
	OAK_ASSERT_EQ(path::is_absolute("/foo/.."),     true);
	OAK_ASSERT_EQ(path::is_absolute("/foo/../.."),  false);
	OAK_ASSERT_EQ(path::is_absolute("foo"),         false);
}

void test_is_parent ()
{
	OAK_ASSERT_EQ(path::is_child("/foo/bar",     "/foo/bar"),    true);
	OAK_ASSERT_EQ(path::is_child("/foo/bar/fud", "/foo/bar"),    true);
	OAK_ASSERT_EQ(path::is_child("/foo/barry",   "/foo/bar"),   false);
	OAK_ASSERT_EQ(path::is_child("/foo/bar",     "/foo/barry"), false);
}

void test_with_tilde ()
{
	using namespace path;

	OAK_ASSERT_EQ(path::with_tilde(home()),                    "~");
	OAK_ASSERT_EQ(path::with_tilde(home() + "/"),              "~/");
	OAK_ASSERT_EQ(path::with_tilde(home() + "//"),             "~/");
	OAK_ASSERT_EQ(path::with_tilde(home() + "/./"),            "~/");
	OAK_ASSERT_EQ(path::with_tilde(home() + "./"),             home() + "./");
	OAK_ASSERT_EQ(path::with_tilde(join(home(), "foo")),       "~/foo");
	OAK_ASSERT_EQ(path::with_tilde(join(home(), "foo") + "/"), "~/foo/");
	OAK_ASSERT_EQ(path::with_tilde("foo" + home()),            "foo" + home());
	OAK_ASSERT_EQ(path::with_tilde("/dummy"),                  "/dummy");
	OAK_ASSERT_EQ(path::with_tilde(NULL_STR),                  NULL_STR);
}

void test_resolve ()
{
	OAK_ASSERT_EQ(path::resolve(NULL_STR), NULL_STR);
}
