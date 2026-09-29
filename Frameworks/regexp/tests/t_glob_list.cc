#include <regexp/src/glob.h>

void test_empty_glob_list ()
{
	path::glob_list_t globs;
	OAK_ASSERT(globs.include("foo"));
	OAK_ASSERT(globs.include("bar"));
	OAK_ASSERT(globs.include(".foo"));
	OAK_ASSERT(globs.include(".bar"));
}

void test_non_empty_glob_list ()
{
	path::glob_list_t globs("*");
	OAK_ASSERT(globs.include("foo"));
	OAK_ASSERT(globs.include("bar"));
	OAK_ASSERT(globs.exclude(".foo"));
	OAK_ASSERT(globs.exclude(".bar"));
}

void test_glob_list ()
{
	path::glob_list_t globs;
	globs.add_exclude_glob("*.{png,pdf}");
	globs.add_exclude_glob("vendor/**");
	globs.add_include_glob(".htaccess");
	globs.add_exclude_glob("cache/**");
	globs.add_include_glob("*");

	OAK_ASSERT(globs.include("main.cc")           );
	OAK_ASSERT(globs.include("foo/main.cc")       );
	OAK_ASSERT(globs.include(".htaccess")         );
	OAK_ASSERT(globs.include("foo/.htaccess")     );
	OAK_ASSERT(globs.include("cache/.htaccess")   );

	OAK_ASSERT(globs.exclude(".profile")          );
	OAK_ASSERT(globs.exclude("foo/.profile")      );
	OAK_ASSERT(globs.exclude("main.pdf")          );
	OAK_ASSERT(globs.exclude("foo/main.pdf")      );
	OAK_ASSERT(globs.exclude("vendor/main.cc")    );
	OAK_ASSERT(globs.exclude("vendor/.htaccess")  );
	OAK_ASSERT(globs.exclude("cache/main.cc")     );
}
