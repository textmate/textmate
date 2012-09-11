#include <regexp/glob.h>

class GlobListTests : public CxxTest::TestSuite
{
public:
	void test_glob_list ()
	{
		path::glob_list_t globs;
		globs.add_exclude_glob("*.{png,pdf}");
		globs.add_exclude_glob("vendor/**");
		globs.add_include_glob(".htaccess");
		globs.add_exclude_glob("cache/**");
		globs.add_include_glob("*");

		TS_ASSERT(globs.include("main.cc")           );
		TS_ASSERT(globs.include("foo/main.cc")       );
		TS_ASSERT(globs.include(".htaccess")         );
		TS_ASSERT(globs.include("foo/.htaccess")     );
		TS_ASSERT(globs.include("cache/.htaccess")   );

		TS_ASSERT(globs.exclude(".profile")          );
		TS_ASSERT(globs.exclude("foo/.profile")      );
		TS_ASSERT(globs.exclude("main.pdf")          );
		TS_ASSERT(globs.exclude("foo/main.pdf")      );
		TS_ASSERT(globs.exclude("vendor/main.cc")    );
		TS_ASSERT(globs.exclude("vendor/.htaccess")  );
		TS_ASSERT(globs.exclude("cache/main.cc")     );
	}
};
