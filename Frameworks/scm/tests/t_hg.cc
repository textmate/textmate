#include <scm/scm.h>
#include <test/jail.h>
#include <io/path.h>

class hg_tests : public CxxTest::TestSuite
{
public:
	void test_basic_status ()
	{
		test::jail_t jail;

		TSM_ASSERT_EQUALS("\n\n  Unable to test mercurial driver (hg executable not found).\n\n  To skip this test:\n    ninja scm/coerce\n\n  To install required executable (via MacPorts):\n    sudo port install mercurial\n", system("which -s hg"), 0);

		std::string const wcPath = jail.path();
		std::string const script = text::format("{ cd '%s' && hg init && touch {clean,ignored,modified,added,missing,untracked}.txt && echo ignored.txt > .hgignore && hg add {.hgignore,{clean,modified,missing}.txt} && hg commit -u 'Test User' -m 'Initial commit' && hg add added.txt && rm missing.txt && echo foo > modified.txt; } >/dev/null", wcPath.c_str());
		if(system(script.c_str()) != 0)
		{
			TS_FAIL(("error in setup: " + script).c_str());
			return;
		}

		if(scm::info_ptr info = scm::info(jail.path("clean.txt")))
		{
			TS_ASSERT_EQUALS(info->status(jail.path("clean.txt")),     scm::status::none);
			TS_ASSERT_EQUALS(info->status(jail.path("ignored.txt")),   scm::status::ignored);
			TS_ASSERT_EQUALS(info->status(jail.path("modified.txt")),  scm::status::modified);
			TS_ASSERT_EQUALS(info->status(jail.path("added.txt")),     scm::status::added);
			TS_ASSERT_EQUALS(info->status(jail.path("missing.txt")),   scm::status::deleted);
			TS_ASSERT_EQUALS(info->status(jail.path("untracked.txt")), scm::status::unversioned);
		}
		else
		{
			TS_FAIL(("error getting wc: " + wcPath).c_str());
		}
	}
};
