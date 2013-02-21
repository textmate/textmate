#include <scm/scm.h>
#include <test/jail.h>
#include <io/path.h>

void test_basic_status ()
{
	test::jail_t jail;

	OAK_MASSERT_EQ("\n\n  Unable to test mercurial driver (hg executable not found).\n\n  To skip this test:\n    ninja scm/coerce\n\n  To install required executable (via MacPorts):\n    sudo port install mercurial\n", system("which -s hg"), 0);

	std::string const wcPath = jail.path();
	std::string const script = text::format("{ cd '%s' && hg init && touch {clean,ignored,modified,added,missing,untracked}.txt && echo ignored.txt > .hgignore && hg add {.hgignore,{clean,modified,missing}.txt} && hg commit -u 'Test User' -m 'Initial commit' && hg add added.txt && rm missing.txt && echo foo > modified.txt; } >/dev/null", wcPath.c_str());
	if(system(script.c_str()) != 0)
		OAK_FAIL("error in setup: " + script);

	if(auto info = scm::ng::info(jail.path()))
	{
		wait_for_status(info);

		auto vars = info->variables();
		OAK_ASSERT_EQ(vars["TM_SCM_NAME"],   "hg");
		OAK_ASSERT_EQ(vars["TM_SCM_BRANCH"], "default");

		OAK_ASSERT_EQ(info->status(jail.path("clean.txt")),     scm::status::none);
		OAK_ASSERT_EQ(info->status(jail.path("ignored.txt")),   scm::status::ignored);
		OAK_ASSERT_EQ(info->status(jail.path("modified.txt")),  scm::status::modified);
		OAK_ASSERT_EQ(info->status(jail.path("added.txt")),     scm::status::added);
		OAK_ASSERT_EQ(info->status(jail.path("missing.txt")),   scm::status::deleted);
		OAK_ASSERT_EQ(info->status(jail.path("untracked.txt")), scm::status::unversioned);
	}
	else
	{
		OAK_FAIL("error getting wc: " + wcPath);
	}
}
