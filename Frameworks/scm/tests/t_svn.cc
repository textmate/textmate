#include <scm/scm.h>
#include <settings/settings.h>
#include <test/jail.h>
#include <io/path.h>

void test_basic_status ()
{
	test::jail_t jail;

	auto tmEnv = variables_for_path();

	auto pathVar = tmEnv.find("PATH");
	if(pathVar != tmEnv.end())
		setenv("PATH", pathVar->second.c_str(), 1);

	auto svnExe = tmEnv.find("TM_SVN");
	if(svnExe != tmEnv.end())
		setenv("TM_SVN", svnExe->second.c_str(), 1);

	OAK_MASSERT_EQ("\n\n  Unable to test subversion driver (svn executable not found).\n\n  To skip this test:\n    ninja scm/coerce\n\n  To install required executable (via MacPorts):\n    sudo port install subversion\n", system("which -s svn"), 0);

	std::string const repoName = "tm-test-repo";
	std::string const wcName = "tm-test-wc";
	std::string const jailPath = jail.path();
	std::string const script = text::format("{ cd '%s' && \"${TM_SVN:-svn}admin\" create '%s' && \"${TM_SVN:-svn}\" co 'file://%s/%s' %s && cd '%s' && touch {clean,ignored,modified,added,missing,untracked}.txt && \"${TM_SVN:-svn}\" propset svn:ignore 'ignored.txt' . && \"${TM_SVN:-svn}\" add {clean,modified,missing}.txt && \"${TM_SVN:-svn}\" commit -m 'Initial commit' && \"${TM_SVN:-svn}\" add added.txt && \"${TM_SVN:-svn}\" rm missing.txt && echo foo > modified.txt; } >/dev/null", jailPath.c_str(), repoName.c_str(), jailPath.c_str(), repoName.c_str(), wcName.c_str(), wcName.c_str());

	if(system(script.c_str()) != 0)
		OAK_FAIL("error in setup: " + script);

	if(auto info = scm::ng::info(jail.path(wcName)))
	{
		wait_for_status(info);

		std::string expectedBranch = text::format("file://%s/%s", jailPath.c_str(), repoName.c_str());

		auto vars = info->variables();
		OAK_ASSERT_EQ(vars["TM_SCM_NAME"],   "svn");
		OAK_ASSERT_EQ(vars["TM_SCM_BRANCH"], expectedBranch);

		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "clean.txt"))),     scm::status::none);
		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "ignored.txt"))),   scm::status::ignored);
		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "modified.txt"))),  scm::status::modified);
		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "added.txt"))),     scm::status::added);
		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "missing.txt"))),   scm::status::deleted);
		OAK_ASSERT_EQ(info->status(jail.path(path::join(wcName, "untracked.txt"))), scm::status::unversioned);
	}
	else
	{
		OAK_FAIL("error getting wc: " + jailPath);
	}
}
