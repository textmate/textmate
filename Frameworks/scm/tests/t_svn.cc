#include <scm/scm.h>
#include <settings/settings.h>
#include <test/jail.h>
#include <io/path.h>

class svn_tests : public CxxTest::TestSuite
{
public:
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

		TSM_ASSERT_EQUALS("\n\n  Unable to test subversion driver (svn executable not found).\n\n  To skip this test:\n    ninja scm/coerce\n\n  To install required executable (via MacPorts):\n    sudo port install subversion\n", system("which -s svn"), 0);

		std::string const repoName = "tm-test-repo";
		std::string const wcName = "tm-test-wc";
		std::string const jailPath = jail.path();
		std::string const script = text::format("{ cd '%s' && \"${TM_SVN:-svn}admin\" create '%s' && \"${TM_SVN:-svn}\" co 'file://%s/%s' %s && cd '%s' && touch {clean,ignored,modified,added,missing,untracked}.txt && \"${TM_SVN:-svn}\" propset svn:ignore 'ignored.txt' . && \"${TM_SVN:-svn}\" add {clean,modified,missing}.txt && \"${TM_SVN:-svn}\" commit -m 'Initial commit' && \"${TM_SVN:-svn}\" add added.txt && \"${TM_SVN:-svn}\" rm missing.txt && echo foo > modified.txt; } >/dev/null", jailPath.c_str(), repoName.c_str(), jailPath.c_str(), repoName.c_str(), wcName.c_str(), wcName.c_str());

		if(system(script.c_str()) != 0)
		{
			TS_FAIL(("error in setup: " + script).c_str());
			return;
		}

		if(scm::info_ptr info = scm::info(jail.path(path::join(wcName, "clean.txt"))))
		{
			std::string expectedBranch = text::format("file://%s/%s", jailPath.c_str(), repoName.c_str());

			TS_ASSERT_EQUALS(expectedBranch, info->branch());

			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "clean.txt"))),     scm::status::none);
			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "ignored.txt"))),   scm::status::ignored);
			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "modified.txt"))),  scm::status::modified);
			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "added.txt"))),     scm::status::added);
			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "missing.txt"))),   scm::status::deleted);
			TS_ASSERT_EQUALS(info->status(jail.path(path::join(wcName, "untracked.txt"))), scm::status::unversioned);
		}
		else
		{
			TS_FAIL(("error getting wc: " + jailPath).c_str());
		}

		jail.set_content(".tm_properties", "scmStatus = false\n");
		scm::info_ptr info = scm::info(jail.path(path::join(wcName, "clean.txt")));
		TS_ASSERT_EQUALS(info ? true : false, false);
	}
};
