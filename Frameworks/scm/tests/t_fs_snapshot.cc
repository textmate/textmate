#include <scm/snapshot.h>
#include <test/jail.h>

class FsTreeTests : public CxxTest::TestSuite
{
public:
	void test_fs_tree ()
	{
		std::string path = path::join(__FILE__, "../../..");
		TS_ASSERT_EQUALS(fs::snapshot_t(path), fs::snapshot_t(path));

		test::jail_t jail;
		fs::snapshot_t jailSnapshot(jail.path());
		TS_ASSERT_DIFFERS(jailSnapshot, fs::snapshot_t(path));
		TS_ASSERT_EQUALS(jailSnapshot, fs::snapshot_t(jail.path()));
		jail.touch("foo");
		TS_ASSERT_DIFFERS(jailSnapshot, fs::snapshot_t(jail.path()));
	}
};
