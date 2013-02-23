#include "../src/snapshot.h"
#include <test/jail.h>

void test_fs_tree ()
{
	std::string path = path::join(__FILE__, "../../..");
	OAK_ASSERT_EQ(fs::snapshot_t(path), fs::snapshot_t(path));

	test::jail_t jail;
	fs::snapshot_t jailSnapshot(jail.path());
	OAK_ASSERT_NE(jailSnapshot, fs::snapshot_t(path));
	OAK_ASSERT_EQ(jailSnapshot, fs::snapshot_t(jail.path()));
	jail.touch("foo");
	OAK_ASSERT_NE(jailSnapshot, fs::snapshot_t(jail.path()));
}
