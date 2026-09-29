#include <scm/src/scm.h>
#include <test/jail.h>

void test_disabling_scm ()
{
	test::jail_t jail;
	jail.set_content(".tm_properties", "scmStatus = false\n");
	OAK_ASSERT_EQ(scm::info(jail.path()) ? true : false, false);
}
