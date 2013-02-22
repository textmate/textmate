#include <io/path.h>
#include <test/jail.h>

void test_identifier ()
{
	test::jail_t jail;

	jail.touch("test.txt");
	jail.ln("link.txt", "test.txt");

	OAK_ASSERT_EQ(path::exists(jail.path("test.txt")), true);
	OAK_ASSERT_EQ(path::exists(jail.path("link.txt")), true);

	path::identifier_t id_1(jail.path("test.txt"));
	path::identifier_t id_2(jail.path("link.txt"));
	path::identifier_t id_3(jail.path("./test.txt"));
	path::identifier_t id_4(jail.path("./link.txt"));
	path::identifier_t id_5(jail.path("./link.txt"), true);

	OAK_ASSERT_EQ(id_1, id_3);
	OAK_ASSERT_EQ(id_1, id_5);
	OAK_ASSERT_NE(id_1, id_2);
	OAK_ASSERT_EQ(id_2, id_4);
	OAK_ASSERT_NE(id_3, id_4);
}
