#include <io/path.h>
#include <test/jail.h>

class IdentifierTests : public CxxTest::TestSuite
{
public:
	void test_identifier ()
	{
		test::jail_t jail;

		jail.touch("test.txt");
		jail.ln("link.txt", "test.txt");

		TS_ASSERT_EQUALS(path::exists(jail.path("test.txt")), true);
		TS_ASSERT_EQUALS(path::exists(jail.path("link.txt")), true);

		path::identifier_t id_1(jail.path("test.txt"));
		path::identifier_t id_2(jail.path("link.txt"));
		path::identifier_t id_3(jail.path("./test.txt"));
		path::identifier_t id_4(jail.path("./link.txt"));
		path::identifier_t id_5(jail.path("./link.txt"), true);

		TS_ASSERT_EQUALS(id_1, id_3);
		TS_ASSERT_EQUALS(id_1, id_5);
		TS_ASSERT_DIFFERS(id_1, id_2);
		TS_ASSERT_EQUALS(id_2, id_4);
		TS_ASSERT_DIFFERS(id_3, id_4);
	}
};
