#include <test/jail.h>
#include <document/document.h>

class SymlinksTests : public CxxTest::TestSuite
{
public:
	void test_symlinks ()
	{
		test::jail_t jail;

		jail.touch("test.txt");
		jail.ln("link.txt", "test.txt");

		TS_ASSERT_EQUALS(path::exists(jail.path("test.txt")), true);
		TS_ASSERT_EQUALS(path::exists(jail.path("link.txt")), true);

		document::document_ptr srcDoc  = document::create(jail.path("test.txt"));
		document::document_ptr linkDoc = document::create(jail.path("link.txt"));
		TS_ASSERT_EQUALS(srcDoc->identifier(), linkDoc->identifier());

		document::document_ptr aliasDoc = document::create(jail.path("./test.txt"));
		TS_ASSERT_EQUALS(srcDoc->identifier(), aliasDoc->identifier());
	}
};
