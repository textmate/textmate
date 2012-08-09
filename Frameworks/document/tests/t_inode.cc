#include <document/document.h>
#include <test/jail.h>

class InodeTests : public CxxTest::TestSuite
{
public:
	void test_inode ()
	{
		test::jail_t jail;
		jail.touch("file_1.txt");

		document::document_ptr doc_1 = document::create(jail.path("file_1.txt"));
		path::move(jail.path("file_1.txt"), jail.path("file_2.txt"));
		document::document_ptr doc_2 = document::create(jail.path("file_2.txt"));
		TS_ASSERT(*doc_1 == *doc_2);
		TS_ASSERT_EQUALS(path::name(doc_2->path()), "file_2.txt");
	}
};
