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

		jail.ln("file_3.txt", "file_2.txt");
		document::document_ptr doc_3 = document::create(jail.path("file_3.txt"));

		TS_ASSERT(*doc_1 == *doc_2);
		TS_ASSERT(*doc_2 == *doc_3);

		TS_ASSERT_EQUALS(path::name(doc_1->path()), "file_1.txt");
		TS_ASSERT_EQUALS(path::name(doc_2->path()), "file_1.txt");
		TS_ASSERT_EQUALS(path::name(doc_3->path()), "file_1.txt");

		document::document_ptr doc_4 = document::create(jail.path("future.txt"));
		jail.touch("future.txt");
		document::document_ptr doc_5 = document::create(jail.path("future.txt"));

		TS_ASSERT(*doc_4 == *doc_5);
		TS_ASSERT_EQUALS(path::name(doc_4->path()), "future.txt");
	}
};
