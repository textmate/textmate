#include <test/jail.h>
#include <document/document.h>

class XAttrTests : public CxxTest::TestSuite
{
public:
	void test_bookmarks ()
	{
		test::jail_t jail;
		jail.set_content("test.txt", "foo\nbar\nfud\n");
		path::set_attr(jail.path("test.txt"), "com.macromates.bookmarks", "( '1:1', '1:8', '4:2' )");

		document::document_ptr doc = document::create(jail.path("test.txt"));
		doc->open();

		std::map<size_t, std::string> marks = doc->buffer().get_marks(0, doc->buffer().size(), "bookmark");
		TS_ASSERT_EQUALS(marks.size(), 3);
		TS_ASSERT_EQUALS(marks[0], "bookmark");
		TS_ASSERT_EQUALS(marks[3], "bookmark");
		TS_ASSERT_EQUALS(marks[12], "bookmark");
	}

	void test_selection ()
	{
		test::jail_t jail;
		jail.set_content("test.txt", "foo\nbar\nfud\n");
		path::set_attr(jail.path("test.txt"), "com.macromates.selectionRange", "2:2&3:1");

		document::document_ptr doc = document::create(jail.path("test.txt"));
		doc->open();

		TS_ASSERT_EQUALS(doc->selection(), "2:2&3:1");

		// ng::ranges_t const sel = convert(doc->buffer(), doc->selection());
		// for(auto const& range : sel)
		// 	fprintf(stderr, "%zu-%zu\n", range.first.index, range.last.index);
	}
};
