#include <document/document.h>

class GrammarPickingTests : public CxxTest::TestSuite
{
public:
	void test_grammar_picking ()
	{
		document::document_ptr doc = document::create();
		doc->set_file_type("source.c");
		doc->open();
		TS_ASSERT_EQUALS(to_s(doc->buffer().scope(0).left), "source.c");
		doc->close();
	}

	void test_modeline_file ()
	{
		test::jail_t jail;
		jail.set_content("source-file", "// -*- Mode: C -*-\n\nint main () { return 0; }\n");

		document::document_ptr doc = document::create(jail.path("source-file"));
		doc->open();
		TS_ASSERT_EQUALS(to_s(doc->buffer().scope(0).left), "source.c");
		TS_ASSERT_EQUALS(doc->file_type(), "source.c");
		doc->close();
	}

	void test_modeline_buffer ()
	{
		document::document_ptr doc = document::from_content("// -*- Mode: C -*-\n\nint main () { return 0; }\n");
		doc->open();
		TS_ASSERT_EQUALS(to_s(doc->buffer().scope(0).left), "source.c");
		TS_ASSERT_EQUALS(doc->file_type(), "source.c");
		doc->close();
	}
};
