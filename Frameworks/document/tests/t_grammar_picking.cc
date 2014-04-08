#include <document/document.h>

void test_grammar_picking ()
{
	document::document_ptr doc = document::create();
	doc->set_file_type("source.c");
	doc->sync_open();
	OAK_ASSERT_EQ(to_s(doc->buffer().scope(0).left), "source.c");
	doc->close();
}

void test_modeline_file ()
{
	test::jail_t jail;
	jail.set_content("source-file", "// -*- Mode: C -*-\n\nint main () { return 0; }\n");

	document::document_ptr doc = document::create(jail.path("source-file"));
	doc->sync_open();
	OAK_ASSERT_EQ(to_s(doc->buffer().scope(0).left), "source.c");
	OAK_ASSERT_EQ(doc->file_type(), "source.c");
	doc->close();
}

void test_modeline_buffer ()
{
	document::document_ptr doc = document::from_content("// -*- Mode: C -*-\n\nint main () { return 0; }\n");
	doc->sync_open();
	OAK_ASSERT_EQ(to_s(doc->buffer().scope(0).left), "source.c");
	OAK_ASSERT_EQ(doc->file_type(), "source.c");
	doc->close();
}
