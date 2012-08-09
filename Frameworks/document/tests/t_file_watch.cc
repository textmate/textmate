#include <document/document.h>
#include <test/jail.h>

struct helper_t : document::document_t
{
	using document::document_t::watch_callback;
};

void notify (document::document_ptr doc, int flags, std::string const& path = NULL_STR)
{
	((*doc).*(&helper_t::watch_callback))(flags, path, false);
}

std::string content (document::document_ptr doc)
{
	return doc->buffer().substr(0, doc->buffer().size());
}

class FileWatchTests : public CxxTest::TestSuite
{
public:
	void test_file_watch ()
	{
		test::jail_t jail;
		jail.set_content("file.txt", "Hello\n");

		document::document_ptr doc = document::create(jail.path("file.txt"));
		doc->open();

		jail.set_content("file.txt", "Hello\nworld\n");
		notify(doc, NOTE_WRITE);
		TS_ASSERT_EQUALS(content(doc), "Hello\nworld\n");

		doc->buffer().insert(5, ", ");
		doc->set_revision(doc->buffer().bump_revision());
		TS_ASSERT_EQUALS(doc->is_modified(), true);
		jail.set_content("file.txt", "Hello, \nworld\n");
		notify(doc, NOTE_WRITE);
		TS_ASSERT_EQUALS(content(doc), "Hello, \nworld\n");
		TS_ASSERT_EQUALS(doc->is_modified(), false);

		jail.set_content("file.txt", "Hello\n");
		notify(doc, NOTE_WRITE);
		TS_ASSERT_EQUALS(content(doc), "Hello\n");
		TS_ASSERT_EQUALS(doc->is_modified(), false);

		doc->close();
	}

	void test_merge ()
	{
		std::string const original = "Hello\n--\n";
		std::string const buffer   = "Hello, world\n--\n";
		std::string const disk     = "Hello\n--\nworld\n";
		std::string const merged   = "Hello, world\n--\nworld\n";

		test::jail_t jail;
		jail.set_content("file.txt", original);

		document::document_ptr doc = document::create(jail.path("file.txt"));
		doc->open();

		doc->buffer().replace(0, doc->buffer().size(), buffer);
		doc->set_revision(doc->buffer().bump_revision());
		TS_ASSERT_EQUALS(content(doc), buffer);
		TS_ASSERT_EQUALS(doc->is_modified(), true);

		jail.set_content("file.txt", disk);
		notify(doc, NOTE_WRITE);
		TS_ASSERT_EQUALS(content(doc), merged);
		TS_ASSERT_EQUALS(doc->is_modified(), true);

		jail.set_content("file.txt", merged);
		notify(doc, NOTE_WRITE);
		TS_ASSERT_EQUALS(content(doc), merged);
		TS_ASSERT_EQUALS(doc->is_modified(), false);

		doc->close();
	}
};
