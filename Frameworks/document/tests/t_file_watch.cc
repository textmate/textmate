#include <document/document.h>
#include <test/jail.h>

struct helper_t : document::document_t
{
	using document::document_t::watch_callback;
};

static void notify (document::document_ptr doc, int flags, std::string const& path = NULL_STR)
{
	((*doc).*(&helper_t::watch_callback))(flags, path, false);
}

static std::string content (document::document_ptr doc)
{
	return doc->content();
}

void test_file_watch ()
{
	test::jail_t jail;
	jail.set_content("file.txt", "Hello\n");

	document::document_ptr doc = document::create(jail.path("file.txt"));
	doc->sync_open();

	jail.set_content("file.txt", "Hello\nworld\n");
	notify(doc, NOTE_WRITE);
	OAK_ASSERT_EQ(content(doc), "Hello\nworld\n");

	doc->buffer().insert(5, ", ");
	doc->set_revision(doc->buffer().bump_revision());
	OAK_ASSERT_EQ(doc->is_modified(), true);
	jail.set_content("file.txt", "Hello, \nworld\n");
	notify(doc, NOTE_WRITE);
	OAK_ASSERT_EQ(content(doc), "Hello, \nworld\n");
	OAK_ASSERT_EQ(doc->is_modified(), false);

	jail.set_content("file.txt", "Hello\n");
	notify(doc, NOTE_WRITE);
	OAK_ASSERT_EQ(content(doc), "Hello\n");
	OAK_ASSERT_EQ(doc->is_modified(), false);

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
	doc->sync_open();

	doc->set_content(buffer);
	doc->set_revision(doc->buffer().bump_revision());
	OAK_ASSERT_EQ(content(doc), buffer);
	OAK_ASSERT_EQ(doc->is_modified(), true);

	jail.set_content("file.txt", disk);
	notify(doc, NOTE_WRITE);
	OAK_ASSERT_EQ(content(doc), merged);
	OAK_ASSERT_EQ(doc->is_modified(), true);

	jail.set_content("file.txt", merged);
	notify(doc, NOTE_WRITE);
	OAK_ASSERT_EQ(content(doc), merged);
	OAK_ASSERT_EQ(doc->is_modified(), false);

	doc->close();
}
