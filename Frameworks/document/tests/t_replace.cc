#include <document/document.h>

static uint32_t set_content (test::jail_t& jail, std::string const& file, std::string const& content)
{
	jail.set_content(file, content);

	boost::crc_32_type tmp;
	tmp.process_bytes(content.data(), content.size());
	return tmp.checksum();
}

void test_replace_single_line ()
{
	test::jail_t jail;
	uint32_t crc32 = set_content(jail, "test.txt", "Lorem ipsum dolor sit amet, consectetur adipisicing elit.");

	document::document_ptr doc = document::create(jail.path("test.txt"));
	std::multimap<std::pair<size_t, size_t>, std::string> replacements;
	replacements.insert(std::make_pair(std::make_pair(12, 17), "charum"));
	replacements.insert(std::make_pair(std::make_pair(28, 39), "abetarda"));
	doc->replace(replacements, crc32);
	doc->sync_open();
	OAK_ASSERT_EQ(doc->content(), "Lorem ipsum charum sit amet, abetarda adipisicing elit.");
	doc->close();
}

void test_replace_multiple_lines ()
{
	test::jail_t jail;
	uint32_t crc32 = set_content(jail, "test.txt",  "Foo\nBar\nFud\n");

	document::document_ptr doc = document::create(jail.path("test.txt"));
	std::multimap<std::pair<size_t, size_t>, std::string> replacements;
	replacements.insert(std::make_pair(std::make_pair(4, 7), "Jazz"));
	doc->replace(replacements, crc32);
	doc->sync_open();
	OAK_ASSERT_EQ(doc->content(), "Foo\nJazz\nFud\n");
	doc->close();
}

void test_replace_several ()
{
	test::jail_t jail;
	uint32_t crc32 = set_content(jail, "test.txt",  "Foo\nBar\nFud\n");

	std::multimap<std::pair<size_t, size_t>, std::string> replacements =
	{
		{ {  0,  1 }, "G"            },
		{ {  3,  3 }, "d"            },
		{ {  4,  7 }, "Jazz"         },
		{ { 12, 12 }, "A new line\n" },
	};

	document::document_ptr doc = document::create(jail.path("test.txt"));
	doc->replace(replacements, crc32);
	doc->sync_save();
	OAK_ASSERT_EQ(path::content(jail.path("test.txt")), "Good\nJazz\nFud\nA new line\n");
}
