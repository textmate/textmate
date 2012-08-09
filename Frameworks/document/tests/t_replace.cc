class ReplaceTests : public CxxTest::TestSuite
{
public:
	void test_replace_single_line ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Lorem ipsum dolor sit amet, consectetur adipisicing elit.");

		document::document_ptr doc = document::create(jail.path("test.txt"));
		std::multimap<text::range_t, std::string> replacements;
		replacements.insert(std::make_pair(text::range_t("1:13-1:18"), "charum"));
		replacements.insert(std::make_pair(text::range_t("1:29-1:40"), "abetarda"));
		doc->replace(replacements);
		doc->open();
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Lorem ipsum charum sit amet, abetarda adipisicing elit.");
		doc->close();
	}

	void test_replace_multiple_lines ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Foo\nBar\nFud\n");
		
		document::document_ptr doc = document::create(jail.path("test.txt"));
		std::multimap<text::range_t, std::string> replacements;
		replacements.insert(std::make_pair(text::range_t("2:1-2:4"), "Jazz"));
		doc->replace(replacements);
		doc->open();
		TS_ASSERT_EQUALS(doc->buffer().substr(0, doc->buffer().size()), "Foo\nJazz\nFud\n");
		doc->close();
	}

	void test_replace_several ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Foo\nBar\nFud\n");
		
		std::multimap<text::range_t, std::string> replacements;
		replacements.insert(std::make_pair(text::range_t("1:1-1:2"), "G"));
		replacements.insert(std::make_pair(text::range_t("1:4-1:4"), "d"));
		replacements.insert(std::make_pair(text::range_t("2:1-2:4"), "Jazz"));
		replacements.insert(std::make_pair(text::range_t("4:1-4:1"), "A new line\n"));

		document::document_ptr doc = document::create(jail.path("test.txt"));
		doc->replace(replacements);
		doc->save();
		TS_ASSERT_EQUALS(path::content(jail.path("test.txt")), "Good\nJazz\nFud\nA new line\n");
	}
};
