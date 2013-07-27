class ReplaceTests : public CxxTest::TestSuite
{
public:
	void test_replace_single_line ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Lorem ipsum dolor sit amet, consectetur adipisicing elit.");

		document::document_ptr doc = document::create(jail.path("test.txt"));
		std::multimap<std::pair<size_t, size_t>, std::string> replacements;
		replacements.insert(std::make_pair(std::make_pair(12, 17), "charum"));
		replacements.insert(std::make_pair(std::make_pair(28, 39), "abetarda"));
		doc->replace(replacements);
		doc->open();
		TS_ASSERT_EQUALS(doc->content(), "Lorem ipsum charum sit amet, abetarda adipisicing elit.");
		doc->close();
	}

	void test_replace_multiple_lines ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Foo\nBar\nFud\n");
		
		document::document_ptr doc = document::create(jail.path("test.txt"));
		std::multimap<std::pair<size_t, size_t>, std::string> replacements;
		replacements.insert(std::make_pair(std::make_pair(4, 7), "Jazz"));
		doc->replace(replacements);
		doc->open();
		TS_ASSERT_EQUALS(doc->content(), "Foo\nJazz\nFud\n");
		doc->close();
	}

	void test_replace_several ()
	{
		test::jail_t jail;
		jail.set_content("test.txt",  "Foo\nBar\nFud\n");
		
		std::multimap<std::pair<size_t, size_t>, std::string> replacements;
		replacements.insert(std::make_pair(std::make_pair(0, 1), "G"));
		replacements.insert(std::make_pair(std::make_pair(3, 3), "d"));
		replacements.insert(std::make_pair(std::make_pair(4, 7), "Jazz"));
		replacements.insert(std::make_pair(std::make_pair(12, 12), "A new line\n"));

		document::document_ptr doc = document::create(jail.path("test.txt"));
		doc->replace(replacements);
		doc->save();
		TS_ASSERT_EQUALS(path::content(jail.path("test.txt")), "Good\nJazz\nFud\nA new line\n");
	}
};
