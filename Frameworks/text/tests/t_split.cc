#include <text/parse.h>

class SplitTests : public CxxTest::TestSuite
{
public:
	void test_split_1 ()
	{
		std::vector<std::string> words = text::split("this, is, nice");
		TS_ASSERT_EQUALS(words.size(), 3);
		TS_ASSERT_EQUALS(words[0], "this");
		TS_ASSERT_EQUALS(words[1], "is");
		TS_ASSERT_EQUALS(words[2], "nice");
	}

	void test_split_2 ()
	{
		std::vector<std::string> words = text::split("this\nis\nnice\n", "\n");
		TS_ASSERT_EQUALS(words.size(), 4);
		TS_ASSERT_EQUALS(words[0], "this");
		TS_ASSERT_EQUALS(words[1], "is");
		TS_ASSERT_EQUALS(words[2], "nice");
		TS_ASSERT_EQUALS(words[3], "");
	}

	void test_split_3 ()
	{
		std::vector<std::string> words = text::split("this\n\nis\nnice\n", "\n");
		TS_ASSERT_EQUALS(words.size(), 5);
		TS_ASSERT_EQUALS(words[0], "this");
		TS_ASSERT_EQUALS(words[1], "");
		TS_ASSERT_EQUALS(words[2], "is");
		TS_ASSERT_EQUALS(words[3], "nice");
		TS_ASSERT_EQUALS(words[4], "");
	}
};
