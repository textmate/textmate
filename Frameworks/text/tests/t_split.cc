#include <text/parse.h>

void test_split_1 ()
{
	std::vector<std::string> words = text::split("this, is, nice");
	OAK_ASSERT_EQ(words.size(), 3);
	OAK_ASSERT_EQ(words[0], "this");
	OAK_ASSERT_EQ(words[1], "is");
	OAK_ASSERT_EQ(words[2], "nice");
}

void test_split_2 ()
{
	std::vector<std::string> words = text::split("this\nis\nnice\n", "\n");
	OAK_ASSERT_EQ(words.size(), 4);
	OAK_ASSERT_EQ(words[0], "this");
	OAK_ASSERT_EQ(words[1], "is");
	OAK_ASSERT_EQ(words[2], "nice");
	OAK_ASSERT_EQ(words[3], "");
}

void test_split_3 ()
{
	std::vector<std::string> words = text::split("this\n\nis\nnice\n", "\n");
	OAK_ASSERT_EQ(words.size(), 5);
	OAK_ASSERT_EQ(words[0], "this");
	OAK_ASSERT_EQ(words[1], "");
	OAK_ASSERT_EQ(words[2], "is");
	OAK_ASSERT_EQ(words[3], "nice");
	OAK_ASSERT_EQ(words[4], "");
}
