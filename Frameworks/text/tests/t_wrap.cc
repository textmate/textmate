#include <text/parse.h>
#include <text/format.h>
#include <text/utf8.h>
#include <oak/oak.h>

static size_t const kWrapColumn = 10;
static size_t const kTabSize    = 3;

static std::string soft_breaks (std::string const& str)
{
	std::string res = "";
	size_t from = 0;
	for(auto const& offset : text::soft_breaks(str, kWrapColumn, kTabSize))
	{
		res += str.substr(from, offset - from) + "‸";
		from = offset;
	}
	return res + str.substr(from);
}

static std::string indented_wrap (std::string const& str, size_t width, size_t indentWidth = 3)
{
	std::vector<std::string> lines;

	size_t from = 0;
	for(auto const& offset : text::soft_breaks(str, width, kTabSize, indentWidth))
	{
		lines.push_back(str.substr(from, offset - from));
		from = offset;
	}
	lines.push_back(str.substr(from));
	return text::join(lines, "\n" + std::string(indentWidth, ' '));
}

void test_soft_breaks ()
{
	OAK_ASSERT_EQ(soft_breaks(""),                            ""                           );
	OAK_ASSERT_EQ(soft_breaks("foo"),                         "foo"                        );
	OAK_ASSERT_EQ(soft_breaks("foo bar"),                     "foo bar"                    );
	OAK_ASSERT_EQ(soft_breaks("foo bar xy"),                  "foo bar xy"                 );
	OAK_ASSERT_EQ(soft_breaks("foo bar xyz"),                 "foo bar ‸xyz"               );
	OAK_ASSERT_EQ(soft_breaks("foo-bar-xy"),                  "foo-bar-xy"                 );
	OAK_ASSERT_EQ(soft_breaks("foo-bar-xyz"),                 "foo-bar-xy‸z"               );
	OAK_ASSERT_EQ(soft_breaks("foo-bar-xyz fud baz\n"),       "foo-bar-xy‸z fud baz\n"     );
	OAK_ASSERT_EQ(soft_breaks("foo-bar-xyz fud baz xyz"),     "foo-bar-xy‸z fud baz ‸xyz"  );
	OAK_ASSERT_EQ(soft_breaks("foo bar fud"),                 "foo bar ‸fud"               );
	OAK_ASSERT_EQ(soft_breaks("foo bar  fud"),                "foo bar  ‸fud"              );
	OAK_ASSERT_EQ(soft_breaks("foo bar   fud"),               "foo bar   ‸fud"             );
	OAK_ASSERT_EQ(soft_breaks("foo bar    fud"),              "foo bar   ‸ fud"            );
	OAK_ASSERT_EQ(soft_breaks("foo bar x fud"),               "foo bar x ‸fud"             );
	OAK_ASSERT_EQ(soft_breaks("foo bar xy fud"),              "foo bar ‸xy fud"            );
	OAK_ASSERT_EQ(soft_breaks("foo bar xyz fud"),             "foo bar ‸xyz fud"           );
	OAK_ASSERT_EQ(soft_breaks("foo bar x\nfud"),              "foo bar x\nfud"             );
	OAK_ASSERT_EQ(soft_breaks("foo bar xy\nfud"),             "foo bar xy\nfud"            );
	OAK_ASSERT_EQ(soft_breaks("foo bar xyz\nfud"),            "foo bar ‸xyz\nfud"          );
	OAK_ASSERT_EQ(soft_breaks("1234567890\n1234567890\n"),    "1234567890\n1234567890\n"   );
	OAK_ASSERT_EQ(soft_breaks("1234 67890\n1234 67890\n"),    "1234 67890\n1234 67890\n"   );
	OAK_ASSERT_EQ(soft_breaks("12345 67890\n1234567890\n"),   "12345 ‸67890\n1234567890\n" );
}

void test_indented_wrap ()
{
	OAK_ASSERT_EQ(indented_wrap(" • This is a paragraph.", 10), " • This \n   is a \n   paragra\n   ph.");
	OAK_ASSERT_EQ(indented_wrap(" • This is a long paragraph.", 10), " • This \n   is a \n   long \n   paragra\n   ph.");
	OAK_ASSERT_EQ(indented_wrap("   A 12345678901234567890",      12, 6), "   A \n      123456\n      789012\n      345678\n      90");
	OAK_ASSERT_EQ(indented_wrap("   AB 12345678901234567890",     12, 6), "   AB \n      123456\n      789012\n      345678\n      90");
	OAK_ASSERT_EQ(indented_wrap("   ABC 12345678901234567890",    12, 6), "   ABC \n      123456\n      789012\n      345678\n      90");
	OAK_ASSERT_EQ(indented_wrap("123456789012345678901234567890", 12, 6), "123456789012\n      345678\n      901234\n      567890");
	OAK_ASSERT_EQ(indented_wrap("// I consectetur adipisicing",   20, 7), "// \n       I consectetur\n        adipisicing");
}
