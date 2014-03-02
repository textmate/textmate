#include <text/tokenize.h>

static std::string replace_token (std::string const& str, char token, std::string const& replacement)
{
	std::vector<std::string> v;
	for(auto const& component : text::tokenize(str.begin(), str.end(), token))
		v.push_back(component);
	return text::join(v, replacement);
}

void test_tokenize ()
{
	OAK_ASSERT_EQ(replace_token( "foo/bar",  '/', " » "),    "foo » bar"   );
	OAK_ASSERT_EQ(replace_token("/foo/bar",  '/', " » "), " » foo » bar"   );
	OAK_ASSERT_EQ(replace_token( "foo/bar/", '/', " » "),    "foo » bar » ");
	OAK_ASSERT_EQ(replace_token("/foo/bar/", '/', " » "), " » foo » bar » ");
}
