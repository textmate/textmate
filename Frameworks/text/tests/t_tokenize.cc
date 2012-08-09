#include <text/tokenize.h>

class TokenizeTests : public CxxTest::TestSuite
{
	std::string replace_token (std::string const& str, char token, std::string const& replacement)
	{
		std::vector<std::string> v;
		citerate(component, text::tokenize(str.begin(), str.end(), token))
			v.push_back(*component);
		return text::join(v, replacement);
	}

public:
	void test_tokenize ()
	{
		TS_ASSERT_EQUALS(replace_token( "foo/bar",  '/', " » "),    "foo » bar"   );
		TS_ASSERT_EQUALS(replace_token("/foo/bar",  '/', " » "), " » foo » bar"   );
		TS_ASSERT_EQUALS(replace_token( "foo/bar/", '/', " » "),    "foo » bar » ");
		TS_ASSERT_EQUALS(replace_token("/foo/bar/", '/', " » "), " » foo » bar » ");
	}
};
