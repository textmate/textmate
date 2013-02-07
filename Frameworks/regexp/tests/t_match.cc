#include <regexp/regexp.h>

class MatchTests : public CxxTest::TestSuite
{
public:
	void test_match ()
	{
		regexp::match_t const match = regexp::search("(\\w+)\\s+(\\w+)", " foo bar fud");
		TS_ASSERT(match);
		TS_ASSERT_EQUALS(match[0], "foo bar");
		TS_ASSERT_EQUALS(match[1], "foo");
		TS_ASSERT_EQUALS(match[2], "bar");
		TS_ASSERT_EQUALS(match[3], NULL_STR);	
	}
};
