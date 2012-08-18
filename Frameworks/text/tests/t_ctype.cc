#include <text/ctype.h>

class CtypeTests : public CxxTest::TestSuite
{
public:
	void test_east_asia_width ()
	{
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x10FF), false);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x1100), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x1101), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x33FE), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x33FF), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3400), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3401), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3402), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x4DBE), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x4DBF), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x4DC0), false);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x4DC1), false);

		TS_ASSERT_EQUALS(text::is_east_asian_width(0x2E99), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x2E9A), false);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x2E9B), true);

		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3FFFC), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3FFFD), true);
		TS_ASSERT_EQUALS(text::is_east_asian_width(0x3FFFE), false);
	}
};
