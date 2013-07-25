#include <text/ctype.h>

void test_east_asia_width ()
{
	OAK_ASSERT_EQ(text::is_east_asian_width(0x10FF), false);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x1100), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x1101), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x33FE), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x33FF), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x3400), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x3401), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x3402), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x4DBE), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x4DBF), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x4DC0), false);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x4DC1), false);

	OAK_ASSERT_EQ(text::is_east_asian_width(0x2E99), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x2E9A), false);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x2E9B), true);

	OAK_ASSERT_EQ(text::is_east_asian_width(0x3FFFC), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x3FFFD), true);
	OAK_ASSERT_EQ(text::is_east_asian_width(0x3FFFE), false);
}
