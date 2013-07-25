#include <text/format.h>

void test_format_number ()
{
	OAK_ASSERT_EQ(text::format_size(1),                "1 byte");
	OAK_ASSERT_EQ(text::format_size(2),                "2 bytes");
	OAK_ASSERT_EQ(text::format_size(600),            "600 bytes");
	OAK_ASSERT_EQ(text::format_size(1024),           "1.0 KiB");
	OAK_ASSERT_EQ(text::format_size(5*1024),         "5.0 KiB");
	OAK_ASSERT_EQ(text::format_size(5*1024+512),     "5.5 KiB");
	OAK_ASSERT_EQ(text::format_size(1024*1024),      "1.0 MiB");
	OAK_ASSERT_EQ(text::format_size(5*1024*1024),    "5.0 MiB");
	OAK_ASSERT_EQ(text::format_size(1024*1024*1024), "1.0 GiB");
}
