#include <text/format.h>

class format_tests : public CxxTest::TestSuite
{
public:
	void test_format_number ()
	{
		TS_ASSERT_EQUALS(text::format_size(1),                "1 byte");
		TS_ASSERT_EQUALS(text::format_size(2),                "2 bytes");
		TS_ASSERT_EQUALS(text::format_size(600),            "600 bytes");
		TS_ASSERT_EQUALS(text::format_size(1024),           "1.0 KiB");
		TS_ASSERT_EQUALS(text::format_size(5*1024),         "5.0 KiB");
		TS_ASSERT_EQUALS(text::format_size(5*1024+512),     "5.5 KiB");
		TS_ASSERT_EQUALS(text::format_size(1024*1024),      "1.0 MiB");
		TS_ASSERT_EQUALS(text::format_size(5*1024*1024),    "5.0 MiB");
		TS_ASSERT_EQUALS(text::format_size(1024*1024*1024), "1.0 GiB");
	}
};
