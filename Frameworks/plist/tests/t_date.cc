#include <plist/date.h>

class DateTests : public CxxTest::TestSuite
{
public:
	void test_date_value ()
	{
		oak::date_t now = oak::date_t::now();
		oak::date_t stillNow(now.time_value());
		TS_ASSERT_DELTA(now.value(), stillNow.value(), 0.5);
	}

	void test_date_time_value ()
	{
		oak::date_t now = oak::date_t(time(NULL));
		oak::date_t stillNow(now.value());
		TS_ASSERT_EQUALS(now.value(), stillNow.value());

		stillNow = oak::date_t(to_s(now));
		TS_ASSERT_EQUALS(now.value(), stillNow.value());
	}
};
