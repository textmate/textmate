#include <plist/src/date.h>

void test_date_value ()
{
	oak::date_t now = oak::date_t::now();
	oak::date_t stillNow(now.time_value());
	OAK_ASSERT_LT(std::abs(now.value() - stillNow.value()), 0.5);
}

void test_date_time_value ()
{
	oak::date_t now = oak::date_t(time(nullptr));
	oak::date_t stillNow(now.value());
	OAK_ASSERT_EQ(now.value(), stillNow.value());

	stillNow = oak::date_t(to_s(now));
	OAK_ASSERT_EQ(now.value(), stillNow.value());
}
