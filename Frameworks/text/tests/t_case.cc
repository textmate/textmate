#include <text/case.h>

void test_upcase ()
{
	OAK_ASSERT_EQ(text::uppercase("æbleGRØD"), "ÆBLEGRØD");
}

void test_downcase ()
{
	OAK_ASSERT_EQ(text::lowercase("æbleGRØD"), "æblegrød");
}

void test_togglecase ()
{
	OAK_ASSERT_EQ(text::opposite_case("Den Grønne æbleGRØD"), "dEN gRØNNE ÆBLEgrød");
}
