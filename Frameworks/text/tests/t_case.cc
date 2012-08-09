#include <text/case.h>

using namespace text;

class CaseTests : public CxxTest::TestSuite
{
public:
	void test_upcase ()
	{
		TS_ASSERT_EQUALS(uppercase("æbleGRØD"), "ÆBLEGRØD");
	}

	void test_downcase ()
	{
		TS_ASSERT_EQUALS(lowercase("æbleGRØD"), "æblegrød");
	}

	void test_togglecase ()
	{
		TS_ASSERT_EQUALS(opposite_case("Den Grønne æbleGRØD"), "dEN gRØNNE ÆBLEgrød");
	}
};
