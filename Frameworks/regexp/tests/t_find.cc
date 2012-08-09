#include <regexp/find.h>

class FindTests : public CxxTest::TestSuite
{
	typedef std::pair<ssize_t, ssize_t> range_t;
public:
	void test_regular_backward ()
	{
		std::string const text = "var = 32 && var == 5;";

		find::find_t matcher("var = ", find::backwards);
		range_t m = matcher.match(text.data(), text.size());
		TS_ASSERT_EQUALS(m, range_t(0, 6));
	}

	void test_regexp_backward ()
	{
		std::string const text = "Lorem text::uppercase ipsum.";

		find::find_t matcher("text::(lower|upper)", find::backwards|find::regular_expression);
		range_t m = matcher.match(text.data() + 10, text.size() - 10);
		TS_ASSERT_LESS_THAN(m.second, m.first);
		m = matcher.match(text.data(), 10);
		TS_ASSERT_LESS_THAN(m.second, m.first);
		m = matcher.match(NULL, 0);
		ssize_t base = text.size();
		TS_ASSERT_EQUALS(range_t(base + m.first, base + m.second), range_t(6, 17));
	}
};

