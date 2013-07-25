#include <regexp/find.h>

typedef std::pair<ssize_t, ssize_t> range_t;

void test_regular_backward ()
{
	std::string const text = "var = 32 && var == 5;";

	find::find_t matcher("var = ", find::backwards);
	range_t m = matcher.match(text.data(), text.size());
	OAK_ASSERT_EQ(m, range_t(0, 6));
}

void test_regexp_backward ()
{
	std::string const text = "Lorem text::uppercase ipsum.";

	find::find_t matcher("text::(lower|upper)", find::backwards|find::regular_expression);
	range_t m = matcher.match(text.data() + 10, text.size() - 10);
	OAK_ASSERT_LT(m.second, m.first);
	m = matcher.match(text.data(), 10);
	OAK_ASSERT_LT(m.second, m.first);
	m = matcher.match(NULL, 0);
	ssize_t base = text.size();
	OAK_ASSERT_EQ(range_t(base + m.first, base + m.second), range_t(6, 17));
}

