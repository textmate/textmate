#include <regexp/src/find.h>

typedef std::pair<size_t, size_t> range_t;

static std::vector<range_t> all_matches (find::find_t matcher, std::string const& text)
{
	std::vector<range_t> res;
	matcher.each_match(text.data(), text.size(), false /* no more data */, [&res](std::pair<size_t, size_t> const& m, std::map<std::string, std::string> const& captures){
		res.push_back(m);
	});
	return res;
}

void test_regular_backward ()
{
	find::find_t matcher("var = ");
	std::vector<range_t> ranges = all_matches(matcher, "var = 32 && var = 5;");

	OAK_ASSERT_EQ(ranges.size(), 2);
	OAK_ASSERT_EQ(ranges[0], range_t(0, 6));
	OAK_ASSERT_EQ(ranges[1], range_t(12, 18));
}

void test_regexp_backward ()
{
	find::find_t matcher("text::(lower|upper)", find::regular_expression);
	std::vector<range_t> ranges = all_matches(matcher, "Lorem text::uppercase ipsum.");

	OAK_ASSERT_EQ(ranges.size(), 1);
	OAK_ASSERT_EQ(ranges[0], range_t(6, 17));
}
