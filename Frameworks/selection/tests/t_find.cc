#include <selection/selection.h>

class FindTests : public CxxTest::TestSuite
{
	std::string matches (ng::buffer_t const& buffer, std::string const& str, find::options_t options = find::none, ng::ranges_t const& ranges = ng::ranges_t())
	{
		ng::ranges_t res;
		citerate(pair, ng::find(buffer, ranges, str, options | (ranges.empty() ? find::all_matches : find::none)))
			res.push_back(pair->first);
		return to_s(buffer, res);
	}

public:
	void test_find_forward ()
	{
		TS_ASSERT_EQUALS(matches("this (is (a test)).", "is", find::none, ng::ranges_t(4)), "1:7-1:9");
	}

	void test_find_backward ()
	{
		TS_ASSERT_EQUALS(matches("this (is (a test)).", "is", find::backwards, ng::ranges_t(4)), "1:3-1:5");
	}

	void test_find_forward_extend ()
	{
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "foo", find::extend_selection, ng::range_t(4, 7)), "1:5-1:8&1:9-1:12");
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "foo", find::extend_selection, ng::ranges_t{ ng::range_t(4, 7), ng::range_t(8, 11) }), "1:5-1:8&1:9-1:12&1:13-1:16");

		TS_ASSERT_EQUALS(matches("foo foo foo foo", "\\b", find::extend_selection|find::regular_expression, ng::range_t(4)), "1:5&1:8");
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "\\b", find::extend_selection|find::regular_expression, ng::ranges_t{ ng::range_t(4), ng::range_t(7) }), "1:5&1:8&1:9");
	}

	void test_find_backward_extend ()
	{
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "foo", find::extend_selection|find::backwards, ng::range_t(4, 7)), "1-1:4&1:5-1:8");
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "foo", find::extend_selection|find::backwards, ng::ranges_t{ ng::range_t(4, 7), ng::range_t(8, 11) }), "1-1:4&1:5-1:8&1:9-1:12");

		TS_ASSERT_EQUALS(matches("foo foo foo foo", "\\b", find::extend_selection|find::backwards|find::regular_expression, ng::range_t(4)), "1:4&1:5");
		TS_ASSERT_EQUALS(matches("foo foo foo foo", "\\b", find::extend_selection|find::backwards|find::regular_expression, ng::ranges_t{ ng::range_t(3), ng::range_t(4) }), "1&1:4&1:5");
	}

	void test_tricky_regexp ()
	{
		TS_ASSERT_EQUALS(matches("abcdef", "\\h+", find::regular_expression,                 ng::ranges_t(0)),   "1-1:7");
		TS_ASSERT_EQUALS(matches("abcdef", "\\h+", find::regular_expression,                 ng::ranges_t(3)), "1:4-1:7");
		TS_WARN("TODO: Investigate possible onigurma bug related to backwards search");
		// TS_ASSERT_EQUALS(matches("abcdef", "\\h+", find::regular_expression|find::backwards, ng::ranges_t(3)),   "1-1:4");

		TS_ASSERT_EQUALS(matches("Foo\nBar", "^.", find::regular_expression, ng::ranges_t(0)), "1-1:2");
		TS_ASSERT_EQUALS(matches("Foo\nBar", "^.", find::regular_expression, ng::ranges_t(1)), "2-2:2");
	}

	void test_find_all ()
	{
		TS_ASSERT_EQUALS(matches("this (is (a test)).", "is"), "1:3-1:5&1:7-1:9");
	}

	void test_find_regexp ()
	{
		static find::options_t const kRegExp = find::regular_expression;

		TS_ASSERT_EQUALS(matches("test",  ".*", kRegExp),                     "1-1:5&1:5"); // How do we best avoid the EOF match?
		TS_ASSERT_EQUALS(matches("test",  ".+", kRegExp),                         "1-1:5");
		TS_ASSERT_EQUALS(matches("test",   ".", kRegExp), "1-1:2&1:2-1:3&1:3-1:4&1:4-1:5");

		TS_ASSERT_EQUALS(matches("test", "\\A", kRegExp),                             "1");
		TS_ASSERT_EQUALS(matches("test",   "^", kRegExp),                             "1");
		TS_ASSERT_EQUALS(matches("test",   "$", kRegExp),                           "1:5");
		TS_ASSERT_EQUALS(matches("test", "\\z", kRegExp),                           "1:5");

		TS_ASSERT_EQUALS(matches("foo\nbar", "\\A", kRegExp),       "1");
		TS_ASSERT_EQUALS(matches("foo\nbar",   "^", kRegExp),     "1&2");
		TS_ASSERT_EQUALS(matches("foo\nbar",   "$", kRegExp), "1:4&2:4");
		TS_ASSERT_EQUALS(matches("foo\nbar", "\\z", kRegExp),     "2:4");

		TS_ASSERT_EQUALS(matches("test",    "\\b", kRegExp),           "1&1:5");
		TS_ASSERT_EQUALS(matches("test",  "(?=.)", kRegExp),   "1&1:2&1:3&1:4");
		TS_ASSERT_EQUALS(matches("test", "(?<=.)", kRegExp), "1:2&1:3&1:4&1:5");
	}
};
