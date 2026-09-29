#include <selection/src/selection.h>

static std::string search (std::string const& needle, std::string haystack, find::options_t options = find::none, bool* didWrap = nullptr)
{
	static std::string const kMarker = "‸";

	ng::ranges_t ranges;

	auto first = ng::index_t(haystack.find(kMarker));
	if(first.index != std::string::npos)
	{
		ng::range_t range(first);

		haystack.replace(range.first.index, kMarker.size(), "");
		auto last = ng::index_t(haystack.find(kMarker));
		if(last.index != std::string::npos)
		{
			range.last = last;
			haystack.replace(last.index, kMarker.size(), "");
		}

		ranges.push_back(range);
	}
	else
	{
		options |= find::all_matches;
	}

	ng::buffer_t buffer;
	buffer.insert(0, haystack);

	std::set<size_t> marks;
	for(auto pair : ng::find(buffer, ranges, needle, options, ng::ranges_t(), didWrap))
	{
		marks.insert(pair.first.first.index);
		marks.insert(pair.first.last.index);
	}

	riterate(index, marks)
		haystack.insert(*index, kMarker);

	return haystack;
}

static std::string matches (ng::buffer_t const& buffer, std::string const& str, find::options_t options = find::none, ng::ranges_t const& ranges = ng::ranges_t())
{
	ng::ranges_t res;
	for(auto const& pair : ng::find(buffer, ranges, str, options | (ranges.empty() ? find::all_matches : find::none)))
		res.push_back(pair.first);
	return to_s(buffer, res);
}

void test_find_forward ()
{
	OAK_ASSERT_EQ(matches("this (is (a test)).", "is", find::none, ng::ranges_t(4)), "1:7-1:9");

	bool didWrap = false;
	auto options = find::none;

	OAK_ASSERT_EQ("‸xx‸ ‸xx‸ ‸xx‸ ‸xx‸", search("xx", "xx xx xx xx",         options, &didWrap)); OAK_ASSERT(!didWrap);

	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "‸xx xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "x‸x xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx‸ xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx ‸xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "‸x‸x xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "‸xx‸ xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "‸xx ‸xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "‸xx x‸x xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "xx xx xx xx‸",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "xx xx xx x‸x",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx ‸xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx‸ xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "xx xx xx x‸x‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "xx xx xx ‸xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx‸ xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx x‸x xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx ‸xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx‸ xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx x‸x xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx ‸xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx‸ xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "‸xx xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);

	auto wrapAround = options | find::wrap_around;
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx xx xx‸",     wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx xx x‸x",     wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx ‸xx",     wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx‸ xx",     wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx xx x‸x‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx xx ‸xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx xx‸ xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx ‸xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx xx‸ xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx x‸x xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx ‸xx xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx‸ xx xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "‸xx xx xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
}

void test_find_backward ()
{
	OAK_ASSERT_EQ(matches("this (is (a test)).", "is", find::backwards, ng::ranges_t(4)), "1:3-1:5");

	bool didWrap = false;
	auto options = find::backwards;

	OAK_ASSERT_EQ("‸xx‸ ‸xx‸ ‸xx‸ ‸xx‸", search("xx", "xx xx xx xx",         options, &didWrap)); OAK_ASSERT(!didWrap);

	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "‸xx xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "x‸x xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx‸ xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx ‸xx xx xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "‸x‸x xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx",         search("xx", "‸xx‸ xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "‸xx ‸xx xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "‸xx x‸x xx xx",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "xx xx xx xx‸",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx x‸x",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx ‸xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx‸ xx",        options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx x‸x‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx ‸xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx‸ xx",       search("xx", "xx xx xx‸ xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx xx x‸x xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx xx ‸xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx‸ xx xx",       search("xx", "xx xx‸ xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx x‸x xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx ‸xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx‸ xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx xx xx‸",       options, &didWrap)); OAK_ASSERT(!didWrap);

	auto wrapAround = options | find::wrap_around;
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx xx xx",     wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "x‸x xx xx xx",     wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx‸ xx xx xx",     wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx‸ xx xx xx",       search("xx", "xx ‸xx xx xx",     wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸x‸x xx xx xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx‸ xx xx xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx ‸xx xx xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx‸ xx xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx ‸xx xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx x‸x xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx xx‸ xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx xx ‸xx",    wrapAround, &didWrap)); OAK_ASSERT(didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx‸",       search("xx", "‸xx xx xx xx‸",    wrapAround, &didWrap)); OAK_ASSERT(!didWrap);
}

void test_find_forward_extend ()
{
	OAK_ASSERT_EQ(matches("foo foo foo foo", "foo", find::extend_selection, ng::range_t(4, 7)), "1:5-1:8&1:9-1:12");
	OAK_ASSERT_EQ(matches("foo foo foo foo", "foo", find::extend_selection, ng::ranges_t{ ng::range_t(4, 7), ng::range_t(8, 11) }), "1:5-1:8&1:9-1:12&1:13-1:16");

	OAK_ASSERT_EQ(matches("foo foo foo foo", "\\b", find::extend_selection|find::regular_expression, ng::range_t(4)), "1:5&1:8");
	OAK_ASSERT_EQ(matches("foo foo foo foo", "\\b", find::extend_selection|find::regular_expression, ng::ranges_t{ ng::range_t(4), ng::range_t(7) }), "1:5&1:8&1:9");
}

void test_find_backward_extend ()
{
	OAK_ASSERT_EQ(matches("foo foo foo foo", "foo", find::extend_selection|find::backwards, ng::range_t(4, 7)), "1-1:4&1:5-1:8");
	OAK_ASSERT_EQ(matches("foo foo foo foo", "foo", find::extend_selection|find::backwards, ng::ranges_t{ ng::range_t(4, 7), ng::range_t(8, 11) }), "1-1:4&1:5-1:8&1:9-1:12");

	OAK_ASSERT_EQ(matches("foo foo foo foo", "\\b", find::extend_selection|find::backwards|find::regular_expression, ng::range_t(4)), "1:4&1:5");
	OAK_ASSERT_EQ(matches("foo foo foo foo", "\\b", find::extend_selection|find::backwards|find::regular_expression, ng::ranges_t{ ng::range_t(3), ng::range_t(4) }), "1&1:4&1:5");
}

void test_find_forward_regexp ()
{
	bool didWrap = false;
	auto options = find::regular_expression | find::wrap_around;

	OAK_ASSERT_EQ("‸xx‸ ‸xx‸ ‸xx‸ ‸xx‸", search("\\b", "xx xx xx xx",  options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx‸ xx xx xx",        search("\\b", "‸xx xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx‸ xx xx xx",        search("\\b", "x‸x xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx ‸xx xx xx",        search("\\b", "xx‸ xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx‸ xx xx",        search("\\b", "xx ‸xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx",        search("\\b", "xx xx xx‸ xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx‸",        search("\\b", "xx xx xx ‸xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx xx xx xx",        search("\\b", "xx xx xx xx‸", options, &didWrap)); OAK_ASSERT(didWrap);
}

void test_find_backward_regexp ()
{
	bool didWrap = false;
	auto options = find::regular_expression | find::backwards | find::wrap_around;

	OAK_ASSERT_EQ("xx xx xx ‸xx",        search("\\b", "xx xx xx xx‸", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx ‸xx",        search("\\b", "xx xx xx x‸x", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx‸ xx",        search("\\b", "xx xx xx ‸xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx ‸xx xx",        search("\\b", "xx xx xx‸ xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx‸ xx xx xx",        search("\\b", "xx ‸xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("‸xx xx xx xx",        search("\\b", "xx‸ xx xx xx", options, &didWrap)); OAK_ASSERT(!didWrap);
	OAK_ASSERT_EQ("xx xx xx xx‸",        search("\\b", "‸xx xx xx xx", options, &didWrap)); OAK_ASSERT(didWrap);
}

void test_tricky_regexp ()
{
	OAK_ASSERT_EQ(matches("abcdef", "\\h+", find::regular_expression,                 ng::ranges_t(0)),   "1-1:7");
	OAK_ASSERT_EQ(matches("abcdef", "\\h+", find::regular_expression,                 ng::ranges_t(3)), "1:4-1:7");

	// The following test fails, see: https://github.com/k-takata/Onigmo/issues/22
	// OAK_ASSERT_EQ(matches("abcdef", "\\h+", find::regular_expression|find::backwards, ng::ranges_t(3)),   "1-1:4");

	OAK_ASSERT_EQ(matches("Foo\nBar", "^.", find::regular_expression, ng::ranges_t(0)), "1-1:2");
	OAK_ASSERT_EQ(matches("Foo\nBar", "^.", find::regular_expression, ng::ranges_t(1)), "2-2:2");
}

void test_find_all ()
{
	OAK_ASSERT_EQ(matches("this (is (a test)).", "is"), "1:3-1:5&1:7-1:9");
	OAK_ASSERT_EQ("‸c̄̌‸ ‸𠻵‸", search("\\b", "c̄̌ 𠻵", find::regular_expression));
}

void test_find_regexp ()
{
	static find::options_t const kRegExp = find::regular_expression;

	OAK_ASSERT_EQ(matches("test",  ".*", kRegExp),                     "1-1:5&1:5"); // How do we best avoid the EOF match?
	OAK_ASSERT_EQ(matches("test",  ".+", kRegExp),                         "1-1:5");
	OAK_ASSERT_EQ(matches("test",   ".", kRegExp), "1-1:2&1:2-1:3&1:3-1:4&1:4-1:5");

	OAK_ASSERT_EQ(matches("test", "\\A", kRegExp),                             "1");
	OAK_ASSERT_EQ(matches("test",   "^", kRegExp),                             "1");
	OAK_ASSERT_EQ(matches("test",   "$", kRegExp),                           "1:5");
	OAK_ASSERT_EQ(matches("test", "\\z", kRegExp),                           "1:5");

	OAK_ASSERT_EQ(matches("foo\nbar", "\\A", kRegExp),       "1");
	OAK_ASSERT_EQ(matches("foo\nbar",   "^", kRegExp),     "1&2");
	OAK_ASSERT_EQ(matches("foo\nbar",   "$", kRegExp), "1:4&2:4");
	OAK_ASSERT_EQ(matches("foo\nbar", "\\z", kRegExp),     "2:4");

	OAK_ASSERT_EQ(matches("test",    "\\b", kRegExp),           "1&1:5");
	OAK_ASSERT_EQ(matches("test",  "(?=.)", kRegExp),   "1&1:2&1:3&1:4");
	OAK_ASSERT_EQ(matches("test", "(?<=.)", kRegExp), "1:2&1:3&1:4&1:5");
}
