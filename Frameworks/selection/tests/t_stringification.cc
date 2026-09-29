#include <buffer/src/buffer.h>
#include <selection/src/selection.h>

using ng::from_string;
using ng::index_t;
using ng::range_t;
using ng::ranges_t;

static std::string round_trip (ng::buffer_t const& buf, std::string const& str)
{
	return to_s(buf, from_string(buf, str));
}

static range_t freehanded (size_t index, size_t offset)
{
	return range_t(index_t(index, offset), index_t(index, offset), false, true);
}

void test_stringification ()
{
	ng::buffer_t buf;
	buf.insert(0, "Lorem ipsum dolor\nsit amet,\nconsectetur.");

	OAK_ASSERT_EQ(to_s(buf, ranges_t( 0)),             "1");
	OAK_ASSERT_EQ(to_s(buf, ranges_t(18)),             "2");
	OAK_ASSERT_EQ(to_s(buf, ranges_t( 1)),           "1:2");
	OAK_ASSERT_EQ(to_s(buf, ranges_t(19)),           "2:2");
	OAK_ASSERT_EQ(to_s(buf, range_t(0, 19)),       "1-2:2");
	OAK_ASSERT_EQ(to_s(buf, range_t(0, 19, true)), "1x2:2");
	OAK_ASSERT_EQ(to_s(buf, freehanded( 0, 1)),      "1+1");
	OAK_ASSERT_EQ(to_s(buf, freehanded(18, 1)),      "2+1");
	OAK_ASSERT_EQ(to_s(buf, freehanded( 1, 1)),    "1:2+1");
	OAK_ASSERT_EQ(to_s(buf, freehanded(19, 1)),    "2:2+1");

	OAK_ASSERT_EQ(round_trip(buf,         "1:1"),         "1");
	OAK_ASSERT_EQ(round_trip(buf,         "2:1"),         "2");
	OAK_ASSERT_EQ(round_trip(buf,         "1:2"),       "1:2");
	OAK_ASSERT_EQ(round_trip(buf,         "2:2"),       "2:2");
	OAK_ASSERT_EQ(round_trip(buf,     "1:1-2:2"),     "1-2:2");
	OAK_ASSERT_EQ(round_trip(buf,     "1:1x2:2"),     "1x2:2");
	OAK_ASSERT_EQ(round_trip(buf,         "1+1"),       "1+1");
	OAK_ASSERT_EQ(round_trip(buf, "1:1+1x2:2+1"), "1+1x2:2+1");
	OAK_ASSERT_EQ(round_trip(buf,         "1&2"),       "1&2");
}
