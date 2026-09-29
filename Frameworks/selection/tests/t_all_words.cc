#include <buffer/src/buffer.h>
#include <selection/src/selection.h>
#include <text/src/format.h>

static std::string all_words (ng::buffer_t const& buf)
{
	std::vector<std::string> res;
	for(auto range : ng::all_words(buf))
		res.push_back(buf.substr(range.first.index, range.last.index));
	return text::join(res, ", ");
}

void test_all_words ()
{
	OAK_ASSERT_EQ(all_words("foo_bar"), "foo_bar");
	OAK_ASSERT_EQ(all_words("foo-bar"), "foo, bar");
	OAK_ASSERT_EQ(all_words(" foo-bar"), "foo, bar");
	OAK_ASSERT_EQ(all_words(" foo-bar "), "foo, bar");
	OAK_ASSERT_EQ(all_words("Ac̄̌count—æblegrød"), "Ac̄̌count, æblegrød");
	OAK_ASSERT_EQ(all_words("南野 繁弘.\n"), "南野, 繁弘");
	OAK_ASSERT_EQ(all_words("Surrogate: “𠻵”.\n"), "Surrogate, 𠻵");
}
