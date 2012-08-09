#include <buffer/buffer.h>
#include <selection/selection.h>
#include <text/format.h>

class AllWordsTests : public CxxTest::TestSuite
{
	std::string all_words (ng::buffer_t const& buf)
	{
		std::vector<std::string> res;
		citerate(range, ng::all_words(buf))
			res.push_back(buf.substr(range->first.index, range->last.index));
		return text::join(res, ", ");
	}

public:
	void test_all_words ()
	{
		TS_ASSERT_EQUALS(all_words("foo_bar"), "foo_bar");
		TS_ASSERT_EQUALS(all_words("foo-bar"), "foo, bar");
		TS_ASSERT_EQUALS(all_words(" foo-bar"), "foo, bar");
		TS_ASSERT_EQUALS(all_words(" foo-bar "), "foo, bar");
		TS_ASSERT_EQUALS(all_words("Ac̄̌count—æblegrød"), "Ac̄̌count, æblegrød");
		TS_ASSERT_EQUALS(all_words("南野 繁弘.\n"), "南野, 繁弘");
		TS_ASSERT_EQUALS(all_words("Surrogate: “𠻵”.\n"), "Surrogate, 𠻵");
	}
};
