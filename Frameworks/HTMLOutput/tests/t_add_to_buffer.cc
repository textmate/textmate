#include "../src/helpers/add_to_buffer.h"

static void small_adds (std::string const& expect, std::string const& src, size_t n, bool clearPrevious)
{
	char const* first = src.data();
	char const* last  = src.data() + src.size();

	std::string tmp, dst;
	while(first != last)
	{
		auto next = std::min(first + n, last);
		auto range = add_bytes_to_utf8_buffer(tmp, first, next, clearPrevious);
		OAK_ASSERT(utf8::is_valid(range.first, range.second));
		dst.insert(dst.end(), range.first, range.second);
		first = next;
	}

	if(!clearPrevious)
		OAK_ASSERT_EQ(std::string(tmp.begin(), utf8::find_safe_end(tmp.begin(), tmp.end())), expect);

	OAK_ASSERT_EQ(dst, expect);
}

void test_add_to_buffer ()
{
	static std::string const GoodUTF8 =
		"Surrogate: 𠻵\n"
		"Diacritics: c̄̌\n"
		"Thai: ไปกินปูดำที่บ้านกตัญญู\n"
		"Right-to-left: ومصادر اخرى ايضاً تُشير بأن اسم الآيفون سيكون\n"
		"Double width: 南野 繁弘\n";

	static std::string const BadUTF8 =
		"Surrogate: 𠻵\n"
		"Diacritics: \xFE" "c̄̌\xFE\n"
		"Thai: ไปกินปูดำที่บ้านกตัญญู\xE2\x99\n"
		"Right-to-left: ومصادر اخرى ايضاً تُشير بأن\xC0\xFE اسم الآيفون سيكون\n"
		"Double width: \xF0\xA0\xBB南野 繁弘\n\xE2\x99";

	for(size_t i = 1; i <= 16; ++i)
	{
		small_adds(GoodUTF8, GoodUTF8, i, true);
		small_adds(GoodUTF8, BadUTF8,  i, true);
		small_adds(GoodUTF8, GoodUTF8, i, false);
		small_adds(GoodUTF8, BadUTF8,  i, false);
	}
}
