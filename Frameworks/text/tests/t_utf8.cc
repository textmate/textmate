#include <text/utf8.h>

class utf8_tests : public CxxTest::TestSuite
{
public:
	void test_safe_end ()
	{
		char const* first = "æblegrød";
		char const* last  = first + strlen(first);

		TS_ASSERT_EQUALS(utf8::find_safe_end(first, first),   first);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, first+1), first);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, first+2), first+2);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, first+3), first+3);

		TS_ASSERT_EQUALS(utf8::find_safe_end(first, last),    last);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, last-1),  last-1);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, last-2),  last-3);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, last-3),  last-3);
		TS_ASSERT_EQUALS(utf8::find_safe_end(first, last-4),  last-4);
	}

	void test_utf8_iterate ()
	{
		static std::string const str = "“Æblegrød…” — 𠻵";
		static std::vector<uint32_t> const expected = { 0x201C, 0xC6, 0x62, 0x6C, 0x65, 0x67, 0x72, 0xF8, 0x64, 0x2026, 0x201D, 0x20, 0x2014, 0x20, 0x20EF5 };

		std::vector<uint32_t> chars;
		foreach(ch, utf8::make(str.data()), utf8::make(str.data() + str.size()))
			chars.push_back(*ch);

		TS_ASSERT_EQUALS(chars, expected);
	}

	void test_to_ch ()
	{
		TS_ASSERT_EQUALS(utf8::to_ch("♥"),             0x2665);
		TS_ASSERT_EQUALS(utf8::to_ch("𠻵"),            0x20EF5);
		TS_ASSERT_EQUALS(utf8::to_ch("\U0010FFFF"),  0x10FFFF);

		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00000003)), 0x00000003);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00000030)), 0x00000030);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00000300)), 0x00000300);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00003000)), 0x00003000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00030000)), 0x00030000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00300000)), 0x00300000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x03000000)), 0x03000000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x30000000)), 0x30000000);

		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x20000003)), 0x20000003);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x02000030)), 0x02000030);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00200300)), 0x00200300);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00023000)), 0x00023000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00032000)), 0x00032000);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x00300200)), 0x00300200);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x03000020)), 0x03000020);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x30000002)), 0x30000002);

		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x3FFFFFFF)), 0x3fFFFFFF);
		TS_ASSERT_EQUALS(utf8::to_ch(utf8::to_s(0x40000000)), 0x40000000);
	}

	void test_to_s ()
	{
		TS_ASSERT_EQUALS(utf8::to_s(    0x2665),           "♥");
		TS_ASSERT_EQUALS(utf8::to_s(   0x20EF5),           "𠻵");
		TS_ASSERT_EQUALS(utf8::to_s(  0x10FFFF),  "\U0010FFFF");

		static uint32_t const chars[] = { 0x201C, 0xC6, 0x62, 0x6C, 0x65, 0x67, 0x72, 0xF8, 0x64, 0x2026, 0x201D, 0x20, 0x2014, 0x20, 0x20EF5 };
		std::string str = "";
		iterate(ch, chars)
			str += utf8::to_s(*ch);
		TS_ASSERT_EQUALS(str, "“Æblegrød…” — 𠻵");
	}
};
