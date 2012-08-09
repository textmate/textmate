#import <ns/spellcheck.h>
#import <AppKit/NSApplication.h>

static class AppKitFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld() { return NSApplicationLoad(); }

} fixture;

class SpellcheckTests : public CxxTest::TestSuite
{
public:
	void test_spellcheck ()
	{
		std::string const str = "This is mispelled and agan.";

		std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length());
		TS_ASSERT_EQUALS(ranges.size(), 2);
		TS_ASSERT_EQUALS(ranges[0].first,  8);
		TS_ASSERT_EQUALS(ranges[0].last,  17);
		TS_ASSERT_EQUALS(ranges[1].first, 22);
		TS_ASSERT_EQUALS(ranges[1].last,  26);
	}

	void test_misspelled ()
	{
		TS_ASSERT_EQUALS(ns::is_misspelled("convinciable"), true);
		TS_ASSERT_EQUALS(ns::is_misspelled("convincible"), false);
	}

	void test_newlines_1 ()
	{
		std::string const str = "my(my)\n me";

		std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length());
		TS_ASSERT_EQUALS(ranges.size(), 0);
	}

	void test_newlines_2 ()
	{
		std::string const str = "my(my)\n qwong";

		std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length());
		TS_ASSERT_EQUALS(ranges.size(), 1);
		TS_ASSERT_EQUALS(ranges[0].first, 8);
		TS_ASSERT_EQUALS(ranges[0].last, 13);
	}
};
