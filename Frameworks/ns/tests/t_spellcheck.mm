#import <ns/src/spellcheck.h>
#import <AppKit/NSApplication.h>

void setup ()
{
	NSApplicationLoad();
	[NSSpellChecker sharedSpellChecker];
}

void test_spellcheck ()
{
	std::string const str = "This is mispelled and agan.";

	std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length(), "en");
	OAK_ASSERT_EQ(ranges.size(), 2);
	OAK_ASSERT_EQ(ranges[0].first,  8);
	OAK_ASSERT_EQ(ranges[0].last,  17);
	OAK_ASSERT_EQ(ranges[1].first, 22);
	OAK_ASSERT_EQ(ranges[1].last,  26);
}

void test_misspelled ()
{
	OAK_ASSERT_EQ(ns::is_misspelled(std::string("convinciable"), "en"), true);
	OAK_ASSERT_EQ(ns::is_misspelled(std::string("convincible"), "en"), false);
}

void test_newlines_1 ()
{
	std::string const str = "my(my)\n me";

	std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length(), "en");
	OAK_ASSERT_EQ(ranges.size(), 0);
}

void test_newlines_2 ()
{
	std::string const str = "my(my)\n qwong";

	std::vector<ns::range_t> ranges = ns::spellcheck(str.data(), str.data() + str.length(), "en");
	OAK_ASSERT_EQ(ranges.size(), 1);
	OAK_ASSERT_EQ(ranges[0].first, 8);
	OAK_ASSERT_EQ(ranges[0].last, 13);
}
