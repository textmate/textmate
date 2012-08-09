#include <text/ranker.h>

class RankerTests : public CxxTest::TestSuite
{
public:
	void test_capital_coverage ()
	{
		TS_ASSERT_LESS_THAN(oak::rank("otv", "OTVStatusBar.mm"), oak::rank("otv", "OakTextView.mm"));
	}

	void test_distance_to_start ()
	{
		TS_ASSERT_LESS_THAN(oak::rank("doc", "OakDocument.mm"), oak::rank("doc", "document.cc"));
	}

	void test_substring ()
	{
		TS_ASSERT_LESS_THAN(oak::rank("paste", "Encrypt With Password — Text"), oak::rank("paste", "Paste Selection Online — TextMate"));
	}

	void test_ranker ()
	{
		static std::string const files[] =
		{
			"OakTabBarView.mm",
			"OakTabBarView.h",
			"OakTextView.mm",
			"OakTextView.h"
		};

		std::multimap<double, std::string> ranked;
		iterate(file, files)
			ranked.insert(std::make_pair(oak::rank("otbv", *file), *file));
		ranked.erase(0);

		TS_ASSERT_EQUALS(ranked.size(), 2);
	}

	void test_ranker_1 ()
	{
		TS_ASSERT_DIFFERS(oak::rank("oakfilechooser", "OakFileChooser"       ), 0);
		TS_ASSERT_DIFFERS(oak::rank("oakfilechooser", "OakFinderLabelChooser"), 0);
		TS_ASSERT_LESS_THAN(oak::rank("oakfilechooser", "OakFinderLabelChooser"), oak::rank("oakfilechooser", "OakFileChooser"));
	}

	void test_ranker_2 ()
	{
		TS_ASSERT_DIFFERS(oak::rank("banne",  "Comments » Insert Comment Banner — Source"), 0);
		TS_ASSERT_DIFFERS(oak::rank("banner", "Comments » Insert Comment Banner — Source"), 0);
	}

	void test_normalize_filter ()
	{
		TS_ASSERT_EQUALS(oak::normalize_filter("t d l"), "tdl");
		TS_ASSERT_EQUALS(oak::normalize_filter("td l"),  "tdl");
		TS_ASSERT_EQUALS(oak::normalize_filter("TDL"),   "tdl");
		TS_ASSERT_EQUALS(oak::normalize_filter("TD L"),  "tdl");
		TS_ASSERT_EQUALS(oak::normalize_filter("TD l"),  "tdl");
	}

	void test_highlight_range ()
	{
		std::vector< std::pair<size_t, size_t> > ranges;
		double rank = oak::rank("HTo",  "HTMLOutput.h", &ranges);
		TS_ASSERT_LESS_THAN(0.0, rank);
		TS_ASSERT_LESS_THAN(rank, 1.0);
		TS_ASSERT_EQUALS(ranges.size(), 2);
		TS_ASSERT_EQUALS(ranges[0].first,  0);
		TS_ASSERT_EQUALS(ranges[0].second, 2);
		TS_ASSERT_EQUALS(ranges[1].first,  4);
		TS_ASSERT_EQUALS(ranges[1].second, 5);
	}
};
