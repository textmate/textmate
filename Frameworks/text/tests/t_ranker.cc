#include <text/ranker.h>

void test_capital_coverage ()
{
	OAK_ASSERT_LT(oak::rank("otv", "OTVStatusBar.mm"), oak::rank("otv", "OakTextView.mm"));
}

void test_distance_to_start ()
{
	OAK_ASSERT_LT(oak::rank("doc", "OakDocument.mm"), oak::rank("doc", "document.cc"));
}

void test_substring ()
{
	OAK_ASSERT_LT(oak::rank("paste", "Encrypt With Password — Text"), oak::rank("paste", "Paste Selection Online — TextMate"));
}

void test_fullmatch ()
{
	OAK_ASSERT_LT(oak::rank("rmate", "RMateServer.cc"), oak::rank("rmate", "rmate"));
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
	for(auto const& file : files)
		ranked.insert(std::make_pair(oak::rank("otbv", file), file));
	ranked.erase(0);

	OAK_ASSERT_EQ(ranked.size(), 2);
}

void test_ranker_1 ()
{
	OAK_ASSERT_NE(oak::rank("oakfilechooser", "OakFileChooser"       ), 0);
	OAK_ASSERT_NE(oak::rank("oakfilechooser", "OakFinderLabelChooser"), 0);
	OAK_ASSERT_LT(oak::rank("oakfilechooser", "OakFinderLabelChooser"), oak::rank("oakfilechooser", "OakFileChooser"));
}

void test_ranker_2 ()
{
	OAK_ASSERT_NE(oak::rank("banne",  "Comments » Insert Comment Banner — Source"), 0);
	OAK_ASSERT_NE(oak::rank("banner", "Comments » Insert Comment Banner — Source"), 0);
}

void test_normalize_filter ()
{
	OAK_ASSERT_EQ(oak::normalize_filter("t d l"), "tdl");
	OAK_ASSERT_EQ(oak::normalize_filter("td l"),  "tdl");
	OAK_ASSERT_EQ(oak::normalize_filter("TDL"),   "tdl");
	OAK_ASSERT_EQ(oak::normalize_filter("TD L"),  "tdl");
	OAK_ASSERT_EQ(oak::normalize_filter("TD l"),  "tdl");
}

void test_highlight_range ()
{
	std::vector< std::pair<size_t, size_t> > ranges;
	double rank = oak::rank("HTo",  "HTMLOutput.h", &ranges);
	OAK_ASSERT_LT(0.0, rank);
	OAK_ASSERT_LT(rank, 1.0);
	OAK_ASSERT_EQ(ranges.size(), 2);
	OAK_ASSERT_EQ(ranges[0].first,  0);
	OAK_ASSERT_EQ(ranges[0].second, 2);
	OAK_ASSERT_EQ(ranges[1].first,  4);
	OAK_ASSERT_EQ(ranges[1].second, 5);
}
