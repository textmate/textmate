#include <buffer/src/buffer.h>
#include <selection/src/selection.h>
#include <test/bundle_index.h>

void setup_fixtures ()
{
	static std::string HighlightPairs =
		"{	name     = 'Highlight Pairs';"
		"	settings = {"
		"		highlightPairs = ("
		"			( '(', ')' ),"
		"			( '{', '}' ),"
		"			( '[', ']' ),"
		"			( '“', '”' ),"
		"			( '/<\\w+[^>]*>/', '/</\\w+>/' ),"
		"		);"
		"	};"
		"}";

	test::bundle_index_t bundleIndex;
	bundleIndex.add(bundles::kItemTypeSettings, HighlightPairs);
	bundleIndex.commit();
}

static std::string move (std::string input, move_unit_type action)
{
	static std::string const kMarker = "‸";

	size_t pos = input.find(kMarker);
	OAK_ASSERT_NE(pos, std::string::npos);
	input.replace(pos, kMarker.size(), "");

	ng::buffer_t buf;
	buf.insert(0, input);
	for(auto range : ng::move(buf, ng::range_t(pos), action))
		buf.replace(range.min().index, range.max().index, "‸");
	return buf.substr(0, buf.size());
}

void test_brace_movement ()
{
	OAK_ASSERT_EQ(move("‸this (is (a test)).", kSelectionMoveToBeginOfTypingPair), "‸this (is (a test)).");
	OAK_ASSERT_EQ(move("this ‸(is (a test)).", kSelectionMoveToBeginOfTypingPair), "this ‸(is (a test)).");
	OAK_ASSERT_EQ(move("this (‸is (a test)).", kSelectionMoveToBeginOfTypingPair), "this (‸is (a test)).");
	OAK_ASSERT_EQ(move("this (is ‸(a test)).", kSelectionMoveToBeginOfTypingPair), "this (‸is (a test)).");
	OAK_ASSERT_EQ(move("this (is (‸a test)).", kSelectionMoveToBeginOfTypingPair), "this (‸is (a test)).");
	OAK_ASSERT_EQ(move("this (is (a test‸)).", kSelectionMoveToBeginOfTypingPair), "this (is (‸a test)).");
	OAK_ASSERT_EQ(move("this (is (a test)‸).", kSelectionMoveToBeginOfTypingPair), "this (‸is (a test)).");
	OAK_ASSERT_EQ(move("this (is (a test))‸.", kSelectionMoveToBeginOfTypingPair), "this (is (a test))‸.");
	OAK_ASSERT_EQ(move("this (is (a test)).‸", kSelectionMoveToBeginOfTypingPair), "this (is (a test)).‸");

	OAK_ASSERT_EQ(move("‸this (is (a test)).", kSelectionMoveToEndOfTypingPair), "‸this (is (a test)).");
	OAK_ASSERT_EQ(move("this ‸(is (a test)).", kSelectionMoveToEndOfTypingPair), "this ‸(is (a test)).");
	OAK_ASSERT_EQ(move("this (‸is (a test)).", kSelectionMoveToEndOfTypingPair), "this (is (a test)‸).");
	OAK_ASSERT_EQ(move("this (is ‸(a test)).", kSelectionMoveToEndOfTypingPair), "this (is (a test)‸).");
	OAK_ASSERT_EQ(move("this (is (‸a test)).", kSelectionMoveToEndOfTypingPair), "this (is (a test‸)).");
	OAK_ASSERT_EQ(move("this (is (a test‸)).", kSelectionMoveToEndOfTypingPair), "this (is (a test)‸).");
	OAK_ASSERT_EQ(move("this (is (a test)‸).", kSelectionMoveToEndOfTypingPair), "this (is (a test)‸).");
	OAK_ASSERT_EQ(move("this (is (a test))‸.", kSelectionMoveToEndOfTypingPair), "this (is (a test))‸.");
	OAK_ASSERT_EQ(move("this (is (a test)).‸", kSelectionMoveToEndOfTypingPair), "this (is (a test)).‸");
}

void test_bad_nesting ()
{
	OAK_ASSERT_EQ(move("(bad‸ly} nested)", kSelectionMoveToBeginOfTypingPair), "(‸badly} nested)");
	OAK_ASSERT_EQ(move("(bad‸ly} nested)", kSelectionMoveToEndOfTypingPair),   "(badly} nested‸)");
	OAK_ASSERT_EQ(move("{bad‸ly) nested}", kSelectionMoveToBeginOfTypingPair), "{‸badly) nested}");
	OAK_ASSERT_EQ(move("{bad‸ly) nested}", kSelectionMoveToEndOfTypingPair),   "{badly) nested‸}");

	OAK_ASSERT_EQ(move("{(bad‸ly} nested)", kSelectionMoveToBeginOfTypingPair), "{‸(badly} nested)");
	OAK_ASSERT_EQ(move("{(bad‸ly} nested)", kSelectionMoveToEndOfTypingPair),   "{(badly‸} nested)");
	OAK_ASSERT_EQ(move("({bad‸ly) nested}", kSelectionMoveToBeginOfTypingPair), "(‸{badly) nested}");
	OAK_ASSERT_EQ(move("({bad‸ly) nested}", kSelectionMoveToEndOfTypingPair),   "({badly‸) nested}");
}

void test_tag_movement ()
{
	OAK_ASSERT_EQ(move("<html></html‸>", kSelectionMoveToBeginOfTypingPair), "<html></html‸>");
	OAK_ASSERT_EQ(move("<html></html>‸", kSelectionMoveToBeginOfTypingPair), "<html></html>‸");
	OAK_ASSERT_EQ(move("<html><body></body‸></html>", kSelectionMoveToBeginOfTypingPair), "<html><body>‸</body></html>");
	OAK_ASSERT_EQ(move("<html><body>‸</body></html>", kSelectionMoveToBeginOfTypingPair), "<html>‸<body></body></html>");

	OAK_ASSERT_EQ(move("<‸html></html>", kSelectionMoveToEndOfTypingPair), "<‸html></html>");
	OAK_ASSERT_EQ(move("‸<html></html>", kSelectionMoveToEndOfTypingPair), "‸<html></html>");
	OAK_ASSERT_EQ(move("<html><‸body></body></html>", kSelectionMoveToEndOfTypingPair), "<html><body>‸</body></html>");
	OAK_ASSERT_EQ(move("<html><body>‸</body></html>", kSelectionMoveToEndOfTypingPair), "<html><body></body>‸</html>");

	OAK_ASSERT_EQ(move("<ul><li>first‸</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul><li>‸first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>‸  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul>‸<li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li> ‸ <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul>‸<li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li‸> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul>‸<li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li>‸ <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul>‸<li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li> <a> <b>hello</b>‸ <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul><li>first</li>  <li> <a>‸ <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li> <a> <b>hello</b> ‸<em>world</em></a></li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul><li>first</li>  <li> <a>‸ <b>hello</b> <em>world</em></a></li><li>third</li></ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li> <a> <b>hello</b> <em>world</em></a>‸</li><li>third</li></ul>", kSelectionMoveToBeginOfTypingPair), "<ul><li>first</li>  <li>‸ <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>");

	OAK_ASSERT_EQ(move("<ul><li>first‸</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToEndOfTypingPair), "<ul><li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li>‸</ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li> ‸ <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li></ul>", kSelectionMoveToEndOfTypingPair), "<ul><li>first</li>  <li> <a> <b>hello</b> <em>world</em></a></li><li>third</li>‸</ul>");
	OAK_ASSERT_EQ(move("<ul><li>first</li>  <li> <a> <b>hello</b> ‸<em>world</em></a></li><li>third</li></ul>", kSelectionMoveToEndOfTypingPair), "<ul><li>first</li>  <li> <a> <b>hello</b> <em>world</em>‸</a></li><li>third</li></ul>");
}

void test_brace_selection ()
{
	ng::buffer_t buf;
	buf.insert(0, "this (is (a test)).");

	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t( 0), kSelectionExtendToTypingPair)),         "1");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t( 5), kSelectionExtendToTypingPair)),       "1:6");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t( 6), kSelectionExtendToTypingPair)),  "1:6-1:19");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t( 9), kSelectionExtendToTypingPair)),  "1:6-1:19");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t(10), kSelectionExtendToTypingPair)), "1:10-1:18");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t(16), kSelectionExtendToTypingPair)), "1:10-1:18");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t(17), kSelectionExtendToTypingPair)),  "1:6-1:19");
	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t(18), kSelectionExtendToTypingPair)),      "1:19");
}

void test_brace_selection_full_line ()
{
	ng::buffer_t buf;
	buf.insert(0, "\t{\nfoo\n\t}\n");

	OAK_ASSERT_EQ(to_s(buf, ng::extend(buf, ng::ranges_t( 2), kSelectionExtendToTypingPair)), "1-4");
}

void test_highlight_pairs ()
{
	ng::buffer_t buf;
	buf.insert(0, "()[)");

	ng::ranges_t caret = ng::range_t(1);
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveRight))), "[0-1]");
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveLeft))),  "[1-2]");

	caret = ng::range_t(3);
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveRight))), "(empty)");
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveLeft))),  "(empty)");
}

void test_tag_highlight_pairs ()
{
	ng::buffer_t buf;
	buf.insert(0, "<a></a>");

	ng::ranges_t caret = ng::range_t(1);
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveLeft))),  "[3-7]");

	caret = ng::range_t(6);
	OAK_ASSERT_EQ(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveRight))), "[0-3]");
}
