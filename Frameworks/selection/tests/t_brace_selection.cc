#include <buffer/buffer.h>
#include <selection/selection.h>

class BraceSelectionTests : public CxxTest::TestSuite
{
public:
	void test_brace_movement ()
	{
		ng::buffer_t buf;
		buf.insert(0, "this (is (a test)).");

		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 0), kSelectionMoveToBeginOfTypingPair)),         "1");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 5), kSelectionMoveToBeginOfTypingPair)),       "1:6");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 6), kSelectionMoveToBeginOfTypingPair)),       "1:7");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 9), kSelectionMoveToBeginOfTypingPair)),       "1:7");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(10), kSelectionMoveToBeginOfTypingPair)),       "1:7");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(16), kSelectionMoveToBeginOfTypingPair)),      "1:11");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(17), kSelectionMoveToBeginOfTypingPair)),      "1:10");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(18), kSelectionMoveToBeginOfTypingPair)),       "1:6");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(19), kSelectionMoveToBeginOfTypingPair)),      "1:20");

		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 0), kSelectionMoveToEndOfTypingPair)),           "1");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 5), kSelectionMoveToEndOfTypingPair)),        "1:19");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 6), kSelectionMoveToEndOfTypingPair)),        "1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t( 9), kSelectionMoveToEndOfTypingPair)),        "1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(10), kSelectionMoveToEndOfTypingPair)),        "1:17");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(16), kSelectionMoveToEndOfTypingPair)),        "1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(17), kSelectionMoveToEndOfTypingPair)),        "1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::move(buf, ng::range_t(18), kSelectionMoveToEndOfTypingPair)),        "1:19");
	}

	void test_brace_selection ()
	{
		ng::buffer_t buf;
		buf.insert(0, "this (is (a test)).");

		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t( 0), kSelectionExtendToTypingPair)),         "1");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t( 5), kSelectionExtendToTypingPair)),       "1:6");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t( 6), kSelectionExtendToTypingPair)),  "1:6-1:19");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t( 9), kSelectionExtendToTypingPair)),  "1:6-1:19");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t(10), kSelectionExtendToTypingPair)), "1:10-1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t(16), kSelectionExtendToTypingPair)), "1:10-1:18");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t(17), kSelectionExtendToTypingPair)),  "1:6-1:19");
		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t(18), kSelectionExtendToTypingPair)),      "1:19");
	}

	void test_brace_selection_full_line ()
	{
		ng::buffer_t buf;
		buf.insert(0, "\t{\nfoo\n\t}\n");

		TS_ASSERT_EQUALS(to_s(buf, ng::extend(buf, ng::ranges_t( 2), kSelectionExtendToTypingPair)), "1-4");
	}

	void test_highlight_pairs ()
	{
		ng::buffer_t buf;
		buf.insert(0, "()[)");

		ng::ranges_t caret = ng::range_t(1);
		TS_ASSERT_EQUALS(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveRight))), "[0-1]");
		TS_ASSERT_EQUALS(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveLeft))),  "[1-2]");

		caret = ng::range_t(3);
		TS_ASSERT_EQUALS(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveRight))), "(empty)");
		TS_ASSERT_EQUALS(to_s(ng::highlight_ranges_for_movement(buf, caret, ng::move(buf, caret, kSelectionMoveLeft))),  "(empty)");
	}
};
