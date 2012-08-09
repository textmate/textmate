#include <ns/event.h>

class EventStringTests : public CxxTest::TestSuite
{
public:
	void test_normalize_event_string ()
	{
		TS_ASSERT_EQUALS(ns::normalize_event_string("@"),    "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("#"),    "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("$"),    "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("^"),    "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("~"),    "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("@~"),   "");
		TS_ASSERT_EQUALS(ns::normalize_event_string("!"),    "!");

		TS_ASSERT_EQUALS(ns::normalize_event_string("\\@"),    "@");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\#"),    "#");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\$"),    "$");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\^"),    "^");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\~"),    "~");
		TS_ASSERT_EQUALS(ns::normalize_event_string("@\\~"),   "@~");
		TS_ASSERT_EQUALS(ns::normalize_event_string("~\\@"),   "~@");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\!"),    "!");
		TS_ASSERT_EQUALS(ns::normalize_event_string("\\\\"),   "\\");

		TS_ASSERT_EQUALS(ns::normalize_event_string("^$@~#1"), "#^~$@1");
	}

	void test_glyphs_for_event_string ()
	{
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("@"),    "@");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("#"),    "#");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("$"),    "$");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("^"),    "^");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("~"),    "~");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("@~"),   "⌘~");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("~@"),   "⌥@");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("!"),    "!");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("\\"),   "\\");

		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("@1"),    "⌘1");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("#1"),    "1⃣");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("$1"),    "⇧1");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("^1"),    "⌃1");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("~1"),    "⌥1");
		TS_ASSERT_EQUALS(ns::glyphs_for_event_string("A"),     "⇧A");
	}
};
