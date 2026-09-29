#include <ns/src/event.h>

void test_normalize_event_string ()
{
	OAK_ASSERT_EQ(ns::normalize_event_string("@"),    "");
	OAK_ASSERT_EQ(ns::normalize_event_string("#"),    "");
	OAK_ASSERT_EQ(ns::normalize_event_string("$"),    "");
	OAK_ASSERT_EQ(ns::normalize_event_string("^"),    "");
	OAK_ASSERT_EQ(ns::normalize_event_string("~"),    "");
	OAK_ASSERT_EQ(ns::normalize_event_string("@~"),   "");
	OAK_ASSERT_EQ(ns::normalize_event_string("!"),    "!");

	OAK_ASSERT_EQ(ns::normalize_event_string("\\@"),    "@");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\#"),    "#");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\$"),    "$");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\^"),    "^");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\~"),    "~");
	OAK_ASSERT_EQ(ns::normalize_event_string("@\\~"),   "@~");
	OAK_ASSERT_EQ(ns::normalize_event_string("~\\@"),   "~@");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\!"),    "!");
	OAK_ASSERT_EQ(ns::normalize_event_string("\\\\"),   "\\");

	OAK_ASSERT_EQ(ns::normalize_event_string("^$@~#1"), "#^~$@1");
}

void test_glyphs_for_event_string ()
{
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("@"),    "@");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("#"),    "#");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("$"),    "$");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("^"),    "^");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("~"),    "~");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("@~"),   "⌘~");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("~@"),   "⌥@");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("!"),    "!");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("\\"),   "\\");

	OAK_ASSERT_EQ(ns::glyphs_for_event_string("@1"),    "⌘1");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("#1"),    "1⃣");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("$1"),    "⇧1");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("^1"),    "⌃1");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("~1"),    "⌥1");
	OAK_ASSERT_EQ(ns::glyphs_for_event_string("A"),     "⇧A");
}
