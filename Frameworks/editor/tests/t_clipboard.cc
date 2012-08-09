#include <editor/clipboard.h>

class ClipboardTests : public CxxTest::TestSuite
{
public:
	void setUp ()
	{
		cb = create_simple_clipboard();
	}

	void test_empty ()
	{
		TS_ASSERT(cb->empty());
		TS_ASSERT(!cb->previous());
		TS_ASSERT(!cb->current());
		TS_ASSERT(!cb->next());
	}

	void test_non_empty ()
	{
		cb->push_back("foo");
		cb->push_back("bar");
		cb->push_back("fud");

		TS_ASSERT_EQUALS(cb->current()->content(),  "fud");
		TS_ASSERT_EQUALS(cb->previous()->content(), "bar");
		TS_ASSERT_EQUALS(cb->previous()->content(), "foo");
		TS_ASSERT(!cb->previous());
		TS_ASSERT_EQUALS(cb->next()->content(),     "bar");
		TS_ASSERT_EQUALS(cb->next()->content(),     "fud");
		TS_ASSERT(!cb->next());
	}

	clipboard_ptr cb;
};
