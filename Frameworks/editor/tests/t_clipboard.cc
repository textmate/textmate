#include <editor/clipboard.h>

void test_empty ()
{
	clipboard_ptr cb = create_simple_clipboard();

	OAK_ASSERT(cb->empty());
	OAK_ASSERT(!cb->previous());
	OAK_ASSERT(!cb->current());
	OAK_ASSERT(!cb->next());
}

void test_non_empty ()
{
	clipboard_ptr cb = create_simple_clipboard();

	cb->push_back("foo");
	cb->push_back("bar");
	cb->push_back("fud");

	OAK_ASSERT_EQ(cb->current()->content(),  "fud");
	OAK_ASSERT_EQ(cb->previous()->content(), "bar");
	OAK_ASSERT_EQ(cb->previous()->content(), "foo");
	OAK_ASSERT(!cb->previous());
	OAK_ASSERT_EQ(cb->next()->content(),     "bar");
	OAK_ASSERT_EQ(cb->next()->content(),     "fud");
	OAK_ASSERT(!cb->next());
}
