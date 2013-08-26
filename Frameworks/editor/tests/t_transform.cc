#include "../src/transform.h"

void test_transform ()
{
	OAK_ASSERT_EQ("dørgelbÆ",        transform::transpose("Æblegrød"));
	OAK_ASSERT_EQ("dørgelbÆ\n",      transform::transpose("Æblegrød\n"));
	OAK_ASSERT_EQ("bar, foo",        transform::transpose("foo, bar"));
	OAK_ASSERT_EQ("bar, (foo)",      transform::transpose("(foo), bar"));
	OAK_ASSERT_EQ("(bar), foo",      transform::transpose("foo, (bar)"));
	OAK_ASSERT_EQ("(bar, foo)",      transform::transpose("(foo, bar)"));
	OAK_ASSERT_EQ("bar + foo",       transform::transpose("foo + bar"));
	OAK_ASSERT_EQ("'bar', 'foo'",    transform::transpose("'foo', 'bar'"));
	OAK_ASSERT_EQ("bar() : foo()",   transform::transpose("foo() : bar()"));
	OAK_ASSERT_EQ("('bar', 'foo')",  transform::transpose("('foo', 'bar')"));
	OAK_ASSERT_EQ("bar < foo",       transform::transpose("foo < bar"));
	OAK_ASSERT_EQ("bar <= foo",      transform::transpose("foo <= bar"));
	OAK_ASSERT_EQ("bar == foo",      transform::transpose("foo == bar"));
	OAK_ASSERT_EQ("bar != foo",      transform::transpose("foo != bar"));
	OAK_ASSERT_EQ("bar > foo",       transform::transpose("foo > bar"));
	OAK_ASSERT_EQ("bar >= foo",      transform::transpose("foo >= bar"));
}
