#include <scope/src/scope.h>

void test_shared_prefix ()
{
	OAK_ASSERT_EQ("",        to_s(scope::shared_prefix("foo",          "bar"            )));
	OAK_ASSERT_EQ("foo",     to_s(scope::shared_prefix("foo bar",      "foo"            )));
	OAK_ASSERT_EQ("foo",     to_s(scope::shared_prefix("foo",          "foo bar"        )));
	OAK_ASSERT_EQ("foo bar", to_s(scope::shared_prefix("foo bar quux", "foo bar baz qux")));
}

void test_xml_difference ()
{
	scope::scope_t empty;
	scope::scope_t first("foo bar");
	scope::scope_t second("foo");
	scope::scope_t third("baz qux");

	OAK_ASSERT_EQ("<foo><bar>",       xml_difference(empty, first));
	OAK_ASSERT_EQ("</bar>",           xml_difference(first, second));
	OAK_ASSERT_EQ("</foo><baz><qux>", xml_difference(second, third));
	OAK_ASSERT_EQ("</qux></baz>",     xml_difference(third, empty));
}
