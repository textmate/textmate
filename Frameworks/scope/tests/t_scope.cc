#include <scope/scope.h>

void test_scope_append ()
{
	scope::scope_t scope("foo bar");
	OAK_ASSERT_EQ("bar", scope.back());
	scope.push_scope("some invalid..scope");
	OAK_ASSERT_EQ("some invalid..scope", scope.back());
	scope.pop_scope();
	OAK_ASSERT_EQ("foo bar", to_s(scope));
	scope.pop_scope();
	OAK_ASSERT_EQ("foo", to_s(scope));
}

void test_operator_bool ()
{
	scope::scope_t scope("foo"), empty;
	OAK_ASSERT(scope);
	OAK_ASSERT(!empty);
	scope.pop_scope();
	OAK_ASSERT(!scope);
}
