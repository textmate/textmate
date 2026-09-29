#include <scope/src/scope.h>

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

void test_empty_scope ()
{
	OAK_ASSERT(scope::scope_t().empty());
	OAK_ASSERT(scope::scope_t("").empty());
	OAK_ASSERT_EQ(scope::scope_t(""), scope::scope_t());
}

void test_has_prefix ()
{
	OAK_ASSERT( scope::scope_t("").has_prefix(""));
	OAK_ASSERT(!scope::scope_t("").has_prefix("foo"));
	OAK_ASSERT( scope::scope_t("foo").has_prefix(""));
	OAK_ASSERT(!scope::scope_t("foo").has_prefix("foo bar"));
	OAK_ASSERT( scope::scope_t("foo bar").has_prefix("foo bar"));
	OAK_ASSERT( scope::scope_t("foo bar baz").has_prefix("foo bar"));
}

void test_operator_bool ()
{
	scope::scope_t scope("foo"), empty;
	OAK_ASSERT(scope);
	OAK_ASSERT(!empty);
	scope.pop_scope();
	OAK_ASSERT(!scope);
}
