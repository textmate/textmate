#include <scope/scope.h>

class ScopeSelectorTests : public CxxTest::TestSuite
{
public:
	void test_child_selector ()
	{
		TS_ASSERT_EQUALS(scope::selector_t("foo fud").does_match("foo bar fud"),   true);
		TS_ASSERT_EQUALS(scope::selector_t("foo > fud").does_match("foo bar fud"), false);
		TS_ASSERT_EQUALS(scope::selector_t("foo > foo > fud").does_match("foo foo fud"), true);
		TS_ASSERT_EQUALS(scope::selector_t("foo > foo > fud").does_match("foo foo fud fud"), true);
		TS_ASSERT_EQUALS(scope::selector_t("foo > foo > fud").does_match("foo foo fud baz"), true);

		TS_ASSERT_EQUALS(scope::selector_t("foo > foo fud > fud").does_match("foo foo bar fud fud"), true);
	}

	void test_mixed ()
	{
		TS_ASSERT_EQUALS(scope::selector_t("^ foo > bar").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("foo > bar $").does_match("foo bar foo"), false);
		TS_ASSERT_EQUALS(scope::selector_t("bar > foo $").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("foo > bar > foo $").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("^ foo > bar > foo $").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("bar > foo $").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("^ foo > bar > baz").does_match("foo bar baz foo bar baz"), true);
		TS_ASSERT_EQUALS(scope::selector_t("^ foo > bar > baz").does_match("foo foo bar baz foo bar baz"), false);
				
	}

	void test_anchor ()
	{
		TS_ASSERT_EQUALS(scope::selector_t("^ foo").does_match("foo bar"), true);
		TS_ASSERT_EQUALS(scope::selector_t("^ bar").does_match("foo bar"), false);
		TS_ASSERT_EQUALS(scope::selector_t("^ foo").does_match("foo bar foo"), true);
		TS_ASSERT_EQUALS(scope::selector_t("foo $").does_match("foo bar"), false);
		TS_ASSERT_EQUALS(scope::selector_t("bar $").does_match("foo bar"), true);
	}

	void test_scope_selector ()
	{
		static scope::scope_t const textScope = "text.html.markdown meta.paragraph.markdown markup.bold.markdown";
		static scope::selector_t const matchingSelectors[] =
		{
			"text.* markup.bold",
			"text markup.bold",
			"markup.bold",
			"text.html meta.*.markdown markup",
			"text.html meta.* markup",
			"text.html * markup",
			"text.html markup",
			"text markup",
			"markup",
			"text.html",
			"text"
		};

		double lastRank = 1;
		for(size_t i = 0; i < sizeofA(matchingSelectors); ++i)
		{
			double rank;
			TS_ASSERT(matchingSelectors[i].does_match(textScope, &rank));
			TS_ASSERT_LESS_THAN(rank, lastRank);
			lastRank = rank;
		}
	}

	void test_rank ()
	{
		scope::scope_t const leftScope  = "text.html.php meta.embedded.block.php source.php comment.block.php";
		scope::scope_t const rightScope = "text.html.php meta.embedded.block.php source.php";
		scope::context_t const scope(leftScope, rightScope);

		scope::selector_t const globalSelector = "comment.block | L:comment.block";
		scope::selector_t const phpSelector    = "L:source.php - string";

		double globalRank, phpRank;
		TS_ASSERT(globalSelector.does_match(scope, &globalRank));
		TS_ASSERT(phpSelector.does_match(scope, &phpRank));
		TS_ASSERT_LESS_THAN(phpRank, globalRank);
	}
};
