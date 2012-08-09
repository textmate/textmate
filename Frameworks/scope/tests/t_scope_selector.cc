#include <scope/scope.h>

class ScopeSelectorTests : public CxxTest::TestSuite
{
public:
	void test_child_selector ()
	{
		TS_WARN("TODO: Child and anchor selectors");
		TS_ASSERT_EQUALS(scope::selector_t("foo fud").does_match("foo bar fud"),   true);
		// TS_ASSERT_EQUALS(scope::selector_t("foo > fud").does_match("foo bar fud"), false);
	}

	void test_anchor ()
	{
		TS_ASSERT_EQUALS(scope::selector_t("^ foo").does_match("foo bar"), true);
		// TS_ASSERT_EQUALS(scope::selector_t("^ bar").does_match("foo bar"), false);
		// TS_ASSERT_EQUALS(scope::selector_t("foo $").does_match("foo bar"), false);
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
};
