#include <scope/scope.h>

void test_child_selector ()
{
	OAK_ASSERT_EQ(scope::selector_t("foo fud").does_match("foo bar fud"),   true);
	OAK_ASSERT_EQ(scope::selector_t("foo > fud").does_match("foo bar fud"), false);
	OAK_ASSERT_EQ(scope::selector_t("foo > foo > fud").does_match("foo foo fud"), true);
	OAK_ASSERT_EQ(scope::selector_t("foo > foo > fud").does_match("foo foo fud fud"), true);
	OAK_ASSERT_EQ(scope::selector_t("foo > foo > fud").does_match("foo foo fud baz"), true);

	OAK_ASSERT_EQ(scope::selector_t("foo > foo fud > fud").does_match("foo foo bar fud fud"), true);
}

void test_mixed ()
{
	OAK_ASSERT_EQ(scope::selector_t("^ foo > bar").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("foo > bar $").does_match("foo bar foo"), false);
	OAK_ASSERT_EQ(scope::selector_t("bar > foo $").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("foo > bar > foo $").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("^ foo > bar > foo $").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("bar > foo $").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("^ foo > bar > baz").does_match("foo bar baz foo bar baz"), true);
	OAK_ASSERT_EQ(scope::selector_t("^ foo > bar > baz").does_match("foo foo bar baz foo bar baz"), false);
			
}

void test_dollar ()
{
	scope::scope_t dyn("foo bar");
	dyn.push_scope("dyn.selection");
	OAK_ASSERT_EQ(scope::selector_t("foo bar$").does_match(dyn), true);
	OAK_ASSERT_EQ(scope::selector_t("foo bar dyn$").does_match(dyn), false);
	OAK_ASSERT_EQ(scope::selector_t("foo bar dyn").does_match(dyn), true);
}

void test_anchor ()
{
	OAK_ASSERT_EQ(scope::selector_t("^ foo").does_match("foo bar"), true);
	OAK_ASSERT_EQ(scope::selector_t("^ bar").does_match("foo bar"), false);
	OAK_ASSERT_EQ(scope::selector_t("^ foo").does_match("foo bar foo"), true);
	OAK_ASSERT_EQ(scope::selector_t("foo $").does_match("foo bar"), false);
	OAK_ASSERT_EQ(scope::selector_t("bar $").does_match("foo bar"), true);
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
		OAK_ASSERT(matchingSelectors[i].does_match(textScope, &rank));
		OAK_ASSERT_LT(rank, lastRank);
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
	OAK_ASSERT(globalSelector.does_match(scope, &globalRank));
	OAK_ASSERT(phpSelector.does_match(scope, &phpRank));
	OAK_ASSERT_LT(phpRank, globalRank);
}

void test_match ()
{
	auto match = [](scope::selector_t const& sel, scope::scope_t const& scope){ return sel.does_match(scope); };

	OAK_ASSERT( match("foo",                  "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo bar",              "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo bar baz",          "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo baz",              "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo.*",                "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo.qux",              "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("foo.qux baz.*.garply", "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT( match("bar",                  "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT(!match("foo qux",              "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT(!match("foo.bar",              "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT(!match("foo.qux baz.garply",   "foo.qux bar.quux.grault baz.corge.garply"));
	OAK_ASSERT(!match("bar.*.baz",            "foo.qux bar.quux.grault baz.corge.garply"));

	OAK_ASSERT( match("foo > bar",             "foo bar baz bar baz"));
	OAK_ASSERT( match("bar > baz",             "foo bar baz bar baz"));
	OAK_ASSERT( match("foo > bar baz",         "foo bar baz bar baz"));
	OAK_ASSERT( match("foo bar > baz",         "foo bar baz bar baz"));
	OAK_ASSERT( match("foo > bar > baz",       "foo bar baz bar baz"));
	OAK_ASSERT( match("foo > bar bar > baz",   "foo bar baz bar baz"));
	OAK_ASSERT(!match("foo > bar > bar > baz", "foo bar baz bar baz"));

	OAK_ASSERT( match("baz $",                 "foo bar baz bar baz"));
	OAK_ASSERT( match("bar > baz $",           "foo bar baz bar baz"));
	OAK_ASSERT( match("bar > baz $",           "foo bar baz bar baz"));
	OAK_ASSERT( match("foo bar > baz $",       "foo bar baz bar baz"));
	OAK_ASSERT( match("foo > bar > baz",       "foo bar baz bar baz"));
	OAK_ASSERT(!match("foo > bar > baz $",     "foo bar baz bar baz"));
	OAK_ASSERT(!match("bar $",                 "foo bar baz bar baz"));

	OAK_ASSERT( match("baz $",                 "foo bar baz bar baz dyn.qux"));
	OAK_ASSERT( match("bar > baz $",           "foo bar baz bar baz dyn.qux"));
	OAK_ASSERT( match("bar > baz $",           "foo bar baz bar baz dyn.qux"));
	OAK_ASSERT( match("foo bar > baz $",       "foo bar baz bar baz dyn.qux"));
	OAK_ASSERT(!match("foo > bar > baz $",     "foo bar baz bar baz dyn.qux"));
	OAK_ASSERT(!match("bar $",                 "foo bar baz bar baz dyn.qux"));

	OAK_ASSERT( match("^ foo",                 "foo bar foo bar baz"));
	OAK_ASSERT( match("^ foo > bar",           "foo bar foo bar baz"));
	OAK_ASSERT( match("^ foo bar > baz",       "foo bar foo bar baz"));
	OAK_ASSERT( match("^ foo > bar baz",       "foo bar foo bar baz"));
	OAK_ASSERT(!match("^ foo > bar > baz",     "foo bar foo bar baz"));
	OAK_ASSERT(!match("^ bar",                 "foo bar foo bar baz"));

	OAK_ASSERT( match("foo > bar > baz",       "foo bar baz foo bar baz"));
	OAK_ASSERT( match("^ foo > bar > baz",     "foo bar baz foo bar baz"));
	OAK_ASSERT( match("foo > bar > baz $",     "foo bar baz foo bar baz"));
	OAK_ASSERT(!match("^ foo > bar > baz $",   "foo bar baz foo bar baz"));
}
