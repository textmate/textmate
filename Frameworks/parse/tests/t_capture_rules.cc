#include "support.cc"
#include <test/bundle_index.h>

static bundles::item_ptr CaptureTestGrammarItem;

static class CaptureGrammarFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld()
	{
		static std::string CaptureTestLanguageGrammar =
			"{ name           = 'Test';"
			"  patterns       = ("
			"    { match = '^((?:.{0,20}\\s*)|(.{21,}\\s*))$';"
			"      captures = {"
			"        1 = {"
			"          patterns = ("
			"            { match = '\\G(fixup|squash)!';"
			"              name = '$1';"
			"            },"
			"          );"
			"        };"
			"        2 = { name = 'warn'; };"
			"      };"
			"    },"
			"  );"
			"  scopeName      = 'test';"
			"  uuid           = 'FB562A16-A2AA-49E0-AAF6-6D030ECC8DAC';"
			"}";

		test::bundle_index_t bundleIndex;
		CaptureTestGrammarItem = bundleIndex.add(bundles::kItemTypeGrammar, CaptureTestLanguageGrammar);
		return bundleIndex.commit();
	}

} capture_grammar_fixture;

class CapturesTests : public CxxTest::TestSuite
{
public:
	void test_captures ()
	{
		auto grammar = parse::parse_grammar(CaptureTestGrammarItem);
		TS_ASSERT_EQUALS(markup(grammar, "Lorem ipsum."),                       "«test»Lorem ipsum.«/test»");
		TS_ASSERT_EQUALS(markup(grammar, "fixup! Lorem ipsum."),                "«test»«fixup»fixup!«/fixup» Lorem ipsum.«/test»");
		TS_ASSERT_EQUALS(markup(grammar, "Lorem ipsum dolor sit amet."),        "«test»«warn»Lorem ipsum dolor sit amet.«/warn»«/test»");
		TS_WARN("TODO: Applying scope names to “nested” captures (where parser ran on the previous pass).");
		// TS_ASSERT_EQUALS(markup(grammar, "fixup! Lorem ipsum dolor sit amet."), "«test»«warn»«fixup»fixup!«/fixup» Lorem ipsum dolor sit amet.«/warn»«/test»");
	}
};
