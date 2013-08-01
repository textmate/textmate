#include "support.h"
#include <test/bundle_index.h>

static bundles::item_ptr CaptureTestGrammarItem;

__attribute__((constructor)) static void setup_fixtures ()
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
}

void test_captures ()
{
	auto grammar = parse::parse_grammar(CaptureTestGrammarItem);
	OAK_ASSERT_EQ(markup(grammar, "Lorem ipsum."),                       "«test»Lorem ipsum.«/test»");
	OAK_ASSERT_EQ(markup(grammar, "fixup! Lorem ipsum."),                "«test»«fixup»fixup!«/fixup» Lorem ipsum.«/test»");
	OAK_ASSERT_EQ(markup(grammar, "Lorem ipsum dolor sit amet."),        "«test»«warn»Lorem ipsum dolor sit amet.«/warn»«/test»");
	OAK_WARN("TODO: Applying scope names to “nested” captures (where parser ran on the previous pass).");
	// OAK_ASSERT_EQ(markup(grammar, "fixup! Lorem ipsum dolor sit amet."), "«test»«warn»«fixup»fixup!«/fixup» Lorem ipsum dolor sit amet.«/warn»«/test»");
}
