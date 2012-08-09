#include <test/bundle_index.h>

static class GrammarFixture : public CxxTest::GlobalFixture
{
public:
	bool setUpWorld()
	{
		static std::string TextLanguageGrammar =
			"{	fileTypes      = ( txt );\n"
			"	keyEquivalent  = '^~P';\n"
			"	name           = 'Plain Text';\n"
			"	patterns       = ( );\n"
			"	scopeName      = 'text.plain';\n"
			"	uuid           = '3130E4FA-B10E-11D9-9F75-000D93589AF6';\n"
			"}\n";

		static std::string CLanguageGrammar =
			"{	fileTypes      = ( c, h );\n"
			"	keyEquivalent  = '^~C';\n"
			"	name           = 'C';\n"
			"	firstLineMatch = '-[*]-( Mode:)? C -[*]-';\n"
			"	patterns       = ( );\n"
			"	scopeName      = 'source.c';\n"
			"	uuid           = '25066DC2-6B1D-11D9-9D5B-000D93589AF6';\n"
			"}\n";

		test::bundle_index_t bundleIndex;
		bundleIndex.add(bundles::kItemTypeGrammar, TextLanguageGrammar);
		bundleIndex.add(bundles::kItemTypeGrammar, CLanguageGrammar);
		return bundleIndex.commit();
	}

} grammar_fixture;

class GrammarFixturesTests : public CxxTest::TestSuite
{
public:
	void test_file_type ()
	{
		std::string path = "/tmp/utf32-be.txt";
		std::vector<bundles::item_ptr> items = bundles::grammars_for_path(path);
		TS_ASSERT_EQUALS(items.size(), 1);
		TS_ASSERT_EQUALS(items[0]->name(), "Plain Text");
	}

	void test_scope_query ()
	{
		std::vector<bundles::item_ptr> items = bundles::query(bundles::kFieldGrammarScope, "text.plain");
		TS_ASSERT_EQUALS(items.size(), 1);
		TS_ASSERT_EQUALS(items[0]->name(), "Plain Text");
	}
};
