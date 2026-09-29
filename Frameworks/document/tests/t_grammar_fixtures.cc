#include <test/bundle_index.h>
#include <file/src/type.h>

void setup_fixtures ()
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
	bundleIndex.commit();
}

void test_file_type ()
{
	std::string path = "/tmp/utf32-be.txt";
	OAK_ASSERT_EQ(file::type_from_path(path), "text.plain");
}

void test_scope_query ()
{
	std::vector<bundles::item_ptr> items = bundles::query(bundles::kFieldGrammarScope, "text.plain");
	OAK_ASSERT_EQ(items.size(), 1);
	OAK_ASSERT_EQ(items[0]->name(), "Plain Text");
}
