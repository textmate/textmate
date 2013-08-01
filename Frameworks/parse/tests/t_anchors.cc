#include "support.h"
#include <test/bundle_index.h>

static bundles::item_ptr AnchorTestGrammarItem;
static bundles::item_ptr AnchorInCapturesTestGrammarItem;

__attribute__((constructor)) static void setup_fixtures ()
{
	static std::string AnchorTestLanguageGrammar =
		"{ name           = 'Test';"
		"  patterns       = ("
		"    { name = 'bof'; match = '\\Axy'; },"
		"    { name = 'bom'; match = '\\Gxy'; },"
		"    { name = 'eof'; match = 'xy\\z'; },"
		"    { begin = '\\['; end = '\\]';"
		"      patterns = ("
		"        { name = 'bom'; match = '\\Axy'; },"
		"        { name = 'bom'; match = '\\Gxy'; },"
		"        { name = 'bom'; match = 'xy\\z'; },"
		"      );"
		"    },"
		"  );"
		"  scopeName      = 'test';"
		"  uuid           = 'F159C30D-9EE7-4A30-AE30-E26EF3BCCC17';"
		"}";

	static std::string AnchorInCapturesTestLanguageGrammar =
		"{ name     = 'Test';"
		"  patterns = ("
		"    { match = '> (.+)';"
		"      name = 'gt';"
		"      captures = { 1 = { patterns = ( { include = '#captures'; } ); }; };"
		"    },"
		"    { match = '(.+) <';"
		"      name = 'lt';"
		"      captures = { 1 = { patterns = ( { include = '#captures'; } ); }; };"
		"    },"
		"    { match = '.+\\z';"
		"      name = 'tail';"
		"      captures = { 0 = { patterns = ( { include = '#captures'; } ); }; };"
		"    },"
		"    { match = '\\A.+';"
		"      name = 'head';"
		"      captures = { 0 = { patterns = ( { include = '#captures'; } ); }; };"
		"    },"
		"    { match = '.+';"
		"      name = 'line';"
		"      captures = { 0 = { patterns = ( { include = '#captures'; } ); }; };"
		"    },"
		"  );"
		"  repository = {"
		"    captures = {"
		"      patterns = ("
		"        { match = '\\A\\w+';"
		"          name = 'b-buf';"
		"        },"
		"        { match = '^\\w+';"
		"          name = 'b-line';"
		"        },"
		"        { match = '\\G\\w+';"
		"          name = 'b-cap';"
		"        },"
		"        { match = '\\w+\\z';"
		"          name = 'e-buf';"
		"        },"
		"        { match = '\\w+$';"
		"          name = 'e-line';"
		"        },"
		"        { match = '\\w+\\Z';"
		"          name = 'e-cap';"
		"        },"
		"      );"
		"    };"
		"  };"
		"  scopeName      = 'test';"
		"  uuid           = '5C80DB53-F519-494F-BA8D-C8D80540E9E4';"
		"}";

	test::bundle_index_t bundleIndex;
	AnchorTestGrammarItem           = bundleIndex.add(bundles::kItemTypeGrammar, AnchorTestLanguageGrammar);
	AnchorInCapturesTestGrammarItem = bundleIndex.add(bundles::kItemTypeGrammar, AnchorInCapturesTestLanguageGrammar);
}

void test_anchors ()
{
	auto grammar = parse::parse_grammar(AnchorTestGrammarItem);

	OAK_ASSERT_EQ(markup(grammar, "xy xy\nxy xy\n[xy xy\nxy xy]\nxy xy"), "«test»«bof»xy«/bof» xy\nxy xy\n[«bom»xy«/bom» xy\nxy xy]\nxy «eof»xy«/eof»«/test»");
	OAK_ASSERT_EQ(markup(grammar, "xy xy"),                               "«test»«bof»xy«/bof» «eof»xy«/eof»«/test»");
	OAK_ASSERT_EQ(markup(grammar, "xy xy\n"),                             "«test»«bof»xy«/bof» xy\n«/test»");
	OAK_ASSERT_EQ(markup(grammar, "[xy xy]"),                             "«test»[«bom»xy«/bom» xy]«/test»");
}

void test_anchor_in_captures ()
{
	auto grammar = parse::parse_grammar(AnchorInCapturesTestGrammarItem);
	OAK_ASSERT_EQ(markup(grammar, "foo\n"),        "«test»«head»«b-buf»foo«/b-buf»«/head»\n«/test»");
	OAK_ASSERT_EQ(markup(grammar, "> foo\n"),      "«test»«gt»> «b-cap»foo«/b-cap»«/gt»\n«/test»");
	OAK_ASSERT_EQ(markup(grammar, "foo <\n"),      "«test»«lt»«b-buf»foo«/b-buf» <«/lt»\n«/test»");
	OAK_ASSERT_EQ(markup(grammar, "\nfoo\n"),      "«test»\n«line»«b-line»foo«/b-line»«/line»\n«/test»");
	OAK_ASSERT_EQ(markup(grammar, "\nfoo"),        "«test»\n«tail»«b-line»foo«/b-line»«/tail»«/test»");
	OAK_ASSERT_EQ(markup(grammar, "\nfoo bar"),    "«test»\n«tail»«b-line»foo«/b-line» «e-buf»bar«/e-buf»«/tail»«/test»");

	// OAK_ASSERT_EQ(markup(grammar, "foo bar\n"),    "«test»«head»«b-buf»foo«/b-buf» «e-line»bar«/e-line»«/head»\n«/test»");
	// OAK_ASSERT_EQ(markup(grammar, "> foo bar\n"),  "«test»«gt»> «b-cap»foo«/b-cap» «e-line»bar«/e-line»«/gt»\n«/test»");
	// OAK_ASSERT_EQ(markup(grammar, "foo bar <\n"),  "«test»«lt»«b-buf»foo«/b-buf» «e-cap»bar«/e-cap» <«/lt»\n«/test»");
	// OAK_ASSERT_EQ(markup(grammar, "\nfoo bar\n"),  "«test»\n«line»«b-line»foo«/b-line» «e-line»bar«/e-line»«/line»\n«/test»");
}
