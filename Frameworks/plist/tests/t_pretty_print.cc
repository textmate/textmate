#include <plist/src/plist.h>

static std::string regular (std::string const& plistSrc)  { return to_s(plist::parse(plistSrc)); }
static std::string enhanced (std::string const& plistSrc) { return to_s(plist::parse(plistSrc), plist::kPreferSingleQuotedStrings); }

void test_regular ()
{
	OAK_ASSERT_EQ(regular("{ foo = \"bar\\\\n\"; }"), "{ foo = \"bar\\\\n\"; }");
	OAK_ASSERT_EQ(regular("{ \"æble\" = bar; }"), "{ \"æble\" = \"bar\"; }");
	OAK_ASSERT_EQ(regular("{ f-o_o. = bar; }"), "{ f-o_o. = \"bar\"; }");
	OAK_ASSERT_EQ(regular("{ _foo = bar; }"), "{ _foo = \"bar\"; }");
	OAK_ASSERT_EQ(regular("{ \".foo\" = bar; }"), "{ \".foo\" = \"bar\"; }");
	OAK_ASSERT_EQ(regular("{ \"-foo\" = bar; }"), "{ \"-foo\" = \"bar\"; }");
}

void test_array_separator ()
{
	OAK_ASSERT_EQ(regular("( 1, 2, 3, )"),           "( \"1\", \"2\", \"3\" )");
	OAK_ASSERT_EQ(regular("( { a = 1; b = 2; } )"),  "(\n\t{\ta = \"1\";\n\t\tb = \"2\";\n\t},\n)");
}

void test_enhanced ()
{
	OAK_ASSERT_EQ(enhanced("{ foo = \"bar\\\\n\"; }"), "{ foo = 'bar\\n'; }");
	OAK_ASSERT_EQ(enhanced("{ foo = \"'\"; }"),        "{ foo = \"'\"; }");
}
