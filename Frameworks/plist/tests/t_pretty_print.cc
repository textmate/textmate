#include <plist/plist.h>

class PrettyPrintTests : public CxxTest::TestSuite
{
	static std::string regular (std::string const& plistSrc)  { return to_s(plist::parse(plistSrc)); }
	static std::string enhanced (std::string const& plistSrc) { return to_s(plist::parse(plistSrc), plist::kPreferSingleQuotedStrings); }

public:
	void test_regular ()
	{
		TS_ASSERT_EQUALS(regular("{ foo = \"bar\\\\n\"; }"), "{ foo = \"bar\\\\n\"; }");
		TS_ASSERT_EQUALS(regular("{ \"æble\" = bar; }"), "{ \"æble\" = \"bar\"; }");
		TS_ASSERT_EQUALS(regular("{ f-o_o. = bar; }"), "{ f-o_o. = \"bar\"; }");
		TS_ASSERT_EQUALS(regular("{ _foo = bar; }"), "{ _foo = \"bar\"; }");
		TS_ASSERT_EQUALS(regular("{ \".foo\" = bar; }"), "{ \".foo\" = \"bar\"; }");
		TS_ASSERT_EQUALS(regular("{ \"-foo\" = bar; }"), "{ \"-foo\" = \"bar\"; }");
	}

	void test_array_separator ()
	{
		TS_ASSERT_EQUALS(regular("( 1, 2, 3, )"),           "( \"1\", \"2\", \"3\" )");
		TS_ASSERT_EQUALS(regular("( { a = 1; b = 2; } )"),  "(\n\t{\ta = \"1\";\n\t\tb = \"2\";\n\t},\n)");
	}

	void test_enhanced ()
	{
		TS_ASSERT_EQUALS(enhanced("{ foo = \"bar\\\\n\"; }"), "{ foo = 'bar\\n'; }");
		TS_ASSERT_EQUALS(enhanced("{ foo = \"'\"; }"),        "{ foo = \"'\"; }");
	}
};
