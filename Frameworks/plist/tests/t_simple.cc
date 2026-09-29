#include <plist/src/ascii.h>
#include <plist/src/stl.h>

static int32_t parse_int (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	if(int32_t const* res = boost::get<int32_t>(&plist))
		return *res;
	return 0;
}

static std::string parse_str (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	if(std::string const* res = boost::get<std::string>(&plist))
		return *res;
	return NULL_STR;
}

static bool is_array (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	return boost::get< std::vector<plist::any_t> >(&plist);
}

static std::vector<plist::any_t> parse_array (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	if(std::vector<plist::any_t> const* res = boost::get< std::vector<plist::any_t> >(&plist))
		return *res;
	return std::vector<plist::any_t>();
}

static bool is_dict (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	return boost::get< std::map<std::string, plist::any_t> >(&plist);
}

static std::map<std::string, plist::any_t> parse_dict (std::string const& src)
{
	plist::any_t plist = plist::parse_ascii(src);
	if(std::map<std::string, plist::any_t> const* res = boost::get< std::map<std::string, plist::any_t> >(&plist))
		return *res;
	return std::map<std::string, plist::any_t>();
}

void test_basic ()
{
	plist::any_t empty;
	// OAK_ASSERT(boost::get<bool>(&empty)        == nullptr);
	OAK_ASSERT(boost::get<int32_t>(&empty)     == nullptr);
	OAK_ASSERT(boost::get<std::string>(&empty) == nullptr);
}

void test_true_test ()
{
	OAK_ASSERT_EQ(plist::is_true(plist::any_t()),                     false);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(false)),                false);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(true)),                 true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(0)),                    false);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(1)),                    true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("0"))),     false);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("1"))),     true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("NO"))),    true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("YES"))),   true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("false"))), true);
	OAK_ASSERT_EQ(plist::is_true(plist::any_t(std::string("true"))),  true);
}

void test_int ()
{
	OAK_ASSERT_EQ(parse_int("1"),            1);
	OAK_ASSERT_EQ(parse_int("42"),          42);
	OAK_ASSERT_EQ(parse_int("325643"),  325643);
	OAK_ASSERT_EQ(parse_int("1.0"),          0);
}

void test_bool ()
{
	plist::any_t truePlist = plist::parse_ascii(":true");
	OAK_ASSERT(boost::get<bool>(&truePlist));
	OAK_ASSERT_EQ(boost::get<bool>(truePlist), true);

	plist::any_t falsePlist = plist::parse_ascii(":false");
	OAK_ASSERT(boost::get<bool>(&falsePlist));
	OAK_ASSERT_EQ(boost::get<bool>(falsePlist), false);

	plist::any_t mixedPlist = plist::parse_ascii("( :true, :false )");
	OAK_ASSERT(boost::get< std::vector<plist::any_t> >(&mixedPlist));
	std::vector<plist::any_t> const& v = boost::get< std::vector<plist::any_t> >(mixedPlist);
	OAK_ASSERT_EQ(boost::get<bool>(v[0]), true);
	OAK_ASSERT_EQ(boost::get<bool>(v[1]), false);

	// plist::any_t badPlist = plist::parse_ascii(":bad");
	// OAK_ASSERT_EQ(boost::get<bool>(&badPlist), (bool*)nullptr);
}

void test_double_strings ()
{
	OAK_ASSERT_EQ(parse_str("\"bar\""),              "bar");
	OAK_ASSERT_EQ(parse_str("\"\\\"bar\\\"\""),      "\"bar\"");
	OAK_ASSERT_EQ(parse_str("\"\\b\\a\\r\""),        "\\b\\a\\r");
	OAK_ASSERT_EQ(parse_str("\"\\\\b\\\\a\\\\r\""),  "\\b\\a\\r");
	OAK_ASSERT_EQ(parse_str("\"bar"),                NULL_STR);
	OAK_ASSERT_EQ(parse_str("\"bar\\\""),            NULL_STR);
}

void test_single_strings ()
{
	OAK_ASSERT_EQ(parse_str("'bar'"),              "bar");
	OAK_ASSERT_EQ(parse_str("'''bar'''"),          "'bar'");
	OAK_ASSERT_EQ(parse_str("'\\b\\a\\r'"),        "\\b\\a\\r");
	OAK_ASSERT_EQ(parse_str("'\\\\b\\\\a\\\\r'"),  "\\\\b\\\\a\\\\r");
	OAK_ASSERT_EQ(parse_str("'bar"),               NULL_STR);
	OAK_ASSERT_EQ(parse_str("'bar\\'"),            "bar\\");
}

void test_bare_strings ()
{
	OAK_ASSERT_EQ(parse_str("bar"),       "bar");
	OAK_ASSERT_EQ(parse_str("bar5"),      "bar5");
	OAK_ASSERT_EQ(parse_str("bar_"),      "bar_");
	OAK_ASSERT_EQ(parse_str("bar-"),      "bar-");
	OAK_ASSERT_EQ(parse_str("bar."),      "bar.");
	OAK_ASSERT_EQ(parse_str("_bar"),      "_bar");
	OAK_ASSERT_EQ(parse_str(".bar"),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("5bar"),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("-bar"),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("-5bar"),     NULL_STR);
	OAK_ASSERT_EQ(parse_str("bar,"),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("bar)"),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("bar="),      NULL_STR);
	OAK_ASSERT_EQ(parse_str("bar;"),      NULL_STR);
}

void test_array ()
{
	OAK_ASSERT_EQ(is_array("( )"),       true);
	OAK_ASSERT_EQ(is_array("( a )"),     true);
	OAK_ASSERT_EQ(is_array("( a, )"),    true);
	OAK_ASSERT_EQ(is_array("( "),        false);
	OAK_ASSERT_EQ(is_array("( , )"),     false);
	OAK_ASSERT_EQ(is_array("( a "),      false);
	OAK_ASSERT_EQ(is_array("( a b )"),   false);

	OAK_ASSERT_EQ(parse_array("( )").size(),       0);
	OAK_ASSERT_EQ(parse_array("( a )").size(),     1);
	OAK_ASSERT_EQ(parse_array("( a, )").size(),    1);
	OAK_ASSERT_EQ(parse_array("( a, b )").size(),  2);
	OAK_ASSERT_EQ(parse_array("( a, b, )").size(), 2);
}

void test_dict ()
{
	OAK_ASSERT_EQ(is_dict("{ }"),                       true);
	OAK_ASSERT_EQ(is_dict("{ key = val; }"),            true);
	OAK_ASSERT_EQ(is_dict("{ key = val; key = val; }"), true);
	OAK_ASSERT_EQ(is_dict("{ 1 = val; 2 = val; }"),     true);
	OAK_ASSERT_EQ(is_dict("{ key = val }"),             false);
	OAK_ASSERT_EQ(is_dict("{ key = val; "),             false);
	OAK_ASSERT_EQ(is_dict("{ key val; }"),              false);
	OAK_ASSERT_EQ(is_dict("{ key = 'val; }"),           false);

	OAK_ASSERT_EQ(parse_dict("{ }").size(),                      0);
	OAK_ASSERT_EQ(parse_dict("{ key = val; }").size(),           1);
	OAK_ASSERT_EQ(parse_dict("{ 1 = val; 2 = val; }").size(),    2);
}

void test_multi_line_comment ()
{
	OAK_ASSERT_EQ(parse_str("/* ** */ /* /* */ bar /* ** */ /* /* */"), "bar");
	OAK_ASSERT_EQ(parse_array("/* … */ ( /* … */ element /* … */ ) /* … */").size(), 1);
	OAK_ASSERT_EQ(parse_dict("/* … */ { /* … */ key /* … */ = /* … */ value /* … */ ; /* … */ } /* … */").size(), 1);
}

void test_single_line_comment ()
{
	OAK_ASSERT_EQ(parse_str("// …\n bar // …"), "bar");
	OAK_ASSERT_EQ(parse_array("( // …\n element, // …\n element )").size(), 2);
	OAK_ASSERT_EQ(parse_dict("// …\n { key = // …\n value // …\n ; }").size(), 1);
}

struct actual_type_t : boost::static_visitor<char const*>
{
	char const* operator() (bool flag) const                       { return "bool";                }
	char const* operator() (int32_t i) const                       { return "int32_t";             }
	char const* operator() (uint64_t i) const                      { return "uint64_t";            }
	char const* operator() (std::string const& str) const          { return "std::string";         }
	char const* operator() (std::vector<char> const& data) const   { return "std::vector<char>";   }
	char const* operator() (oak::date_t const& date) const         { return "oak::date_t";         }
	char const* operator() (plist::array_t const& array) const     { return "plist::array_t";      }
	char const* operator() (plist::dictionary_t const& dict) const { return "plist::dictionary_t"; }
};

static std::string plist_type (std::string const& plistString)
{
	plist::any_t plist = plist::parse_ascii(plistString);
	return boost::apply_visitor(actual_type_t(), plist);
}

void test_number_type ()
{
	OAK_ASSERT_EQ(plist_type("-2147483648"), "int32_t");
	OAK_ASSERT_EQ(plist_type("-1073741824"), "int32_t");
	OAK_ASSERT_EQ(plist_type(         "-1"), "int32_t");
	OAK_ASSERT_EQ(plist_type(          "0"), "int32_t");
	OAK_ASSERT_EQ(plist_type(          "1"), "int32_t");
	OAK_ASSERT_EQ(plist_type( "1073741823"), "int32_t");
	OAK_ASSERT_EQ(plist_type( "2147483647"), "int32_t");
	OAK_ASSERT_EQ(plist_type( "0x7fffffff"), "int32_t");

	OAK_ASSERT_EQ(plist_type( "2147483648"), "uint64_t");
	OAK_ASSERT_EQ(plist_type( "0x80000000"), "uint64_t");
}

void test_number_conversion ()
{
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(  "+32")),       32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(   "32")),       32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(  "-32")),      -32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii("+0x32")),     0x32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii( "0x32")),     0x32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii("-0x32")),    -0x32);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(   "07")),        7);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii( "+010")),        8);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(  "010")),        8);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii(  "-07")),       -7);
	OAK_ASSERT_EQ(plist::get<int32_t>(plist::parse_ascii( "-010")),       -8);
}

void test_uint64_conversion ()
{
	OAK_ASSERT_EQ(plist::get<uint64_t>(plist::parse_ascii("8589934592")),     8589934592ULL);
	OAK_ASSERT_EQ(plist::get<uint64_t>(plist::parse_ascii("0x812345678")),   0x812345678ULL);
	OAK_ASSERT_EQ(plist::get<uint64_t>(plist::parse_ascii("'0x812345678'")), 0x812345678ULL);
}

void test_dictionary_keys ()
{
	std::map<std::string, int32_t> integerMap;
	integerMap["42"] = 1;
	integerMap["80"] = 2;
	OAK_ASSERT_EQ(to_s(plist::parse_ascii("{ 42 = 1; 80 = 2; }")), boost::to_s(plist::to_plist(integerMap)));

	std::map<std::string, std::string> booleanMap;
	booleanMap["1"] = std::string("true");
	booleanMap["0"] = std::string("false");
	OAK_ASSERT_EQ(to_s(plist::parse_ascii("{ :true = true; :false = false; }")), boost::to_s(plist::to_plist(booleanMap)));

	// OAK_ASSERT_EQ(to_s(plist::parse_ascii("{ ( bad ) = key; }")), to_s(plist::any_t(false)));
}
