#include <plist/ascii.h>
#include <plist/stl.h>

class SimpleTests : public CxxTest::TestSuite
{
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

public:
	void test_basic ()
	{
		plist::any_t empty;
		// TS_ASSERT_EQUALS(boost::get<bool>(&empty),        (bool*)NULL);
		TS_ASSERT_EQUALS(boost::get<int32_t>(&empty),     (int32_t*)NULL);
		TS_ASSERT_EQUALS(boost::get<std::string>(&empty), (std::string*)NULL);
	}

	void test_true_test ()
	{
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t()),                     false);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(false)),                false);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(true)),                 true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(0)),                    false);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(1)),                    true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("0"))),     false);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("1"))),     true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("NO"))),    true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("YES"))),   true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("false"))), true);
		TS_ASSERT_EQUALS(plist::is_true(plist::any_t(std::string("true"))),  true);
	}

	void test_int ()
	{
		TS_ASSERT_EQUALS(parse_int("1"),            1);
		TS_ASSERT_EQUALS(parse_int("42"),          42);
		TS_ASSERT_EQUALS(parse_int("325643"),  325643);
		TS_ASSERT_EQUALS(parse_int("1.0"),          0);
	}

	void test_bool ()
	{
		plist::any_t truePlist = plist::parse_ascii(":true");
		TS_ASSERT(boost::get<bool>(&truePlist));
		TS_ASSERT_EQUALS(boost::get<bool>(truePlist), true);

		plist::any_t falsePlist = plist::parse_ascii(":false");
		TS_ASSERT(boost::get<bool>(&falsePlist));
		TS_ASSERT_EQUALS(boost::get<bool>(falsePlist), false);

		plist::any_t mixedPlist = plist::parse_ascii("( :true, :false )");
		TS_ASSERT(boost::get< std::vector<plist::any_t> >(&mixedPlist));
		std::vector<plist::any_t> const& v = boost::get< std::vector<plist::any_t> >(mixedPlist);
		TS_ASSERT_EQUALS(boost::get<bool>(v[0]), true);
		TS_ASSERT_EQUALS(boost::get<bool>(v[1]), false);

		// plist::any_t badPlist = plist::parse_ascii(":bad");
		// TS_ASSERT_EQUALS(boost::get<bool>(&badPlist), (bool*)NULL);
	}

	void test_double_strings ()
	{
		TS_ASSERT_EQUALS(parse_str("\"bar\""),              "bar");
		TS_ASSERT_EQUALS(parse_str("\"\\\"bar\\\"\""),      "\"bar\"");
		TS_ASSERT_EQUALS(parse_str("\"\\b\\a\\r\""),        "\\b\\a\\r");
		TS_ASSERT_EQUALS(parse_str("\"\\\\b\\\\a\\\\r\""),  "\\b\\a\\r");
		TS_ASSERT_EQUALS(parse_str("\"bar"),                NULL_STR);
		TS_ASSERT_EQUALS(parse_str("\"bar\\\""),            NULL_STR);
	}

	void test_single_strings ()
	{
		TS_ASSERT_EQUALS(parse_str("'bar'"),              "bar");
		TS_ASSERT_EQUALS(parse_str("'''bar'''"),          "'bar'");
		TS_ASSERT_EQUALS(parse_str("'\\b\\a\\r'"),        "\\b\\a\\r");
		TS_ASSERT_EQUALS(parse_str("'\\\\b\\\\a\\\\r'"),  "\\\\b\\\\a\\\\r");
		TS_ASSERT_EQUALS(parse_str("'bar"),               NULL_STR);
		TS_ASSERT_EQUALS(parse_str("'bar\\'"),            "bar\\");
	}

	void test_bare_strings ()
	{
		TS_ASSERT_EQUALS(parse_str("bar"),       "bar");
		TS_ASSERT_EQUALS(parse_str("bar5"),      "bar5");
		TS_ASSERT_EQUALS(parse_str("bar_"),      "bar_");
		TS_ASSERT_EQUALS(parse_str("bar-"),      "bar-");
		TS_ASSERT_EQUALS(parse_str("bar."),      "bar.");
		TS_ASSERT_EQUALS(parse_str("_bar"),      "_bar");
		TS_ASSERT_EQUALS(parse_str(".bar"),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("5bar"),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("-bar"),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("-5bar"),     NULL_STR);
		TS_ASSERT_EQUALS(parse_str("bar,"),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("bar)"),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("bar="),      NULL_STR);
		TS_ASSERT_EQUALS(parse_str("bar;"),      NULL_STR);
	}

	void test_array ()
	{
		TS_ASSERT_EQUALS(is_array("( )"),       true);
		TS_ASSERT_EQUALS(is_array("( a )"),     true);
		TS_ASSERT_EQUALS(is_array("( a, )"),    true);
		TS_ASSERT_EQUALS(is_array("( "),        false);
		TS_ASSERT_EQUALS(is_array("( , )"),     false);
		TS_ASSERT_EQUALS(is_array("( a "),      false);
		TS_ASSERT_EQUALS(is_array("( a b )"),   false);

		TS_ASSERT_EQUALS(parse_array("( )").size(),       0);
		TS_ASSERT_EQUALS(parse_array("( a )").size(),     1);
		TS_ASSERT_EQUALS(parse_array("( a, )").size(),    1);
		TS_ASSERT_EQUALS(parse_array("( a, b )").size(),  2);
		TS_ASSERT_EQUALS(parse_array("( a, b, )").size(), 2);
	}

	void test_dict ()
	{
		TS_ASSERT_EQUALS(is_dict("{ }"),                       true);
		TS_ASSERT_EQUALS(is_dict("{ key = val; }"),            true);
		TS_ASSERT_EQUALS(is_dict("{ key = val; key = val; }"), true);
		TS_ASSERT_EQUALS(is_dict("{ 1 = val; 2 = val; }"),     true);
		TS_ASSERT_EQUALS(is_dict("{ key = val }"),             false);
		TS_ASSERT_EQUALS(is_dict("{ key = val; "),             false);
		TS_ASSERT_EQUALS(is_dict("{ key val; }"),              false);
		TS_ASSERT_EQUALS(is_dict("{ key = 'val; }"),           false);

		TS_ASSERT_EQUALS(parse_dict("{ }").size(),                      0);
		TS_ASSERT_EQUALS(parse_dict("{ key = val; }").size(),           1);
		TS_ASSERT_EQUALS(parse_dict("{ 1 = val; 2 = val; }").size(),    2);
	}

	void test_multi_line_comment ()
	{
		TS_ASSERT_EQUALS(parse_str("/* ** */ /* /* */ bar /* ** */ /* /* */"), "bar");
		TS_ASSERT_EQUALS(parse_array("/* … */ ( /* … */ element /* … */ ) /* … */").size(), 1);
		TS_ASSERT_EQUALS(parse_dict("/* … */ { /* … */ key /* … */ = /* … */ value /* … */ ; /* … */ } /* … */").size(), 1);
	}

	void test_single_line_comment ()
	{
		TS_ASSERT_EQUALS(parse_str("// …\n bar // …"), "bar");
		TS_ASSERT_EQUALS(parse_array("( // …\n element, // …\n element )").size(), 2);
		TS_ASSERT_EQUALS(parse_dict("// …\n { key = // …\n value // …\n ; }").size(), 1);
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

	char const* plist_type (std::string const& plistString)
	{
		plist::any_t plist = plist::parse_ascii(plistString);
		return boost::apply_visitor(actual_type_t(), plist);
	}

 	void test_number_type ()
	{
		TS_ASSERT_EQUALS(plist_type("-2147483648"), "int32_t");
		TS_ASSERT_EQUALS(plist_type("-1073741824"), "int32_t");
		TS_ASSERT_EQUALS(plist_type(         "-1"), "int32_t");
		TS_ASSERT_EQUALS(plist_type(          "0"), "int32_t");
		TS_ASSERT_EQUALS(plist_type(          "1"), "int32_t");
		TS_ASSERT_EQUALS(plist_type( "1073741823"), "int32_t");
		TS_ASSERT_EQUALS(plist_type( "2147483647"), "int32_t");
		TS_ASSERT_EQUALS(plist_type( "0x7fffffff"), "int32_t");

		TS_ASSERT_EQUALS(plist_type( "2147483648"), "uint64_t");
		TS_ASSERT_EQUALS(plist_type( "0x80000000"), "uint64_t");
	}

 	void test_number_conversion ()
	{
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(  "+32")),       32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(   "32")),       32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(  "-32")),      -32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii("+0x32")),     0x32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii( "0x32")),     0x32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii("-0x32")),    -0x32);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(   "07")),        7);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii( "+010")),        8);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(  "010")),        8);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii(  "-07")),       -7);
		TS_ASSERT_EQUALS(plist::get<int32_t>(plist::parse_ascii( "-010")),       -8);
	}

 	void test_uint64_conversion ()
	{
		TS_ASSERT_EQUALS(plist::get<uint64_t>(plist::parse_ascii("8589934592")),     8589934592ULL);
		TS_ASSERT_EQUALS(plist::get<uint64_t>(plist::parse_ascii("0x812345678")),   0x812345678ULL);
		TS_ASSERT_EQUALS(plist::get<uint64_t>(plist::parse_ascii("'0x812345678'")), 0x812345678ULL);
	}

	void test_dictionary_keys ()
	{
		std::map<std::string, int32_t> integerMap;
		integerMap["42"] = 1;
		integerMap["80"] = 2;
		TS_ASSERT_EQUALS(to_s(plist::parse_ascii("{ 42 = 1; 80 = 2; }")), to_s(plist::to_plist(integerMap)));

		std::map<std::string, std::string> booleanMap;
		booleanMap["1"] = std::string("true");
		booleanMap["0"] = std::string("false");
		TS_ASSERT_EQUALS(to_s(plist::parse_ascii("{ :true = true; :false = false; }")), to_s(plist::to_plist(booleanMap)));

		// TS_ASSERT_EQUALS(to_s(plist::parse_ascii("{ ( bad ) = key; }")), to_s(plist::any_t(false)));
	}
};
