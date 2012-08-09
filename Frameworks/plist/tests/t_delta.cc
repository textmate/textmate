#include <plist/ascii.h>
#include <plist/delta.h>

class DeltaTests : public CxxTest::TestSuite
{
public:
	void test_delta ()
	{
		std::string oldPlistString =
			"{	foo = bar;\n"
			"	bar = bar;\n"
			"	duff = me;\n"
			"	array_1 = ( 1, 2, 3 );\n"
			"	array_2 = ( 1, 3 );\n"
			"	array_3 = ( power, ( me, { bad = good; } ), 3 );\n"
			"	dict_1 = {\n"
			"		key = value;\n"
			"		foo = ( bar );\n"
			"		nested = {\n"
			"			funny = shit;\n"
			"			more = clean;\n"
			"		};\n"
			"	};\n"
			"	dict_2 = {\n"
			"		key = value;\n"
			"		foo = ( bar );\n"
			"		nested = {\n"
			"			funny = shit;\n"
			"			more = clean;\n"
			"		};\n"
			"	};\n"
			"}";

		std::string newPlistString =
			"{	foo = bar;\n"
			"	duff = other;\n"
			"	charlie = sheen;\n"
			"	array_1 = ( 1, 2, 3 );\n"
			"	array_2 = ( 1, 5, 3 );\n"
			"	array_3 = ( power, ( me, { bad = good; } ), 3 );\n"
			"	dict_1 = {\n"
			"		key = value;\n"
			"		foo = ( bar );\n"
			"		nested = {\n"
			"			funny = shit;\n"
			"			more = explicit;\n"
			"		};\n"
			"	};\n"
			"	dict_2 = {\n"
			"		key = value;\n"
			"		foo = ( bar );\n"
			"		nested = {\n"
			"			funny = shit;\n"
			"			less = clean;\n"
			"		};\n"
			"	};\n"
			"}";

		std::string deltaPlistString =
			"{	deleted = ( bar, 'dict_2.nested.more' );\n"
			"	changed = {\n"
			"		duff = other;\n"
			"		charlie = sheen;\n"
			"		array_2 = ( 1, 5, 3 );\n"
			"		'dict_1.nested.more' = explicit;\n"
			"		'dict_2.nested.less' = clean;\n"
			"	};\n"
			"	isDelta = :true;\n"
			"}";

		plist::dictionary_t const oldPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(oldPlistString));
		plist::dictionary_t const newPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(newPlistString));
		plist::dictionary_t const deltaPlist = boost::get<plist::dictionary_t>(plist::parse_ascii(deltaPlistString));

		TS_ASSERT_EQUALS(to_s(plist::create_delta(oldPlist, newPlist)), to_s(deltaPlist));

		std::vector<plist::dictionary_t> plists;
		plists.push_back(deltaPlist);
		plists.push_back(oldPlist);
		TS_ASSERT_EQUALS(to_s(plist::merge_delta(plists)), to_s(newPlist));

		// =======================
		// = Test plist::equal() =
		// =======================

		TS_ASSERT(plist::equal(plist::create_delta(oldPlist, newPlist), deltaPlist));
		TS_ASSERT(plist::equal(plist::merge_delta(plists), newPlist));
		TS_ASSERT(!plist::equal(oldPlist, newPlist));
	}
};
