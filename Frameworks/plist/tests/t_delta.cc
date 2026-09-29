#include <plist/src/ascii.h>
#include <plist/src/delta.h>

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

	OAK_ASSERT_EQ(to_s(plist::create_delta(oldPlist, newPlist)), to_s(deltaPlist));

	std::vector<plist::dictionary_t> plists{ deltaPlist, oldPlist };
	OAK_ASSERT_EQ(to_s(plist::merge_delta(plists)), to_s(newPlist));

	// =======================
	// = Test plist::equal() =
	// =======================

	OAK_ASSERT(plist::equal(plist::create_delta(oldPlist, newPlist), deltaPlist));
	OAK_ASSERT(plist::equal(plist::merge_delta(plists), newPlist));
	OAK_ASSERT(!plist::equal(oldPlist, newPlist));
}

void test_delta_settings_changed ()
{
	std::string oldPlistString =
		"{	name = 'Tag Preferences';\n"
		"	scope = 'meta.tag';\n"
		"	settings = ( 'smartTypingPairs', 'spellChecking' );\n"
		"	uuid = '73251DBE-EBD2-470F-8148-E6F2EC1A9641';\n"
		"}\n";

	std::string deltaPlistString =
		"{	changed = {\n"
		"		settings.shellVariables = (\n"
		"			{	name = 'TM_FOO';\n"
		"				value = 'bar';\n"
		"			},\n"
		"		);\n"
		"	};\n"
		"	isDelta = :true;\n"
		"	uuid = '73251DBE-EBD2-470F-8148-E6F2EC1A9641';\n"
		"}\n";

	std::string newPlistString =
		"{	name = 'Tag Preferences';\n"
		"	scope = 'meta.tag';\n"
		"	settings = ( 'smartTypingPairs', 'spellChecking', 'shellVariables' );\n"
		"	uuid = '73251DBE-EBD2-470F-8148-E6F2EC1A9641';\n"
		"}\n";

	plist::dictionary_t const oldPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(oldPlistString));
	plist::dictionary_t const newPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(newPlistString));
	plist::dictionary_t const deltaPlist = boost::get<plist::dictionary_t>(plist::parse_ascii(deltaPlistString));

	std::vector<plist::dictionary_t> plists{ deltaPlist, oldPlist };
	OAK_ASSERT_EQ(to_s(plist::merge_delta(plists)), to_s(newPlist));
}

void test_delta_settings_deleted ()
{
	std::string oldPlistString =
		"{	name = 'Unprintable';\n"
		"	scope = 'deco.unprintable';\n"
		"	settings = ( 'background', 'fontName', 'fontSize', 'foreground' );\n"
		"	uuid = '20881CB9-5D12-4D74-8EE6-9ABAA7B408D3';\n"
		"}\n";

	std::string deltaPlistString =
		"{	deleted = ( 'settings.fontName', 'settings.fontSize' );\n"
		"	isDelta = :true;\n"
		"	uuid = '20881CB9-5D12-4D74-8EE6-9ABAA7B408D3';\n"
		"}\n";

	std::string newPlistString =
		"{	name = 'Unprintable';\n"
		"	scope = 'deco.unprintable';\n"
		"	settings = ( 'background', 'foreground' );\n"
		"	uuid = '20881CB9-5D12-4D74-8EE6-9ABAA7B408D3';\n"
		"}\n";

	plist::dictionary_t const oldPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(oldPlistString));
	plist::dictionary_t const newPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(newPlistString));
	plist::dictionary_t const deltaPlist = boost::get<plist::dictionary_t>(plist::parse_ascii(deltaPlistString));

	std::vector<plist::dictionary_t> plists{ deltaPlist, oldPlist };
	OAK_ASSERT_EQ(to_s(plist::merge_delta(plists)), to_s(newPlist));
}

void test_delta_keys_with_dots ()
{
	std::string oldPlistString =
		"{	name = 'HTML Grammar';\n"
		"	injections = {\n"
		"		'text.html.basic' = foo;\n"
		"		'text.html.php' = bar;\n"
		"	};\n"
		"}\n";

	std::string deltaPlistString =
		"{	changed = {\n"
		"		'injections.text\\.html\\.basic' = bar;\n"
		"		'injections.text\\.html\\.markdown' = { name = something; };\n"
		"	};\n"
		"	deleted = (\n"
		"		'injections.text\\.html\\.php'\n"
		"	);\n"
		"	isDelta = :true;\n"
		"}\n";

	std::string newPlistString =
		"{	name = 'HTML Grammar';\n"
		"	injections = {\n"
		"		'text.html.basic' = bar;\n"
		"		'text.html.markdown' = { name = something; };\n"
		"	};\n"
		"}\n";

	plist::dictionary_t const oldPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(oldPlistString));
	plist::dictionary_t const newPlist   = boost::get<plist::dictionary_t>(plist::parse_ascii(newPlistString));
	plist::dictionary_t const deltaPlist = boost::get<plist::dictionary_t>(plist::parse_ascii(deltaPlistString));

	OAK_ASSERT_EQ(to_s(plist::create_delta(oldPlist, newPlist)), to_s(deltaPlist));

	std::vector<plist::dictionary_t> plists{ deltaPlist, oldPlist };
	OAK_ASSERT_EQ(to_s(plist::merge_delta(plists)), to_s(newPlist));
}
