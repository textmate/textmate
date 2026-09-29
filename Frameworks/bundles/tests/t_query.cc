#include <bundles/src/bundles.h>
#include <test/bundle_index.h>
#include <test/jail.h>

void setup_fixtures ()
{
	static std::string BaseEnvironment =
		"{	name     = 'Base Environment';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'TEST'; value = 'foo'; },"
		"		);"
		"	};"
		"}";

	static std::string PathEnvironment =
		"{	name     = 'Path Environment';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'PATH'; value = '/usr/bin';                 },"
		"			{	name = 'PATH'; value = '$PATH:/bin';               },"
		"			{	name = 'PATH'; value = '$PATH:/tmp'; disabled = 1; },"
		"			{	name = 'PATH'; value = '$PATH:/sbin';              },"
		"		);"
		"	};"
		"}";

	static std::string TeXEnvironment =
		"{	name     = 'TeX Environment';"
		"	scope    = 'text.tex';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'PATH'; value = '$PATH:/usr/texbin'; },"
		"		);"
		"	};"
		"}";

	static std::string CxxEnvironment =
		"{	name     = 'C++ Environment';"
		"	scope    = 'source.c++';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'TEST'; value = '${TEST:+$TEST:}bar'; },"
		"		);"
		"	};"
		"}";

	static std::string DialogEnvironment =
		"{	name     = 'Dialog Environment';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'DialogPath'; value = '${TM_DIALOG_BUNDLE_SUPPORT:?$TM_DIALOG_BUNDLE_SUPPORT/bin:*** Dialog bundle missing ***}'; },"
		"		);"
		"	};"
		"	require = ("
		"		{ name = 'Dialog'; uuid = 'B0B94C92-1870-491C-A928-9528387EEACA'; },"
		"	);"
		"}";

	static std::string BaseCommentEnvironment =
		"{	name     = 'Base Environment';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'TM_COMMENT_START';    value = '/*';                   },"
		"			{	name = 'TM_COMMENT_STOP';     value = '*/';                   },"
		"			{	name = 'TM_COMMENT_START_2';  value = '//';                   },"
		"			{	name = 'TM_COMMENT_STYLE';    value = '$TM_BUNDLE_ITEM_NAME'; },"
		"		);"
		"	};"
		"}";

	static std::string RubyCommentEnvironment =
		"{	name     = 'Ruby Environment';"
		"	scope    = 'source.ruby';"
		"	settings = {"
		"		shellVariables = ("
		"			{	name = 'TM_COMMENT_START';    value = '# ';                   },"
		"			{	name = 'TM_COMMENT_START_2';  value = '==begin';              },"
		"			{	name = 'TM_COMMENT_STOP_2';   value = '==end';                },"
		"			{	name = 'TM_COMMENT_STYLE';    value = '$TM_BUNDLE_ITEM_NAME'; },"
		"		);"
		"	};"
		"}";

	static std::string BaseSnippet =
		"{	name          = 'Base Snippet';"
		"	keyEquivalent = '^p';"
		"	tabTrigger    = 'bla';"
		"	content       = 'foo';"
		"}";

	static std::string CxxSnippet =
		"{	name          = 'C++ Snippet';"
		"	keyEquivalent = '^p';"
		"	tabTrigger    = 'bla';"
		"	scope         = 'source.c++';"
		"	content       = 'bar';"
		"}";

	static std::string DisabledCxxSnippet =
		"{	name          = 'Disabled C++ Snippet';"
		"	keyEquivalent = '^p';"
		"	tabTrigger    = 'bla';"
		"	scope         = 'source.c++';"
		"	content       = 'bar';"
		"	isDisabled    = 1;"
		"}";

	static std::string TrueWithLocation =
		"{	name = 'TrueWithLocation';"
		"	requiredCommands = ("
		"		{	command = 'true';"
		"			locations = ( '/usr/bin/true' );"
		"		},"
		"	);"
		"}";

	static std::string TrueWithVariable =
		"{	name = 'TrueWithVariable';"
		"	requiredCommands = ("
		"		{	command = 'true';"
		"			variable = 'TM_TRUE';"
		"		},"
		"	);"
		"}";

	static std::string TrueWithLocationAndVariable =
		"{	name = 'TrueWithLocationAndVariable';"
		"	requiredCommands = ("
		"		{	command = 'true';"
		"			locations = ( '/usr/bin/true' );"
		"			variable = 'TM_TRUE';"
		"		},"
		"	);"
		"}";

	static std::string TrueWithBadLocation =
		"{	name = 'TrueWithBadLocation';"
		"	requiredCommands = ("
		"		{	command = 'true';"
		"			locations = ( '/foo/bar/true' );"
		"		},"
		"	);"
		"}";

	static std::string TrueWithBadLocationAndVariable =
		"{	name = 'TrueWithBadLocationAndVariable';"
		"	requiredCommands = ("
		"		{	command = 'true';"
		"			locations = ( '/foo/bar/true' );"
		"			variable = 'TM_TRUE';"
		"		},"
		"	);"
		"}";

	test::bundle_index_t bundleIndex;
	bundleIndex.add(bundles::kItemTypeSettings, BaseEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, BaseCommentEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, PathEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, DialogEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, TeXEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, CxxEnvironment);
	bundleIndex.add(bundles::kItemTypeSettings, RubyCommentEnvironment);
	bundleIndex.add(bundles::kItemTypeSnippet,  BaseSnippet);
	bundleIndex.add(bundles::kItemTypeSnippet,  CxxSnippet);
	bundleIndex.add(bundles::kItemTypeSnippet,  DisabledCxxSnippet);

	bundleIndex.add(bundles::kItemTypeCommand,  TrueWithLocation);
	bundleIndex.add(bundles::kItemTypeCommand,  TrueWithVariable);
	bundleIndex.add(bundles::kItemTypeCommand,  TrueWithLocationAndVariable);
	bundleIndex.add(bundles::kItemTypeCommand,  TrueWithBadLocation);
	bundleIndex.add(bundles::kItemTypeCommand,  TrueWithBadLocationAndVariable);

	bundles::item_ptr dialogBundle = bundleIndex.add(bundles::kItemTypeBundle, "{ name = 'Dialog'; uuid = 'B0B94C92-1870-491C-A928-9528387EEACA'; }");

	static test::jail_t jail;
	bundles::set_locations(std::vector<std::string>(1, jail.path()));
	dialogBundle->save();
	jail.mkdir("Bundles/Dialog.tmbundle/Support");

	bundleIndex.commit();
}

#define OAK_ASSERT_STR_EQ(lhs, rhs) do { std::string _lhs = (lhs); std::string _rhs = (rhs); if(!(_lhs == _rhs)) oak_assertion_error(oak_format_bad_relation(#lhs, "==", #rhs, to_s(_lhs), "!=", to_s(_rhs)), __FILE__, __LINE__); } while(false)

void test_environment_format_strings ()
{
	std::map<std::string, std::string> base;

	OAK_ASSERT_EQ(bundles::scope_variables(base, "")["TEST"],                        "foo");
	OAK_ASSERT_EQ(bundles::scope_variables(base, "source.c++")["TEST"],              "foo:bar");
	OAK_ASSERT_EQ(bundles::scope_variables(base, "source.any")["TM_COMMENT_STYLE"],  "Base Environment");
	OAK_ASSERT_EQ(bundles::scope_variables(base, "source.ruby")["TM_COMMENT_STYLE"], "Ruby Environment");

	OAK_ASSERT_EQ(bundles::scope_variables(base, "text.plain")["PATH"], "/usr/bin:/bin:/sbin");
	OAK_ASSERT_EQ(bundles::scope_variables(base, "text.tex").find("PATH")->second,   "/usr/bin:/bin:/sbin:/usr/texbin");
	OAK_ASSERT_EQ(bundles::scope_variables(base, "text.tex")["PATH"], "/usr/bin:/bin:/sbin:/usr/texbin");
}

void test_v1_variable_shadowing ()
{
	auto baseEnv = bundles::scope_variables(std::map<std::string, std::string>(), "");
	OAK_ASSERT_EQ(baseEnv["TM_COMMENT_START"],   "/*");
	OAK_ASSERT_EQ(baseEnv["TM_COMMENT_STOP"],    "*/");
	OAK_ASSERT_EQ(baseEnv["TM_COMMENT_START_2"], "//");
	OAK_ASSERT(baseEnv.find("TM_COMMENT_STOP_2") == baseEnv.end());

	std::map<std::string, std::string> rubyEnv = bundles::scope_variables(std::map<std::string, std::string>(), "source.ruby");
	OAK_ASSERT_EQ(rubyEnv["TM_COMMENT_START"],   "# ");
	OAK_ASSERT(rubyEnv.find("TM_COMMENT_STOP") == rubyEnv.end());
	OAK_ASSERT_EQ(rubyEnv["TM_COMMENT_START_2"], "==begin");
	OAK_ASSERT_EQ(rubyEnv["TM_COMMENT_STOP_2"],  "==end");
}

void test_scope_query ()
{
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++").size(), 1);
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).size(), 2);
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false, true).size(), 3);

	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.any").size(), 1);
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.any").front()->name(), "Base Snippet");
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++").size(), 1);
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++").front()->name(), "C++ Snippet");

	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).size(), 2);
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).front()->name(), "C++ Snippet");
	OAK_ASSERT_EQ(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).back()->name(),  "Base Snippet");
}

void test_require ()
{
	std::string dialogPath = bundles::scope_variables(std::map<std::string, std::string>(), "text")["DialogPath"];
	std::string pathSuffix = "/Bundles/Dialog.tmbundle/Support/bin";
	OAK_ASSERT_EQ(dialogPath.find(pathSuffix) + pathSuffix.size(), dialogPath.size());
}
