#include <bundles/bundles.h>
#include <test/bundle_index.h>
#include <test/jail.h>

class QueryTests : public CxxTest::TestSuite
{
	test::jail_t jail;
public:
	QueryTests ()
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
		bundles::item_ptr dialogBundle = bundleIndex.add(bundles::kItemTypeBundle, "{ name = 'Dialog'; uuid = 'B0B94C92-1870-491C-A928-9528387EEACA'; }");
		bundleIndex.commit();

		bundles::set_locations(std::vector<std::string>(1, jail.path()));
		dialogBundle->save();
		jail.mkdir("Bundles/Dialog.tmbundle/Support");
	}

	void test_environment_format_strings ()
	{
		TS_ASSERT_EQUALS(bundles::scope_variables("")["TEST"],                        "foo");
		TS_ASSERT_EQUALS(bundles::scope_variables("source.c++")["TEST"],              "foo:bar");
		TS_ASSERT_EQUALS(bundles::scope_variables("source.any")["TM_COMMENT_STYLE"],  "Base Environment");
		TS_ASSERT_EQUALS(bundles::scope_variables("source.ruby")["TM_COMMENT_STYLE"], "Ruby Environment");

		TS_ASSERT_EQUALS(bundles::scope_variables("text.plain")["PATH"], "/usr/bin:/bin:/sbin");
		TS_ASSERT_EQUALS(bundles::scope_variables("text.tex")["PATH"],   "/usr/bin:/bin:/sbin:/usr/texbin");
	}

	void test_v1_variable_shadowing ()
	{
		std::map<std::string, std::string> baseEnv = bundles::scope_variables("");
		TS_ASSERT_EQUALS(baseEnv["TM_COMMENT_START"],   "/*");
		TS_ASSERT_EQUALS(baseEnv["TM_COMMENT_STOP"],    "*/");
		TS_ASSERT_EQUALS(baseEnv["TM_COMMENT_START_2"], "//");
		TS_ASSERT(baseEnv.find("TM_COMMENT_STOP_2") == baseEnv.end());

		std::map<std::string, std::string> rubyEnv = bundles::scope_variables("source.ruby");
		TS_ASSERT_EQUALS(rubyEnv["TM_COMMENT_START"],   "# ");
		TS_ASSERT(rubyEnv.find("TM_COMMENT_STOP") == rubyEnv.end());
		TS_ASSERT_EQUALS(rubyEnv["TM_COMMENT_START_2"], "==begin");
		TS_ASSERT_EQUALS(rubyEnv["TM_COMMENT_STOP_2"],  "==end");
	}

	void test_scope_query ()
	{
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++").size(), 1);
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).size(), 2);
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldKeyEquivalent, "^p", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false, true).size(), 3);

		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.any").size(), 1);
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.any").front()->name(), "Base Snippet");
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++").size(), 1);
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++").front()->name(), "C++ Snippet");

		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).size(), 2);
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).front()->name(), "C++ Snippet");
		TS_ASSERT_EQUALS(bundles::query(bundles::kFieldTabTrigger, "bla", "source.c++", bundles::kItemTypeMenuTypes, oak::uuid_t(), false).back()->name(),  "Base Snippet");
	}

	void test_require ()
	{
		TS_ASSERT_EQUALS(bundles::scope_variables("text")["DialogPath"], jail.path("Bundles/Dialog.tmbundle/Support/bin"));
	}

	void test_wrappers ()
	{
#if 0
		bundles::item_ptr match;
		plist::any_t value = bundles::value_for_setting("highlightPairs", "source.c", &match);
		fprintf(stderr, "highlightPairs (%s): %s\n", match ? match->full_name().c_str() : "no match", to_s(value).c_str());

		citerate(item, bundles::grammars_for_path("charlie.m"))
			fprintf(stderr, "charlie.m → %s\n", (*item)->full_name().c_str());
		citerate(item, bundles::drag_commands_for_path("FOO.CSS", "text.html"))
			fprintf(stderr, "FOO.CSS drop in HTML → %s\n", (*item)->full_name().c_str());
		citerate(item, bundles::drag_commands_for_path("foo.erb.html", "text.html"))
			fprintf(stderr, "foo.erb.html drop in HTML → %s\n", (*item)->full_name().c_str());
#endif
	}
};
