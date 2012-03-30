#include <theme/theme.h>
#include <oak/duration.h>
#include <scope/compile.h>
#import <test/bundle_index.h>

class ThemeSpeedTests : public CxxTest::TestSuite
{
	int repeat =  10;//000;
	std::string grammar()
	{
		std::string str;
		if(FILE* fp = fopen("/Users/joachimm/Projects/avian/Frameworks/theme/tests/BrillianceBlack.plist", "r"))
		{
			char buf[1024];
			while(size_t len = fread(buf, 1, sizeof(buf), fp))
				str.insert(str.end(), buf, buf + len);
			fclose(fp);
		}
		return str;
		
	}
public:
	void test_theme_speed1 ()
	{
		static scope::scope_t const textScope = "text.html.markdown meta.paragraph.markdown markup.bold.markdown";
		test::bundle_index_t bundleIndex;
		bundles::item_ptr TestGrammarItem;
		//printf("grammar:%s", grammar().c_str());
		TestGrammarItem = bundleIndex.add(bundles::kItemTypeTheme, grammar());
		bundleIndex.commit();

		theme_t theme(TestGrammarItem);
		oak::duration_t timer1;
		
		for(int i = 0; i < repeat ; i++)
		theme.styles_for_scope(textScope, "", 1.0);
	   
	 	printf ("%.4f seconds to classic theme\n", timer1.duration());
		
	}
	
	void test_theme_speed2 ()
	{
		static scope::context_t const textScope = "text.html.markdown meta.paragraph.markdown markup.bold.markdown";
		test::bundle_index_t bundleIndex;
		bundles::item_ptr TestGrammarItem;
		//printf("grammar:%s", grammar().c_str());
		TestGrammarItem = bundleIndex.add(bundles::kItemTypeTheme, grammar());
		bundleIndex.commit();

		theme_t theme(TestGrammarItem);
		std::vector<theme_t::decomposed_style_t> list = theme._styles; 
		scope::compile::compiled_t<theme_t::decomposed_style_t> compiled = scope::compile::compile(list);
		
		oak::duration_t timer2;
		for(int i = 0; i < repeat ; i++)
		{
			std::multimap<double, theme_t::decomposed_style_t> ordered;
			compiled.match(textScope, ordered);
		}
	 	printf ("%.4f seconds to new theme\n", timer2.duration());
		
	
	}
	
	
};