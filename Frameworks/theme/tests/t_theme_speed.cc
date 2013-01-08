#include <theme/theme.h>
#include <oak/duration.h>
#include <scope/compile.h>
#import <test/bundle_index.h>
#import <io/path.h>
#import <text/parse.h>

class ThemeSpeedTests : public CxxTest::TestSuite
{

	int repeat =  100;
	std::string grammar()
	{
		std::string str;
		if(FILE* fp = fopen(path::join(__FILE__, "../BrillianceBlack.plist").c_str(), "r"))		{
			char buf[1024];
			while(size_t len = fread(buf, 1, sizeof(buf), fp))
				str.insert(str.end(), buf, buf + len);
			fclose(fp);
		}
		return str;
		
	}
	
	std::vector<std::string> scopes()
	{
		std::string str;
		if(FILE* fp = fopen(path::join(__FILE__, "../pjax.scopes").c_str(), "r"))		{
			char buf[1024];
			while(size_t len = fread(buf, 1, sizeof(buf), fp))
				str.insert(str.end(), buf, buf + len);
			fclose(fp);
		}
		
		std::vector<std::string> scopes = text::split(str, "\n");
		return scopes;
		
	}
	
public:
	void test_theme_speed1 ()
	{
		test::bundle_index_t bundleIndex;
		bundles::item_ptr TestGrammarItem;
		//printf("grammar:%s", grammar().c_str());
		TestGrammarItem = bundleIndex.add(bundles::kItemTypeTheme, grammar());
		bundleIndex.commit();

		theme_t theme(TestGrammarItem);
		std::vector<std::string> _scopes = scopes();
		std::vector<scope::context_t::context_t> contexts;

		iterate(textScope, _scopes)
			contexts.push_back(scope::context_t::context_t(*textScope));
		oak::duration_t timer1;
		
		for(int i = 0; i < repeat ; i++)
			iterate(textScope, contexts)
			{				
				theme.styles_for_scope(*textScope, "", 1.0);
			}

	 	printf ("%.4f seconds to classic theme\n", timer1.duration());
		
	}
};