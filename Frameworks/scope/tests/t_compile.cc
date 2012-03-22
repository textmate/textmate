#include "scope/compile.h"
#include <test/bundle_index.h>
class CompileTests : public CxxTest::TestSuite
{
public:
	
	class selector_container_t
	{
		scope::selector_t selector;
	public:
		selector_container_t(scope::selector_t s) : selector(s) {}
		scope::selector_t scope_selector () const { return selector;}
	};

	void test_compile ()
	{
		static scope::selector_t const matchingSelectors[] =
		{

			"text.*, markup.bold",
			"text, markup.bold",
			"markup.bold",
			"text.html, meta.*.markdown markup",
			"text.html, meta.* markup",
			"text.html, * markup, meta.beta.markup",
			"text.html, markup",
			"text, markup",
			"markup",
			"text.html",
			"text",
			"text.markup",
			"*.*.*.*.*"
			
		};
		
		std::vector<selector_container_t> vec;
		iterate(s, matchingSelectors)
			vec.push_back(*s);
		auto list = scope::compile::compile(vec);
		//TS_ASSERT(list);
	}
};