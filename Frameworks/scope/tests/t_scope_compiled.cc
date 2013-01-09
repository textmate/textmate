#include <scope/compile.h>

class CompiledScopeTests : public CxxTest::TestSuite
{
public:
	void test_interim ()
	{
		scope::compile::interim_t interim;
		TS_ASSERT(!interim.has_any());
	}
	
	struct selector_container_t
	{
		scope::selector_t scope_selector;
	public:
		selector_container_t(scope::selector_t s) : scope_selector(s) {}
	};

	void test_compiler_generation ()
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
		scope::compile::compile(vec);
		//TS_ASSERT(list);
	}
};
