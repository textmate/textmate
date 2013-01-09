#include <scope/compile.h>

class CompiledScopeTests : public CxxTest::TestSuite
{
public:
	void test_interim ()
	{
		scope::compile::interim_t interim;
		TS_ASSERT(!interim.has_any());
	}

};
