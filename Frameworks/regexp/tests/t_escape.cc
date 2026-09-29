#include <regexp/src/regexp.h>

void test_escape ()
{
	OAK_ASSERT_EQ(regexp::escape("char buf[32]"), "char buf\\[32\\]");
	OAK_ASSERT_EQ(regexp::escape("[obj method]"), "\\[obj method\\]");
	OAK_ASSERT_EQ(regexp::escape("foo->bar()"),   "foo->bar\\(\\)");
	OAK_ASSERT_EQ(regexp::escape("obj.property"), "obj\\.property");
	OAK_ASSERT_EQ(regexp::escape("// comment"),   "// comment");
}
