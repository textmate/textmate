#include <test/jail.h>
#include <document/document.h>

void test_symlinks ()
{
	test::jail_t jail;

	jail.touch("test.txt");
	jail.ln("link.txt", "test.txt");

	OAK_ASSERT_EQ(path::exists(jail.path("test.txt")), true);
	OAK_ASSERT_EQ(path::exists(jail.path("link.txt")), true);

	document::document_ptr srcDoc  = document::create(jail.path("test.txt"));
	document::document_ptr linkDoc = document::create(jail.path("link.txt"));
	OAK_ASSERT_EQ(srcDoc->identifier(), linkDoc->identifier());

	document::document_ptr aliasDoc = document::create(jail.path("./test.txt"));
	OAK_ASSERT_EQ(srcDoc->identifier(), aliasDoc->identifier());
}
