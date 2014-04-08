#include <document/document.h>
#include <test/jail.h>

void test_save ()
{
	test::jail_t jail;

	document::document_ptr doc = document::from_content("Æblegrød");
	doc->set_path(jail.path("test.txt"));
	doc->sync_save();
	OAK_ASSERT_EQ(path::content(jail.path("test.txt")), "Æblegrød");
}
