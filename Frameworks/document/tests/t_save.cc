#include <document/document.h>
#include <test/jail.h>

class SaveTests : public CxxTest::TestSuite
{
public:
	void test_save ()
	{
		test::jail_t jail;

		document::document_ptr doc = document::from_content("Æblegrød");
		doc->set_path(jail.path("test.txt"));
		doc->save();
		TS_ASSERT_EQUALS(path::content(jail.path("test.txt")), "Æblegrød");
	}
};
