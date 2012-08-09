#include <file/type.h>
#include <plist/plist.h>
#include <test/jail.h>

class TypeTests : public CxxTest::TestSuite
{
public:
	void test_file_type ()
	{
		test::jail_t jail;

		std::string const xmlPlist = jail.path("xml.plist");
		TS_ASSERT(plist::save(xmlPlist, true, plist::kPlistFormatXML));
		TS_ASSERT_EQUALS(file::type(xmlPlist, io::bytes_ptr(new io::bytes_t(path::content(xmlPlist)))), "source.xml.plist");
		TS_ASSERT_EQUALS(file::type(jail.path("ascii.plist"), io::bytes_ptr(new io::bytes_t("{ foo = 'bar'; }"))), "source.plist");
	}
};

