#include <file/type.h>
#include <plist/plist.h>
#include <test/jail.h>

void test_file_type ()
{
	test::jail_t jail;

	std::string const xmlPlist = jail.path("xml.plist");
	OAK_ASSERT(plist::save(xmlPlist, true, plist::kPlistFormatXML));
	OAK_ASSERT_EQ(file::type(xmlPlist, io::bytes_ptr(new io::bytes_t(path::content(xmlPlist)))), "source.xml.plist");
	OAK_ASSERT_EQ(file::type(jail.path("ascii.plist"), io::bytes_ptr(new io::bytes_t("{ foo = 'bar'; }"))), "source.plist");
}
