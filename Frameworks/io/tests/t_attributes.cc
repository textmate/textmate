#include <io/path.h>
#include <test/jail.h>

class AttributesTests : public CxxTest::TestSuite
{
public:
	void test_attributes ()
	{
		test::jail_t jail;
		std::string const file = jail.path("dummy");

		path::set_content(file, "«some content»");

		path::set_attr(file, "foo", "bar");
		path::set_attr(file, "buz", "jazz");

		TS_ASSERT_EQUALS(path::get_attr(file, "foo"), "bar");
		TS_ASSERT_EQUALS(path::get_attr(file, "buz"), "jazz");

		std::map<std::string, std::string> newAttrs;
		newAttrs.insert(std::make_pair("new", "value"));
		newAttrs.insert(std::make_pair("foo", NULL_STR));
		path::set_attributes(file, newAttrs);

		TS_ASSERT_EQUALS(path::get_attr(file, "foo"), NULL_STR);
		TS_ASSERT_EQUALS(path::get_attr(file, "buz"), "jazz");
		TS_ASSERT_EQUALS(path::get_attr(file, "new"), "value");

		TS_ASSERT_EQUALS(path::attributes(file).size(), 2);

		TS_ASSERT_EQUALS(path::content(file), "«some content»");
	}
};
