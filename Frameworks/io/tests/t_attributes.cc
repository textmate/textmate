#include <io/src/path.h>
#include <test/jail.h>

void test_attributes ()
{
	test::jail_t jail;
	std::string const file = jail.path("dummy");

	path::set_content(file, "«some content»");

	path::set_attr(file, "foo", "bar");
	path::set_attr(file, "buz", "jazz");

	OAK_ASSERT_EQ(path::get_attr(file, "foo"), "bar");
	OAK_ASSERT_EQ(path::get_attr(file, "buz"), "jazz");

	std::map<std::string, std::string> newAttrs;
	newAttrs.insert(std::make_pair("new", "value"));
	newAttrs.insert(std::make_pair("foo", NULL_STR));
	path::set_attributes(file, newAttrs);

	OAK_ASSERT_EQ(path::get_attr(file, "foo"), NULL_STR);
	OAK_ASSERT_EQ(path::get_attr(file, "buz"), "jazz");
	OAK_ASSERT_EQ(path::get_attr(file, "new"), "value");

	OAK_ASSERT_EQ(path::attributes(file).size(), 2);

	OAK_ASSERT_EQ(path::content(file), "«some content»");
}
