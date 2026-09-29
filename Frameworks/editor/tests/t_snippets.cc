#include <editor/src/editor.h>

void test_repopulating_mirrors ()
{
	static std::string const plistSrc = "{ content = \"${1/.+/-/}${1:x}${1/.+/-/}\"; }";

	ng::buffer_t buf;
	ng::editor_t editor(buf);

	editor.snippet_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>());
	OAK_ASSERT_EQ(editor.as_string(), "-x-");
	OAK_ASSERT_EQ(to_s(editor.ranges()), "[1-2]");

	editor.perform(ng::kDeleteSelection);
	OAK_ASSERT_EQ(editor.as_string(), "");
	OAK_ASSERT_EQ(to_s(editor.ranges()), "[0]");

	editor.insert("y");
	OAK_ASSERT_EQ(editor.as_string(), "-y-");
	OAK_ASSERT_EQ(to_s(editor.ranges()), "[2]");
}
