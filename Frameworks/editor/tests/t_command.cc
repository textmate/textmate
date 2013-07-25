#include <editor/editor.h>

void test_replace_selection_command ()
{
	static std::string const plistSrc = 
		"{ command = '#!/bin/sh\necho Hello';"
		"  input = 'document';"
		"  output = 'insertAsSnippet';"
		"}";

	ng::buffer_t buf;
	ng::editor_t editor(buf);
	editor.insert("to be replaced");
	editor.perform(ng::kSelectAll);
	editor.execute_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>());
	// OAK_ASSERT_EQ(editor.as_string(), "Hello\n");
}
