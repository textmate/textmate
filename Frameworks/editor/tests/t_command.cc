#include <editor/editor.h>

class CommandTests : public CxxTest::TestSuite
{
public:
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
		// TS_ASSERT_EQUALS(editor.as_string(), "Hello\n");
	}
};
