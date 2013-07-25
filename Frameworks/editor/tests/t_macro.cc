#include <editor/editor.h>

void test_insert ()
{
	static std::string const plistSrc = "{ commands = ("
		"{ command = 'insertText:'; argument = 'T'; },"
		"{ command = 'insertText:'; argument = 'e'; },"
		"{ command = 'insertText:'; argument = 's'; },"
		"{ command = 'insertText:'; argument = 't'; },"
	"); }";

	ng::buffer_t buf;
	ng::editor_t editor(buf);
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>());
	OAK_ASSERT_EQ(editor.as_string(), "Test");
}

void test_snippet ()
{
	static std::string const plistSrc = "{ commands = ("
		"{ command = 'insertText:'; argument = 'i'; },"
		"{ command = 'insertText:'; argument = 's'; },"
		"{ command = 'insertText:'; argument = 'o'; },"
		"{ command = 'insertText:'; argument = 'D'; },"
		"{ command = 'deleteTabTrigger:'; argument = 'isoD'; },"
		"{ command = 'insertSnippetWithOptions:';"
		"  argument = {"
		"    name    = 'Current Date — YYYY-MM-DD';"
		"    content = '`#!/bin/sh\ndate +2010-01-25`';"
		"    uuid    = 'C9CAF012-6E50-11D9-AA12-000D93589AF6';"
		"  };"
		"},"
	"); }";

	ng::buffer_t buf;
	ng::editor_t editor(buf);
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>());
	OAK_ASSERT_EQ(editor.as_string(), "2010-01-25");
}

void test_command ()
{
	static std::string const plistSrc = "{ commands = ("
		"{ command = 'selectAll:'; },"
		"{ command = 'executeCommandWithOptions:';"
		"  argument = {"
		"    command = '#!/bin/sh\necho Hello';"
		"    input = 'document';"
		"    output = 'insertAsSnippet';"
		"  };"
		"},"
	"); }";

	ng::buffer_t buf;
	ng::editor_t editor(buf);
	editor.insert("to be replaced");
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>());
	// OAK_ASSERT_EQ(editor.as_string(), "Hello\n");
}
