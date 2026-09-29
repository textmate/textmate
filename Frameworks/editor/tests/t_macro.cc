#include <editor/src/editor.h>

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
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>(), [](bundle_command_t const& cmd, ng::buffer_api_t const& buf, ng::ranges_t const& sel, std::map<std::string, std::string> const& env){
		OAK_ASSERT(false); // Macro does not contain any commands
	});
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
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>(), [](bundle_command_t const& cmd, ng::buffer_api_t const& buf, ng::ranges_t const& sel, std::map<std::string, std::string> const& env){
		OAK_ASSERT(false); // Macro does not contain any commands
	});
	OAK_ASSERT_EQ(editor.as_string(), "2010-01-25");
}

void test_command ()
{
	struct delegate_t : command::delegate_t
	{
		delegate_t (ng::editor_t& editor) : _editor(editor) { }

		ng::ranges_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) { close(fd); return { }; }

		void show_document (std::string const& str) { }
		void show_tool_tip (std::string const& str) { }
		void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) { }

		bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> const& environment)
		{
			return _editor.handle_result(out, placement, format, outputCaret, inputRanges, environment);
		}

	private:
		ng::editor_t& _editor;
	};

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
	editor.macro_dispatch(boost::get<plist::dictionary_t>(plist::parse(plistSrc)), std::map<std::string, std::string>(), [&editor](bundle_command_t const& cmd, ng::buffer_api_t const& buf, ng::ranges_t const& sel, std::map<std::string, std::string> const& env){
		command::runner_ptr runner = command::runner(cmd, buf, sel, env, std::make_shared<delegate_t>(editor));
		runner->launch();
		runner->wait_for_command();
	});
	OAK_ASSERT_EQ(editor.as_string(), "Hello\n");
}
