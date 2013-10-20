#include <command/runner.h>
#include <command/parser.h>
#include <settings/settings.h>
#include <cf/cf.h>
#include <OakSystem/application.h>

CXXTEST_ENUM_TRAITS(output::type,
	CXXTEST_ENUM_MEMBER(output::replace_input);
	CXXTEST_ENUM_MEMBER(output::replace_document);
	CXXTEST_ENUM_MEMBER(output::at_caret);
	CXXTEST_ENUM_MEMBER(output::after_input);
	CXXTEST_ENUM_MEMBER(output::new_window);
	CXXTEST_ENUM_MEMBER(output::tool_tip);
	CXXTEST_ENUM_MEMBER(output::discard);
	CXXTEST_ENUM_MEMBER(output::replace_selection);
);

struct delegate_t : command::delegate_t
{
	std::string out, err, html;
	int rc;
	output::type placement;
	output_format::type format;
	output_caret::type caret;

	delegate_t () : out(""), err(""), html(""), rc(0), placement(output::discard) { }

	ng::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
	{
		close(fd);
		return ng::range_t();
	}

	bool accept_html_data (command::runner_ptr runner, char const* data, size_t len)
	{
		return html.insert(html.end(), data, data + len), true;
	}

	bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::range_t inputRange, std::map<std::string, std::string> const& environment)
	{
		this->out       = out;
		this->placement = placement;
		this->format    = format;
		this->caret     = outputCaret;

		fprintf(stderr, "output: ‘%s’\n", out.c_str());
		return true;
	}

	void show_document (std::string const& str)
	{
		out       = str;
		placement = output::new_window;
	}

	void show_tool_tip (std::string const& str)
	{
		out       = str;
		placement = output::tool_tip;
	}

	void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err)
	{
		this->out = out;
		this->err = err;
		this->rc  = rc;
	}

	void detach () { }
	void done ()   { }
};

typedef std::shared_ptr<delegate_t> delegate_ptr;

class run_tests : public CxxTest::TestSuite
{
public:
	delegate_ptr run_command (std::string const& cmd, std::string const& output = "discard")
	{
		static std::string const bashInit =
			"exit_discard ()               { echo -n \"$1\"; exit 200; }\n"
			"exit_replace_text ()          { echo -n \"$1\"; exit 201; }\n"
			"exit_replace_document ()      { echo -n \"$1\"; exit 202; }\n"
			"exit_insert_text ()           { echo -n \"$1\"; exit 203; }\n"
			"exit_insert_snippet ()        { echo -n \"$1\"; exit 204; }\n"
			"exit_show_html ()             { echo -n \"$1\"; exit 205; }\n"
			"exit_show_tool_tip ()         { echo -n \"$1\"; exit 206; }\n"
			"exit_create_new_document ()   { echo -n \"$1\"; exit 207; }\n"
		;

		plist::dictionary_t plist;
		plist["command"] = cmd.find("#!") == 0 ? cmd : bashInit + cmd;
		plist["name"]    = std::string("Test Command");
		plist["input"]   = std::string("none");
		plist["output"]  = output;

		delegate_ptr delegate(new delegate_t);
		command::runner_ptr runner = command::runner(parse_command(convert_command_from_v1(plist)), ng::buffer_t(), ng::ranges_t(), variables_for_path(oak::basic_environment()), delegate);
		runner->launch();
		runner->wait(true);
		return delegate;
	}

public:
	void test_tool_tip ()
	{
		delegate_ptr res = run_command("exit_show_tool_tip 'Hello'");
		TS_ASSERT_EQUALS(res->placement, output::tool_tip);
		TS_ASSERT_EQUALS(res->out, "Hello");
		TS_ASSERT_EQUALS(res->err, "");
		TS_ASSERT_EQUALS(res->rc, 0);
	}

	void test_new_document ()
	{
		delegate_ptr res = run_command("exit_create_new_document 'Hello'");
		TS_ASSERT_EQUALS(res->placement, output::new_window);
		TS_ASSERT_EQUALS(res->out, "Hello");
		TS_ASSERT_EQUALS(res->err, "");
		TS_ASSERT_EQUALS(res->rc, 0);
	}

	void test_html_success ()
	{
		delegate_ptr res = run_command("echo >&2 Error && echo Hello && true", "showAsHTML");
		TS_ASSERT_EQUALS(res->html, "Hello\nError\n");
		TS_ASSERT_EQUALS(res->out, "");
		TS_ASSERT_EQUALS(res->err, "");
		TS_ASSERT_EQUALS(res->rc, 0);
	}

	void test_html_error ()
	{
		delegate_ptr res = run_command("echo >&2 Error && echo Hello && exit 1", "showAsHTML");
		TS_ASSERT_EQUALS(res->html, "Hello\n");
		TS_ASSERT_EQUALS(res->out, "");
		TS_ASSERT_EQUALS(res->err, "Error\n");
		TS_ASSERT_EQUALS(res->rc, 1);
	}
};
