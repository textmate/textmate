#include <command/src/runner.h>
#include <command/src/parser.h>
#include <settings/src/settings.h>
#include <cf/src/cf.h>
#include <OakSystem/src/application.h>

static std::string as_str (output::type output)
{
	switch(output)
	{
		case output::replace_input:     return "replace_input";
		case output::replace_document:  return "replace_document";
		case output::at_caret:          return "at_caret";
		case output::after_input:       return "after_input";
		case output::new_window:        return "new_window";
		case output::tool_tip:          return "tool_tip";
		case output::discard:           return "discard";
		case output::replace_selection: return "replace_selection";
	}
}

struct delegate_t : command::delegate_t
{
	std::string out, err, html;
	int rc;
	std::string placement;
	output_format::type format;
	output_caret::type caret;

	delegate_t () : out(""), err(""), html(""), rc(-42), placement(as_str(output::discard)) { }

	ng::ranges_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection)
	{
		close(fd);
		return { };
	}

	bool accept_html_data (command::runner_ptr runner, char const* data, size_t len)
	{
		rc = 0;
		return html.insert(html.end(), data, data + len), true;
	}

	bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> const& environment)
	{
		this->out       = out;
		this->placement = as_str(placement);
		this->format    = format;
		this->caret     = outputCaret;
		this->rc        = 0;

		fprintf(stderr, "output: ‘%s’\n", out.c_str());
		return true;
	}

	void show_document (std::string const& str)
	{
		out       = str;
		placement = as_str(output::new_window);
		rc        = 0;
	}

	void show_tool_tip (std::string const& str)
	{
		out       = str;
		placement = as_str(output::tool_tip);
		rc        = 0;
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

static delegate_ptr run_command (std::string const& cmd, std::string const& output = "discard")
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
	runner->wait_for_command();
	return delegate;
}

void test_tool_tip ()
{
	delegate_ptr res = run_command("exit_show_tool_tip 'Hello'");
	OAK_ASSERT_EQ(res->placement, as_str(output::tool_tip));
	OAK_ASSERT_EQ(res->out, "Hello");
	OAK_ASSERT_EQ(res->err, "");
	OAK_ASSERT_EQ(res->rc, 0);
}

void test_new_document ()
{
	delegate_ptr res = run_command("exit_create_new_document 'Hello'");
	OAK_ASSERT_EQ(res->placement, as_str(output::new_window));
	OAK_ASSERT_EQ(res->out, "Hello");
	OAK_ASSERT_EQ(res->err, "");
	OAK_ASSERT_EQ(res->rc, 0);
}

void test_html_success ()
{
	delegate_ptr res = run_command("echo >&2 Error && echo Hello && true", "showAsHTML");
	OAK_ASSERT_EQ(res->html, "Hello\nError\n");
	OAK_ASSERT_EQ(res->out, "");
	OAK_ASSERT_EQ(res->err, "");
	OAK_ASSERT_EQ(res->rc, 0);
}

void test_html_error ()
{
	delegate_ptr res = run_command("echo >&2 Error && echo Hello && exit 1", "showAsHTML");
	OAK_ASSERT_EQ(res->html, "Hello\n");
	OAK_ASSERT_EQ(res->out, "");
	OAK_ASSERT_EQ(res->err, "Error\n");
	OAK_ASSERT_EQ(res->rc, 1);
}
