#ifndef COMMAND_RUNNER_H_MW7OSOTP
#define COMMAND_RUNNER_H_MW7OSOTP

#include "parser.h"
#include <buffer/src/buffer.h>
#include <selection/src/selection.h>
#include <text/src/types.h>
#include <oak/debug.h>
#include <oak/callbacks.h>

namespace command
{
	void fix_shebang (std::string* command);
	std::string create_script_path (std::string const& command);

	struct delegate_t;
	struct runner_t;

	typedef std::shared_ptr<delegate_t>     delegate_ptr;
	typedef std::shared_ptr<runner_t>       runner_ptr;

	struct delegate_t : std::enable_shared_from_this<delegate_t>
	{
		virtual ~delegate_t () { }

		virtual ng::ranges_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) = 0;

		virtual bool accept_html_data (runner_ptr runner, char const* data, size_t len) { return true; }
		virtual void discard_html () { }
		virtual bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::ranges_t const& inputRanges, std::map<std::string, std::string> const& environment) = 0;

		virtual void show_document (std::string const& str) = 0;
		virtual void show_tool_tip (std::string const& str) = 0;
		virtual void show_error (bundle_command_t const& command, int rc, std::string const& out, std::string const& err) = 0;
		virtual void detach ()     { }
		virtual void done ()       { }
	};

	struct callback_t
	{
		virtual ~callback_t ()                                                { }
		virtual void output (runner_ptr runner, char const* data, size_t len) { }
		virtual void done (runner_ptr runner)                                 { }
	};

	runner_ptr runner (bundle_command_t const& command, ng::buffer_api_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, delegate_ptr delegate, std::string const& pwd = NULL_STR);

	struct runner_t : std::enable_shared_from_this<runner_t>
	{
		runner_t () = delete;
		runner_t (bundle_command_t const& command, ng::buffer_api_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, std::string const& pwd, delegate_ptr delegate);

		void launch ();
		void wait ();
		void wait_for_command ();

		void add_callback (callback_t* callback)    { _callbacks.add(callback); }
		void remove_callback (callback_t* callback) { _callbacks.remove(callback); }

		std::string const& name () const                               { return _command.name; }
		oak::uuid_t const& uuid () const                               { return _command.uuid; }
		bool auto_scroll_output () const                               { return _command.auto_scroll_output; }
		output_reuse::type output_reuse () const                       { return _command.output_reuse; }
		bool running () const                                          { return _process_id != -1; }
		pid_t process_id () const                                      { return _process_id; }
		std::map<std::string, std::string> const& environment () const { return _environment; }

	private:
		void did_exit (int status);
		void send_html_data (char const* bytes, size_t len);
		void show_document ();

		bundle_command_t _command;
		std::map<std::string, std::string> _environment;
		std::string _directory;
		delegate_ptr _delegate;

		ng::ranges_t _input_ranges;   // used when output replaces input
		bool _input_was_selection;    // used with ‘exit_insert_snippet’ and when ‘output_caret == heuristic’
		bool _did_send_html = false;
		bool _did_detach;

		pid_t _process_id = -1;
		dispatch_group_t _dispatch_group;
		std::string _temp_path = NULL_STR;

		std::string _out, _err;
		bool _user_abort = false;

		oak::callbacks_t<callback_t> _callbacks;
	};

} /* command */

#endif /* end of include guard: COMMAND_RUNNER_H_MW7OSOTP */
