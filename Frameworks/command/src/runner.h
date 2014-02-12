#ifndef COMMAND_RUNNER_H_MW7OSOTP
#define COMMAND_RUNNER_H_MW7OSOTP

#include "parser.h"
#include "process.h"
#include "reader.h"
#include <buffer/buffer.h>
#include <selection/selection.h>
#include <text/types.h>
#include <oak/debug.h>
#include <oak/callbacks.h>

namespace command
{
	PUBLIC void fix_shebang (std::string* command);

	struct delegate_t;
	struct runner_t;

	typedef std::shared_ptr<delegate_t>     delegate_ptr;
	typedef std::shared_ptr<runner_t>       runner_ptr;

	struct PUBLIC delegate_t : std::enable_shared_from_this<delegate_t>
	{
		virtual ~delegate_t () { }

		virtual ng::range_t write_unit_to_fd (int fd, input::type unit, input::type fallbackUnit, input_format::type format, scope::selector_t const& scopeSelector, std::map<std::string, std::string>& variables, bool* inputWasSelection) = 0;

		virtual bool accept_html_data (runner_ptr runner, char const* data, size_t len) { return true; }
		virtual void discard_html () { }
		virtual bool accept_result (std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, ng::range_t inputRange, std::map<std::string, std::string> const& environment) = 0;

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

	PUBLIC runner_ptr runner (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, delegate_ptr delegate, std::string const& pwd = NULL_STR);

	struct PUBLIC runner_t : std::enable_shared_from_this<runner_t>
	{
		runner_t () = delete;
		runner_t (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, std::string const& pwd, delegate_ptr delegate);

		void launch ();
		void wait (bool alsoForDetached = false);

		void add_callback (callback_t* callback)    { _callbacks.add(callback); }
		void remove_callback (callback_t* callback) { _callbacks.remove(callback); }

		std::string const& name () const                               { return _command.name; }
		oak::uuid_t const& uuid () const                               { return _command.uuid; }
		bool auto_scroll_output () const                               { return _command.auto_scroll_output; }
		bool running () const                                          { return _process.is_running; }
		pid_t process_id () const                                      { return _process.process_id; }
		std::map<std::string, std::string> const& environment () const { return _environment; }

	private:
		struct my_process_t : process_t
		{
			WATCH_LEAKS(my_process_t);

			my_process_t (runner_t* callback) : _callback(callback) { }
			void did_exit (int rc);
		private:
			runner_t* _callback;
		};

		struct my_reader_t : reader_t
		{
			WATCH_LEAKS(my_reader_t);

			my_reader_t (int fd, runner_ptr callback, bool is_error) : reader_t(fd), _callback(callback), _is_error(is_error) { }
			void receive_data (char const* bytes, size_t len);
		private:
			runner_ptr _callback;
			bool _is_error;
		};

		typedef std::shared_ptr<my_reader_t> my_reader_ptr;

		friend struct my_reader_t;
		void receive_data (char const* bytes, size_t len, bool is_error);

		friend struct my_process_t;
		void did_exit (int rc);

		void send_html_data (char const* bytes, size_t len);
		void show_document ();
		void release ();
		void finish ();

		bundle_command_t _command;
		std::map<std::string, std::string> _environment;
		std::string _directory;
		delegate_ptr _delegate;

		ng::range_t _input_range;     // used when output replaces input
		bool _input_was_selection;    // used with ‘exit_insert_snippet’ and when ‘output_caret == heuristic’
		bool _output_is_html;
		bool _did_send_html = false;
		bool _did_detach;
		size_t _retain_count;

		my_process_t _process;
		my_reader_ptr _output_reader;
		my_reader_ptr _error_reader;

		std::string _out, _err;
		int _return_code;
		bool _user_abort = false;

		oak::callbacks_t<callback_t> _callbacks;
	};

} /* command */

#endif /* end of include guard: COMMAND_RUNNER_H_MW7OSOTP */
