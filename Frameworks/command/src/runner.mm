#include "runner.h"
#include <OakSystem/application.h>
#include <io/path.h>
#include <regexp/format_string.h>

OAK_DEBUG_VAR(Command_Runner);

static std::string trim_right (std::string const& str, std::string const& trimChars = " \t\n")
{
	std::string::size_type len = str.find_last_not_of(trimChars);
	return len == std::string::npos ? "" : str.substr(0, len+1);
}

namespace command
{
	void fix_shebang (std::string* command)
	{
		if(command->substr(0, 2) != "#!")
			command->insert(0, "#!/usr/bin/env bash\n[[ -f \"${TM_SUPPORT_PATH}/lib/bash_init.sh\" ]] && . \"${TM_SUPPORT_PATH}/lib/bash_init.sh\"\n\n");
	}

	void runner_t::my_process_t::did_exit (int rc)
	{
		process_t::did_exit(rc);
		_callback->did_exit(rc);
	}

	void runner_t::my_reader_t::receive_data (char const* bytes, size_t len)
	{
		_callback->receive_data(bytes, len, _is_error);
	}

	// ==================
	// = Command Runner =
	// ==================

	runner_t::runner_t (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, std::string const& pwd, delegate_ptr delegate) : _command(command), _environment(environment), _directory(pwd), _delegate(delegate), _input_was_selection(false), _output_is_html(false), _did_detach(false), _retain_count(3), _process(this), _return_code(-2)
	{
		fix_shebang(&_command.command);
	}

	runner_ptr runner (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, delegate_ptr delegate, std::string const& pwd)
	{
		return std::make_shared<runner_t>(command, buffer, selection, environment, pwd, delegate);
	}

	void runner_t::release ()
	{
		D(DBF_Command_Runner, bug("retain count %zu\n", _retain_count););
		if(--_retain_count == 0)
		{
			finish();
			_output_reader = my_reader_ptr();
			_error_reader  = my_reader_ptr();
		}
	}

	void runner_t::launch ()
	{
		ASSERT(_delegate);

		int inputPipe[2];
		pipe(inputPipe);
		fcntl(inputPipe[1], F_SETFD, FD_CLOEXEC);
		_input_range = _command.input == input::nothing ? (close(inputPipe[1]), ng::range_t()) : _delegate->write_unit_to_fd(inputPipe[1], _command.input, _command.input_fallback, _command.input_format, _command.scope_selector, _environment, &_input_was_selection);

		// ----------8<----------

		_process.command     = _command.command;
		_process.input_fd    = inputPipe[0];
		_process.environment = _environment;
		_process.directory   = _directory;

		_process.launch();

		_output_reader = std::make_shared<my_reader_t>(_process.output_fd, shared_from_this(), false);
		_error_reader  = std::make_shared<my_reader_t>(_process.error_fd, shared_from_this(), true);

		if(_command.output == output::new_window && _command.output_format == output_format::html)
		{
			_output_is_html = true;
			_delegate->detach();
			_did_detach = true;
		}
	}

	void runner_t::send_html_data (char const* bytes, size_t len)
	{
		_did_send_html = true;
		_delegate->accept_html_data(shared_from_this(), bytes, len);
		_callbacks(&callback_t::output, shared_from_this(), bytes, len);
	}

	void runner_t::receive_data (char const* bytes, size_t len, bool is_error)
	{
		D(DBF_Command_Runner, bug("%zu bytes (error: %s)\n", len, BSTR(is_error)););
		if(len == 0)
			return release();

		if(is_error)
			_err.insert(_err.end(), bytes, bytes + len);
		else if(_output_is_html)
			send_html_data(bytes, len);
		else
			_out.insert(_out.end(), bytes, bytes + len);
	}

	void runner_t::wait (bool alsoForDetached)
	{
		while(_retain_count != 0 && (alsoForDetached || !_did_detach))
		{
			if(CFRunLoopRunInMode(kCFRunLoopDefaultMode, 20, true) == kCFRunLoopRunFinished)
				break;
		}
	}

	void runner_t::did_exit (int rc)
	{
		D(DBF_Command_Runner, bug("%d\n", rc););
		_return_code = rc;
		release();
	}

	void runner_t::finish ()
	{
		std::string newOut, newErr;
		oak::replace_copy(_out.begin(), _out.end(), _process.temp_path, _process.temp_path + strlen(_process.temp_path), _command.name.begin(), _command.name.end(), back_inserter(newOut));
		oak::replace_copy(_err.begin(), _err.end(), _process.temp_path, _process.temp_path + strlen(_process.temp_path), _command.name.begin(), _command.name.end(), back_inserter(newErr));
		newOut.swap(_out);
		newErr.swap(_err);

		output::type placement         = _command.output;
		output_format::type format     = _command.output_format;
		output_caret::type outputCaret = _command.output_caret;

		enum { exit_discard = 200, exit_replace_text, exit_replace_document, exit_insert_text, exit_insert_snippet, exit_show_html, exit_show_tool_tip, exit_create_new_document };
		switch(_return_code)
		{
			case exit_discard:             placement = output::discard;                                            break;
			case exit_replace_text:        placement = output::replace_input;     format = output_format::text;    outputCaret = output_caret::heuristic;             break;
			case exit_replace_document:    placement = output::replace_document;  format = output_format::text;    outputCaret = output_caret::interpolate_by_line;   break;
			case exit_insert_text:         placement = output::after_input;       format = output_format::text;    outputCaret = output_caret::after_output;          break;
			case exit_insert_snippet:      placement = _command.input == input::selection ? output::replace_input : output::after_input; format = output_format::snippet; break;
			case exit_show_html:           placement = output::new_window;        format = output_format::html;    break;
			case exit_show_tool_tip:       placement = output::tool_tip;          format = output_format::text;    break;
			case exit_create_new_document: placement = output::new_window;        format = output_format::text;    break;
		}

		D(DBF_Command_Runner, bug("placement %d, format %d\n", placement, format););
		if(_return_code != 0 && !(200 <= _return_code && _return_code <= 207))
		{
			_delegate->show_error(_command, _return_code, _out, _err);
		}
		else if(placement == output::new_window)
		{
			if(format == output_format::text)
			{
				_delegate->show_document(_err + _out);
			}
			else if(format == output_format::html)
			{
				if(!_err.empty())
					send_html_data(_err.data(), _err.size());

				if(!_output_is_html)
				{
					_output_is_html = true;
					send_html_data(_out.data(), _out.size());
				}
			}
		}
		else if(placement == output::tool_tip)
		{
			std::string str = trim_right(_err + _out);
			if(!str.empty())
				_delegate->show_tool_tip(str);
		}
		else if(placement != output::discard)
		{
			if(outputCaret == output_caret::heuristic)
			{
				if(_command.input == input::selection && _input_was_selection)
					outputCaret = output_caret::select_output;
				else if(_command.input == input::selection && (_command.input_fallback == input::line || _command.input_fallback == input::word || _command.input_fallback == input::scope))
					outputCaret = output_caret::interpolate_by_char;
				else if(_command.input == input::selection && (_command.input_fallback == input::entire_document))
					outputCaret = output_caret::interpolate_by_line;
				else
					outputCaret = output_caret::after_output;
			}

			if(!_input_range)
			{
				switch(placement)
				{
					case output::after_input:   placement = output::at_caret;          break;
					case output::replace_input: placement = output::replace_selection; break;
				}
			}

			if(format == output_format::snippet && _command.disable_output_auto_indent)
				format = output_format::snippet_no_auto_indent;

			_delegate->accept_result(_out, placement, format, outputCaret, _input_range, _environment);
		}
		else if(_did_send_html)
		{
			_delegate->discard_html();
		}

		_delegate->done();
		_delegate.reset();

		_callbacks(&callback_t::done, shared_from_this());
	}

} /* command */
