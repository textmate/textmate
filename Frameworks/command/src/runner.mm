#include "runner.h"
#include <OakSystem/application.h>
#include <OakSystem/process.h>
#include <io/path.h>
#include <regexp/format_string.h>
#include <oak/datatypes.h>

OAK_DEBUG_VAR(Command_Runner);

static std::string trim_right (std::string const& str, std::string const& trimChars = " \t\n")
{
	std::string::size_type len = str.find_last_not_of(trimChars);
	return len == std::string::npos ? "" : str.substr(0, len+1);
}

static std::tuple<pid_t, int, int> my_fork (char* cmd, int stdinFd, std::map<std::string, std::string> const& environment, std::string const& cwd)
{
	for(auto const& pair : environment)
	{
		if(pair.first.size() + pair.second.size() + 2 >= ARG_MAX)
			fprintf(stderr, "*** variable exceeds ARG_MAX: %s\n", pair.first.c_str());
	}

	int outputPipe[2], errorPipe[2];
	pipe(outputPipe);
	pipe(errorPipe);
	fcntl(outputPipe[0], F_SETFD, FD_CLOEXEC);
	fcntl(errorPipe[0],  F_SETFD, FD_CLOEXEC);

	char const* workingDir = cwd.c_str();
	oak::c_array env(environment);

	pid_t pid = vfork();
	if(pid == 0)
	{
		int const signals[] = { SIGINT, SIGTERM, SIGPIPE, SIGUSR1 };
		for(int sig : signals) signal(sig, SIG_DFL);

		int const oldOutErr[] = { 0, 1, 2 };
		int const newOutErr[] = { stdinFd, outputPipe[1], errorPipe[1] };
		for(int fd : oldOutErr) close(fd);
		for(int fd : newOutErr) dup(fd);
		for(int fd : newOutErr) close(fd);

		setpgid(0, getpid());
		chdir(workingDir);

		char* argv[] = { cmd, NULL };
		execve(argv[0], argv, env);
		perror("interpreter failed");
		_exit(0);
	}

	int const fds[] = { stdinFd, outputPipe[1], errorPipe[1] };
	for(int fd : fds) close(fd);

	return std::make_tuple(pid, outputPipe[0], errorPipe[0]);
}

static void exhaust_fd_in_queue (dispatch_group_t group, int fd, void(^handler)(char const* bytes, size_t len))
{
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		char buf[8192];
		ssize_t len = 0;
		while((len = read(fd, buf, sizeof(buf))) > 0)
		{
			char const* bytes = buf;
			dispatch_sync(dispatch_get_main_queue(), ^{
				handler(bytes, len);
			});
		}
		if(len == -1)
			perror("read");
		close(fd);
	});
}

static pid_t run_command (char* cmd, int stdinFd, std::map<std::string, std::string> const& env, std::string const& cwd, void(^stdoutHandler)(char const* bytes, size_t len), void(^stderrHandler)(char const* bytes, size_t len), void(^completionHandler)(int status))
{
	pid_t pid;
	int outputFd, errorFd;
	std::tie(pid, outputFd, errorFd) = my_fork(cmd, stdinFd, env, cwd);

	dispatch_group_t group = dispatch_group_create();

	exhaust_fd_in_queue(group, outputFd, stdoutHandler);
	exhaust_fd_in_queue(group, errorFd, stderrHandler);

	__block int status = 0;
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		if(waitpid(pid, &status, 0) != pid)
			perror("waitpid");
	});

	dispatch_group_notify(group, dispatch_get_main_queue(), ^{
		completionHandler(status);
	});

	dispatch_release(group);

	return pid;
}

namespace command
{
	void fix_shebang (std::string* command)
	{
		if(command->substr(0, 2) != "#!")
			command->insert(0, "#!/usr/bin/env bash\n[[ -f \"${TM_SUPPORT_PATH}/lib/bash_init.sh\" ]] && . \"${TM_SUPPORT_PATH}/lib/bash_init.sh\"\n\n");
	}

	// ==================
	// = Command Runner =
	// ==================

	runner_t::runner_t (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, std::string const& pwd, delegate_ptr delegate) : _command(command), _environment(environment), _directory(pwd), _delegate(delegate), _input_was_selection(false), _did_detach(false)
	{
		fix_shebang(&_command.command);
	}

	runner_ptr runner (bundle_command_t const& command, ng::buffer_t const& buffer, ng::ranges_t const& selection, std::map<std::string, std::string> const& environment, delegate_ptr delegate, std::string const& pwd)
	{
		return std::make_shared<runner_t>(command, buffer, selection, environment, pwd, delegate);
	}

	void runner_t::launch ()
	{
		ASSERT(_delegate);
		ASSERT(_command.command.find("#!") == 0);

		_temp_path = strdup(path::temp("command", _command.command).c_str());

		int inputPipe[2];
		pipe(inputPipe);
		fcntl(inputPipe[1], F_SETFD, FD_CLOEXEC);
		_input_range = _command.input == input::nothing ? (close(inputPipe[1]), ng::range_t()) : _delegate->write_unit_to_fd(inputPipe[1], _command.input, _command.input_fallback, _command.input_format, _command.scope_selector, _environment, &_input_was_selection);

		auto textOutHandler = ^(char const* bytes, size_t len) { _out.insert(_out.end(), bytes, bytes + len); };
		auto htmlOutHandler = ^(char const* bytes, size_t len) { send_html_data(bytes, len); };
		auto stderrHandler  = ^(char const* bytes, size_t len) { _err.insert(_err.end(), bytes, bytes + len); };

		runner_ptr runner = shared_from_this();
		auto completionHandler = ^(int rc) {
			runner->did_exit(rc);

			// Wake up our local event loop (if it is running)
			[NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:0 data1:0 data2:0] atStart:NO];
		};

		bool hasHTMLOutput = _command.output == output::new_window && _command.output_format == output_format::html;
		_process_id = run_command(_temp_path, inputPipe[0], _environment, _directory, hasHTMLOutput ? htmlOutHandler : textOutHandler, stderrHandler, completionHandler);

		if(hasHTMLOutput)
		{
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

	void runner_t::wait (bool alsoForDetached)
	{
		NSMutableArray* queuedEvents = [NSMutableArray array];
		while(_process_id != -1 && (alsoForDetached || !_did_detach))
		{
			if(NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES])
			{
				static NSEventType const events[] = { NSLeftMouseDown, NSLeftMouseUp, NSRightMouseDown, NSRightMouseUp, NSOtherMouseDown, NSOtherMouseUp, NSLeftMouseDragged, NSRightMouseDragged, NSOtherMouseDragged, NSKeyDown, NSKeyUp, NSFlagsChanged };
				if(!oak::contains(std::begin(events), std::end(events), [event type]))
				{
					[NSApp sendEvent:event];
				}
				else if([event type] == NSKeyDown && (([[event charactersIgnoringModifiers] isEqualToString:@"c"] && ([event modifierFlags] & (NSShiftKeyMask|NSControlKeyMask|NSAlternateKeyMask|NSCommandKeyMask)) == NSControlKeyMask) || ([[event charactersIgnoringModifiers] isEqualToString:@"."] && ([event modifierFlags] & (NSShiftKeyMask|NSControlKeyMask|NSAlternateKeyMask|NSCommandKeyMask)) == NSCommandKeyMask)))
				{
					NSInteger choice = NSRunAlertPanel(@"Stop Command Execution", @"Would you like to kill the current shell command?", @"Kill Command", @"Cancel", nil);
					if(choice == NSAlertDefaultReturn) // "Kill Command"
					{
						_user_abort = true;
						oak::kill_process_group_in_background(process_id());
					}
				}
				else
				{
					[queuedEvents addObject:event];
				}
			}
			else
			{
				break;
			}
		}

		for(NSEvent* event in queuedEvents)
			[NSApp postEvent:event atStart:NO];
	}

	void runner_t::did_exit (int status)
	{
		D(DBF_Command_Runner, bug("%d\n", status););
		if(WIFSIGNALED(status))
			fprintf(stderr, "*** process terminated: %s\n", strsignal(WTERMSIG(status)));
		else if(!WIFEXITED(status))
			fprintf(stderr, "*** process terminated abnormally %d\n", status);

		_process_id = -1;

		ASSERT(_temp_path);

		std::string newOut, newErr;
		oak::replace_copy(_out.begin(), _out.end(), _temp_path, _temp_path + strlen(_temp_path), _command.name.begin(), _command.name.end(), back_inserter(newOut));
		oak::replace_copy(_err.begin(), _err.end(), _temp_path, _temp_path + strlen(_temp_path), _command.name.begin(), _command.name.end(), back_inserter(newErr));
		newOut.swap(_out);
		newErr.swap(_err);

		unlink(_temp_path);
		free(_temp_path);
		_temp_path = nullptr;

		output::type placement         = _command.output;
		output_format::type format     = _command.output_format;
		output_caret::type outputCaret = _command.output_caret;

		int rc = WIFEXITED(status) ? WEXITSTATUS(status) : (WIFSIGNALED(status) ? 0 : -1);
		enum { exit_discard = 200, exit_replace_text, exit_replace_document, exit_insert_text, exit_insert_snippet, exit_show_html, exit_show_tool_tip, exit_create_new_document };
		switch(rc)
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
		if(rc != 0 && !_user_abort && !(200 <= rc && rc <= 207))
		{
			_delegate->show_error(_command, rc, _out, _err);
		}
		else if(placement == output::new_window)
		{
			if(format == output_format::text)
			{
				_delegate->show_document(_err + _out);
			}
			else if(format == output_format::html)
			{
				bool outputWasHTML = _command.output == output::new_window && _command.output_format == output_format::html;
				if(!outputWasHTML)
					send_html_data(_out.data(), _out.size());

				if(!_err.empty())
					send_html_data(_err.data(), _err.size());
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
