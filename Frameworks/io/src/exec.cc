#include "exec.h"
#include "environment.h"
#include <text/format.h>
#include <oak/datatypes.h>
#include <oak/debug/OakDebugLog.h>

OAK_DEBUG_VAR(IO_Exec);

namespace io
{
	// takes NULL-terminated list of arguments
	static std::string vexec (std::map<std::string, std::string> const& environment, std::string const& cmd, va_list args)
	{
		std::string output;
		std::string error;

		std::vector<char*> command(1, (char*)cmd.c_str());
		char* arg = NULL;
		while((arg = va_arg(args, char*)) && *arg)
			command.push_back(arg);
		va_end(args);

		char* argv[command.size() + 1];
		std::copy(command.begin(), command.end(), &argv[0]);
		argv[command.size()] = NULL;

		int outputPipe[2], errorPipe[2];
		pipe(outputPipe);
		pipe(errorPipe);
		int output_fd = outputPipe[0], error_fd = errorPipe[0];

		std::map<std::string, std::string>::const_iterator it = environment.find("PWD");
		char const* pwdDir = it != environment.end() ? it->second.c_str() : NULL;

		oak::c_array env(environment);
		pid_t process_id = vfork();
		if(process_id == 0)
		{
			setpgid(0, getpid());

			int const oldOutErr[] = { 0, 1, 2, output_fd };
			std::for_each(beginof(oldOutErr), endof(oldOutErr), close);

			open("/dev/null", O_RDONLY); // stdin
			dup(outputPipe[1]);
			close(outputPipe[1]);
			dup(errorPipe[1]);
			close(errorPipe[1]);

			if(pwdDir)
				chdir(pwdDir);

			execve(argv[0], argv, env);
			perror("interpreter failed");
			_exit(0);
		}

		close(outputPipe[1]);
		close(errorPipe[1]);

		char buf[255];
		while(ssize_t len = read(output_fd, buf, sizeof(buf)))
		{
			if(len < 0)
				break;
			output.insert(output.end(), buf, buf + len);
		}
		close(output_fd);

		char error_buf[255];
		while(ssize_t len = read(error_fd, error_buf, sizeof(error_buf)))
		{
			if(len < 0)
				break;
			error.insert(error.end(), error_buf, error_buf + len);
		}
		close(error_fd);
		D(DBF_SCM_RunCommand, if(!error.empty()) bug("error from command: “%s”\n", error.c_str()););

		int status = 0;
		bool didTerminate = waitpid(process_id, &status, 0) == process_id;
		if(didTerminate && WIFEXITED(status) && WEXITSTATUS(status) == 0)
			return output;

		if(!didTerminate)
			perror("waitpid");
		else if(!WIFEXITED(status))
			fprintf(stderr, "*** abnormal exit (%d) from ‘%s’ in ‘%s’\n", status, text::join(command, " ").c_str(), pwdDir);
		else
			fprintf(stderr, "*** exit code %d from ‘%s’ in ‘%s’\n", WEXITSTATUS(status), text::join(command, " ").c_str(), pwdDir);

		return NULL_STR;
	}

	std::string exec (std::map<std::string, std::string> const& env, std::string const& cmd, ...)
	{
		va_list args;
		va_start(args, cmd);
		return vexec(env, cmd, args);
	}

	std::string exec (std::string const& cmd, ...)
	{
		std::map<std::string, std::string> env = oak::basic_environment();

		va_list args;
		va_start(args, cmd);
		return vexec(env, cmd, args);
	}

} /* io */
