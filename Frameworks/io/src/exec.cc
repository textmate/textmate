#include "exec.h"
#include "environment.h"
#include <text/format.h>
#include <oak/datatypes.h>
#include <oak/debug/OakDebugLog.h>
#include <oak/compat.h>

OAK_DEBUG_VAR(IO_Exec);

#define OAK_CHECK(expr) do { if((expr) != 0) { perror(#expr); abort(); } } while(false)

namespace io
{
	// takes NULL-terminated list of arguments
	static std::string vexec (std::map<std::string, std::string> const& environment, std::string const& cmd, va_list args)
	{
		std::vector<char*> command(1, (char*)cmd.c_str());
		char* arg = NULL;
		while((arg = va_arg(args, char*)) && *arg)
			command.push_back(arg);
		va_end(args);

		char* argv[command.size() + 1];
		std::copy(command.begin(), command.end(), &argv[0]);
		argv[command.size()] = NULL;

		oak::c_array env(environment);

		int out[2], err[2];
		posix_spawn_file_actions_t fileActions;
		posix_spawnattr_t flags;
		pid_t pid = -1;

		short closeOnExecFlag = (oak::os_major() == 10 && oak::os_minor() == 7) ? 0 : POSIX_SPAWN_CLOEXEC_DEFAULT;

		OAK_CHECK(pipe(&out[0]));
		OAK_CHECK(pipe(&err[0]));
		OAK_CHECK(fcntl(out[0], F_SETFD, FD_CLOEXEC));
		OAK_CHECK(fcntl(err[0], F_SETFD, FD_CLOEXEC));
		OAK_CHECK(posix_spawn_file_actions_init(&fileActions));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, 0));
		OAK_CHECK(posix_spawn_file_actions_addopen(&fileActions, 0, "/dev/null", O_RDONLY, 0));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, out[1], 1));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, err[1], 2));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, out[1]));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, err[1]));
		OAK_CHECK(posix_spawnattr_init(&flags));
		OAK_CHECK(posix_spawnattr_setflags(&flags, POSIX_SPAWN_SETSIGDEF|closeOnExecFlag));
		OAK_CHECK(posix_spawnattr_setpgroup(&flags, getpid()));
		OAK_CHECK(posix_spawn(&pid, argv[0], &fileActions, &flags, argv, env));
		OAK_CHECK(posix_spawnattr_destroy(&flags));
		OAK_CHECK(posix_spawn_file_actions_destroy(&fileActions));
		OAK_CHECK(close(out[1]));
		OAK_CHECK(close(err[1]));

		std::string output, error;

		char buf[255];
		while(ssize_t len = read(out[0], buf, sizeof(buf)))
		{
			if(len < 0)
				break;
			output.insert(output.end(), buf, buf + len);
		}
		close(out[0]);

		while(ssize_t len = read(err[0], buf, sizeof(buf)))
		{
			if(len < 0)
				break;
			error.insert(error.end(), buf, buf + len);
		}
		close(err[0]);

		D(DBF_IO_Exec, if(!error.empty()) bug("error from command: “%s”\n", error.c_str()););

		int status = 0;
		bool didTerminate = waitpid(pid, &status, 0) == pid;
		if(didTerminate && WIFEXITED(status) && WEXITSTATUS(status) == 0)
			return output;

		if(!didTerminate)
			perror("waitpid");
		else if(!WIFEXITED(status))
			fprintf(stderr, "*** abnormal exit (%d) from ‘%s’\n", status, text::join(command, " ").c_str());
		else
			fprintf(stderr, "*** exit code %d from ‘%s’\n", WEXITSTATUS(status), text::join(command, " ").c_str());

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
		va_list args;
		va_start(args, cmd);
		return vexec(oak::basic_environment(), cmd, args);
	}

} /* io */
