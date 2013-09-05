#include "exec.h"
#include "environment.h"
#include <text/format.h>
#include <oak/datatypes.h>
#include <oak/debug/OakDebugLog.h>
#include <oak/compat.h>
#include <crash/info.h>

OAK_DEBUG_VAR(IO_Exec);

#define OAK_CHECK(expr) do { if((expr) != 0) { crash_reporter_info_t crashInfo(text::format("%s: %s", #expr, strerror(errno))); abort(); } } while(false)

namespace io
{
	process_t spawn (std::vector<std::string> const& args, std::map<std::string, std::string> const& environment)
	{
		int in[2], out[2], err[2];
		posix_spawn_file_actions_t fileActions;
		posix_spawnattr_t flags;

		short closeOnExecFlag = (oak::os_major() == 10 && oak::os_minor() == 7) ? 0 : POSIX_SPAWN_CLOEXEC_DEFAULT;

		OAK_CHECK(pipe(&in[0]));
		OAK_CHECK(pipe(&out[0]));
		OAK_CHECK(pipe(&err[0]));
		OAK_CHECK(fcntl(in[1],  F_SETFD, FD_CLOEXEC));
		OAK_CHECK(fcntl(out[0], F_SETFD, FD_CLOEXEC));
		OAK_CHECK(fcntl(err[0], F_SETFD, FD_CLOEXEC));
		OAK_CHECK(posix_spawn_file_actions_init(&fileActions));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, in[0],  0));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, out[1], 1));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, err[1], 2));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, in[0]));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, out[1]));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, err[1]));
		OAK_CHECK(posix_spawnattr_init(&flags));
		OAK_CHECK(posix_spawnattr_setflags(&flags, POSIX_SPAWN_SETSIGDEF|closeOnExecFlag));

		char* argv[args.size() + 1];
		std::transform(args.begin(), args.end(), &argv[0], [](std::string const& str){ return (char*)str.c_str(); });
		argv[args.size()] = NULL;

		process_t res;
		int rc = posix_spawn(&res.pid, argv[0], &fileActions, &flags, argv, oak::c_array(environment));
		if(rc != 0)
			perror(text::format("posix_spawn(\"%s\")", argv[0]).c_str());

		OAK_CHECK(posix_spawnattr_destroy(&flags));
		OAK_CHECK(posix_spawn_file_actions_destroy(&fileActions));
		OAK_CHECK(close(in[0]));
		OAK_CHECK(close(out[1]));
		OAK_CHECK(close(err[1]));

		if(rc == 0)
		{
			res.in  = in[1];
			res.out = out[0];
			res.err = err[0];
		}
		else
		{
			OAK_CHECK(close(in[1]));
			OAK_CHECK(close(out[0]));
			OAK_CHECK(close(err[0]));
		}

		return res;
	}

	process_t spawn (std::vector<std::string> const& args)
	{
		return spawn(args, oak::basic_environment());
	}

	void exhaust_fd (int fd, std::string* out)
	{
		char buf[255];
		while(ssize_t len = read(fd, buf, sizeof(buf)))
		{
			if(len < 0)
				break;
			out->insert(out->end(), buf, buf + len);
		}
		close(fd);
	}

	// takes NULL-terminated list of arguments
	static std::string vexec (std::map<std::string, std::string> const& environment, std::string const& cmd, va_list args)
	{
		std::vector<std::string> command(1, cmd);
		char* arg = NULL;
		while((arg = va_arg(args, char*)) && *arg)
			command.push_back(arg);
		va_end(args);

		process_t process = spawn(command, environment);
		close(process.in);

		__block std::string output, error;
		__block bool success = false;

		dispatch_group_t group = dispatch_group_create();

		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			exhaust_fd(process.out, &output);
		});

		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			exhaust_fd(process.err, &error);
		});

		dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			int status = 0;
			if(waitpid(process.pid, &status, 0) != process.pid)
				perror("waitpid");
			else if(!WIFEXITED(status))
				fprintf(stderr, "*** abnormal exit (%d) from ‘%s’\n", status, text::join(command, " ").c_str());
			else if(WEXITSTATUS(status) != 0)
				fprintf(stderr, "*** exit code %d from ‘%s’\n", WEXITSTATUS(status), text::join(command, " ").c_str());
			else
				success = true;
		});

		dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
		dispatch_release(group);

		D(DBF_IO_Exec, if(!error.empty()) bug("error from command: “%s”\n", error.c_str()););

		return success ? output : NULL_STR;
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
