#include "exec.h"
#include "environment.h"
#include "pipe.h"
#include <text/format.h>
#include <oak/datatypes.h>
#include <crash/info.h>

namespace io
{
	process_t spawn (std::vector<std::string> const& args, std::map<std::string, std::string> const& environment)
	{
		process_t res;

		int in, out, err;
		std::tie(in, res.in)   = io::create_pipe();
		std::tie(res.out, out) = io::create_pipe();
		std::tie(res.err, err) = io::create_pipe();

		int rc = -1;
		if(in != -1 && out != -1 && err != -1)
		{
			posix_spawn_file_actions_t fileActions;
			if(posix_spawn_file_actions_init(&fileActions) == 0)
			{
				if(posix_spawn_file_actions_adddup2(&fileActions, in,  STDIN_FILENO) == 0 && posix_spawn_file_actions_adddup2(&fileActions, out, STDOUT_FILENO) == 0 && posix_spawn_file_actions_adddup2(&fileActions, err, STDERR_FILENO) == 0)
				{
					posix_spawnattr_t flags;
					if(posix_spawnattr_init(&flags) == 0)
					{
						if(posix_spawnattr_setflags(&flags, POSIX_SPAWN_SETSIGDEF|POSIX_SPAWN_CLOEXEC_DEFAULT) == 0)
						{
							char* argv[args.size() + 1];
							std::transform(args.begin(), args.end(), &argv[0], [](std::string const& str){ return (char*)str.c_str(); });
							argv[args.size()] = nullptr;

							rc = posix_spawn(&res.pid, argv[0], &fileActions, &flags, argv, oak::c_array(environment));
							if(rc != 0)
								perrorf("io::spawn: posix_spawn(\"%s\")", argv[0]);
						}
						else
						{
							perrorf("posix_spawnattr_setflags");
						}
						posix_spawnattr_destroy(&flags);
					}
					else
					{
						perror("posix_spawnattr_init");
					}
				}
				else
				{
					perror("posix_spawn_file_actions_adddup2");
				}
				posix_spawn_file_actions_destroy(&fileActions);
			}
			else
			{
				perror("posix_spawn_file_actions_init");
			}
		}

		close(in);
		close(out);
		close(err);

		if(rc != 0)
		{
			close(res.in);
			close(res.out);
			close(res.err);

			res.in = res.out = res.err = -1;
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
		char* arg = nullptr;
		while((arg = va_arg(args, char*)) && *arg)
			command.push_back(arg);
		va_end(args);

		process_t process = spawn(command, environment);
		if(!process)
			return NULL_STR;
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
				perror("io::vexec: waitpid");
			else if(!WIFEXITED(status))
				os_log_error(OS_LOG_DEFAULT, "Abnormal exit (%d) from ‘%{public}s’", status, text::join(command, " ").c_str());
			else if(WEXITSTATUS(status) != 0)
				os_log_error(OS_LOG_DEFAULT, "Exit code %d from ‘%{public}s’", WEXITSTATUS(status), text::join(command, " ").c_str());
			else
				success = true;
		});

		dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
		dispatch_release(group);
		return success ? output : NULL_STR;
	}

	std::string exec (std::map<std::string, std::string> const& env, std::string const cmd, ...)
	{
		va_list args;
		va_start(args, cmd);
		return vexec(env, cmd, args);
	}

	std::string exec (std::string const cmd, ...)
	{
		va_list args;
		va_start(args, cmd);
		return vexec(oak::basic_environment(), cmd, args);
	}

} /* io */
