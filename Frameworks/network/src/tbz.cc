#include "tbz.h"
#include <text/format.h>
#include <text/trim.h>
#include <oak/datatypes.h>
#include <oak/compat.h>
#include <io/environment.h>

#define OAK_CHECK(expr) do { if((expr) != 0) { perror(#expr); return -1; } } while(false)

namespace network
{
	pid_t launch_tbz (std::string const& dest, int& input, int& output, std::string& error)
	{
		signal(SIGPIPE, SIG_IGN);

		char const* const argv[] = { "/usr/bin/tar", "-jxmkC", dest.c_str(), "--strip-components", "1", NULL };
		oak::c_array env(oak::basic_environment());

		int in[2], out[2];
		posix_spawn_file_actions_t fileActions;
		posix_spawnattr_t flags;
		pid_t pid = -1;

		short closeOnExecFlag = (oak::os_major() == 10 && oak::os_minor() == 7) ? 0 : POSIX_SPAWN_CLOEXEC_DEFAULT;

		OAK_CHECK(pipe(&in[0]) );
		OAK_CHECK(pipe(&out[0]));
		OAK_CHECK(fcntl(in[1],  F_SETFD, FD_CLOEXEC));
		OAK_CHECK(fcntl(out[0], F_SETFD, FD_CLOEXEC));
		OAK_CHECK(posix_spawn_file_actions_init(&fileActions));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, in[0], 0));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, out[1], 1));
		OAK_CHECK(posix_spawn_file_actions_adddup2(&fileActions, out[1], 2));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, in[0]));
		OAK_CHECK(posix_spawn_file_actions_addclose(&fileActions, out[1]));
		OAK_CHECK(posix_spawnattr_init(&flags));
		OAK_CHECK(posix_spawnattr_setflags(&flags, POSIX_SPAWN_SETSIGDEF|closeOnExecFlag));

		int rc = posix_spawn(&pid, "/usr/bin/tar", &fileActions, &flags, (char* const*)argv, env);
		if(rc != 0)
			perror(text::format("posix_spawn(\"/usr/bin/tar\")").c_str());

		OAK_CHECK(posix_spawnattr_destroy(&flags));
		OAK_CHECK(posix_spawn_file_actions_destroy(&fileActions));
		OAK_CHECK(close(in[0]));
		OAK_CHECK(close(out[1]));

		if(rc != 0)
		{
			close(in[1]);
			close(out[0]);
			return -1;
		}

		input  = in[1];
		output = out[0];

		return pid;
	}

	bool finish_tbz (pid_t pid, int& input, int& output, std::string& error)
	{
		close(input);

		std::string tbzOut;
		ssize_t len;
		char bytes[512];
		while((len = read(output, bytes, sizeof(bytes))) > 0)
			tbzOut.insert(tbzOut.end(), bytes, bytes + len);
		close(output);

		int status = 0;
		if(waitpid(pid, &status, 0) == pid && WIFEXITED(status))
		{
			if(WEXITSTATUS(status) == 0 && tbzOut.empty())
				return true;
			error = "Extracting archive.";
			// fprintf(stderr, "%s: unexpected exit code from tar %d: %s\n", getprogname(), WEXITSTATUS(status), text::trim(tbzOut).c_str());
		}
		else
		{
			error = text::format("Abnormal exit from tar (%d).\n", status);
		}
		return false;
	}

} /* network */