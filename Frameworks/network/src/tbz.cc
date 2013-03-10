#include "tbz.h"
#include <text/format.h>
#include <text/trim.h>
#include <oak/datatypes.h>
#include <io/environment.h>

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

		bool ok = true;
		ok = ok && pipe(&in[0])  == 0;
		ok = ok && pipe(&out[0]) == 0;
		ok = ok && posix_spawn_file_actions_init(&fileActions) == 0;
		ok = ok && posix_spawn_file_actions_adddup2(&fileActions, in[0],  0) == 0;
		ok = ok && posix_spawn_file_actions_adddup2(&fileActions, out[1], 1) == 0;
		ok = ok && posix_spawn_file_actions_adddup2(&fileActions, out[1], 2) == 0;
		ok = ok && posix_spawnattr_init(&flags) == 0;
		ok = ok && posix_spawnattr_setflags(&flags, POSIX_SPAWN_SETSIGDEF|POSIX_SPAWN_CLOEXEC_DEFAULT) == 0;
		ok = ok && posix_spawn(&pid, "/usr/bin/tar", &fileActions, &flags, (char* const*)argv, env) == 0;
		ok = ok && posix_spawnattr_destroy(&flags) == 0;
		ok = ok && posix_spawn_file_actions_destroy(&fileActions) == 0;
		ok = ok && close(in[0])  == 0;
		ok = ok && close(out[1]) == 0;

		if(!ok)
		{
			perror("launch_tbz");
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