#include "tbz.h"
#include <text/format.h>
#include <oak/datatypes.h>
#include <io/environment.h>

namespace network
{
	static char const* strip_components_flag ()
	{
		SInt32 osVersion = 0;
		Gestalt(gestaltSystemVersion, &osVersion);
		return osVersion >= 0x1050 ? "--strip-components" : "--strip-path";
	}

	pid_t launch_tbz (std::string const& dest, int& input, int& output, std::string& error)
	{
		signal(SIGPIPE, SIG_IGN);

		int in[2], out[2];
		pipe(&in[0]);
		pipe(&out[0]);

		char const* argv[] = { "/usr/bin/tar", "-jxmkC", dest.c_str(), strip_components_flag(), "1", NULL };
		oak::c_array env(oak::basic_environment());
		pid_t pid = vfork();
		if(pid == 0)
		{
			close(0); close(1); close(2);
			dup(in[0]); dup(out[1]); dup(out[1]);
			close(in[0]); close(in[1]); close(out[0]); close(out[1]);

			signal(SIGPIPE, SIG_DFL);

			execve(argv[0], (char* const*)argv, env);
			_exit(-1);
		}
		else
		{
			close(in[0]);
			close(out[1]);

			if(pid == -1)
			{
				close(in[1]);
				close(out[0]);

				error = text::format("Error launching tar: %s", strerror(errno));
			}
			else
			{
				fcntl(input  = in[1],  F_SETFD, 1);
				fcntl(output = out[0], F_SETFD, 1);
			}
		}
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
			error = "Corrupt archive.";
			// error = text::format("Unexpected exit code from tar (%d)\n%s\n", WEXITSTATUS(status), text::trim(tbzOut).c_str());
		}
		else
		{
			error = text::format("Abnormal exit from tar (%d).\n", status);
		}
		return false;
	}

} /* network */