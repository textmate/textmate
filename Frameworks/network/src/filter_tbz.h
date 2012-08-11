#ifndef EXTRACT_H_L6SJUZHG
#define EXTRACT_H_L6SJUZHG

#include "download.h" // filter_t
#include <io/io.h>
#include <OakSystem/application.h>
#include <oak/datatypes.h>
#include <text/format.h>
#include <text/trim.h>

// =======================
// = Extract tbz archive =
// =======================

namespace network
{
	struct tbz_t : filter_t
	{
		bool setup ()
		{
			path = path::temp("dl_tbz_filter");
			if(mkdir(path.c_str(), S_IRUSR|S_IWUSR|S_IXUSR|S_IRGRP|S_IWGRP|S_IXGRP|S_IROTH|S_IWOTH|S_IXOTH) == 0)
				return launch_tar();
			return false;
		}

		bool receive_data (char const* buf, size_t len)
		{
			return write(input[1], buf, len) == len;
		}

		bool receive_end (std::string& error)
		{
			close(input[1]);

			std::string tarOutput;
			ssize_t len;
			char buf[512];
			while((len = read(output[0], buf, sizeof(buf))) > 0)
				tarOutput.insert(tarOutput.end(), buf, buf + len);
			close(output[0]);

			int status = 0;
			if(waitpid(pid, &status, 0) == pid && WIFEXITED(status))
			{
				if(WEXITSTATUS(status) == 0 && tarOutput.empty())
					return true;
				error = text::format("Corrupt archive.");
				fprintf(stderr, "tar exit status %d\n%s\n", WEXITSTATUS(status), text::trim(tarOutput).c_str());
			}
			else
			{
				error = text::format("Abnormal exit from tar (%d).", status);
			}
			return false;
		}

		std::string name ()
		{
			return "tar";
		}

		std::string path = NULL_STR;

	private:
		static char const* strip_components_flag ()
		{
			SInt32 osVersion = 0;
			Gestalt(gestaltSystemVersion, &osVersion);
			return osVersion >= 0x1050 ? "--strip-components" : "--strip-path";
		}

		bool launch_tar ()
		{
			signal(SIGPIPE, SIG_IGN);

			pipe(&input[0]);
			pipe(&output[0]);

			char const* argv[] = { "/usr/bin/tar", "-jxmkC", path.c_str(), strip_components_flag(), "1", NULL };
			oak::c_array env(oak::basic_environment());
			pid = vfork();
			if(pid == 0)
			{
				close(0); close(1); close(2);
				dup(input[0]); dup(output[1]); dup(output[1]);
				close(input[0]); close(input[1]); close(output[0]); close(output[1]);

				signal(SIGPIPE, SIG_DFL);

				execve(argv[0], (char* const*)argv, env);
				_exit(-1);
			}
			else if(pid == -1)
			{
				return false;
			}
			else
			{
				close(input[0]);
				close(output[1]);
				fcntl(input[1], F_SETFD, 1);
				fcntl(output[0], F_SETFD, 1);
			}
			return true;
		}

		int input[2];
		int output[2];
		pid_t pid;
	};

} /* network */

#endif /* end of include guard: EXTRACT_H_L6SJUZHG */
