#include "tbz.h"
#include <text/format.h>
#include <text/trim.h>
#include <oak/datatypes.h>
#include <oak/compat.h>
#include <io/exec.h>

#define OAK_CHECK(expr) do { if((expr) != 0) { perror(#expr); return -1; } } while(false)

namespace network
{
	pid_t launch_tbz (std::string const& dest, int& input, int& output, std::string& error)
	{
		if(io::process_t process = io::spawn(std::vector<std::string>{ "/usr/bin/tar", "-jxmkC", dest, "--strip-components", "1" }))
		{
			input  = process.in;
			output = process.out;

			dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				ssize_t len;
				char bytes[512];
				std::string str;
				while((len = read(process.err, bytes, sizeof(bytes))) > 0)
					str.insert(str.end(), bytes, bytes + len);
				close(process.err);

				if(!str.empty())
					fprintf(stderr, "%s\n", str.c_str());
			});

			return process.pid;
		}
		return -1;
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