#include "pipe.h"

namespace io
{
	std::tuple<int, int> create_pipe ()
	{
		int pipes[2];
		if(pipe(&pipes[0]) != -1)
		{
			fcntl(pipes[0], F_SETFD, FD_CLOEXEC);
			fcntl(pipes[1], F_SETFD, FD_CLOEXEC);
		}
		else
		{
			perror("io::create_pipe: pipe");
		}
		return { pipes[0], pipes[1] };
	}

} /* io */
