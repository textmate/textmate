#include "pipe.h"

namespace io
{
	void create_pipe (int& read_pipe, int& write_pipe, bool close_on_exec)
	{
		int pipes[2];
		pipe(&pipes[0]);
		read_pipe = pipes[0];
		write_pipe = pipes[1];
		if(close_on_exec)
		{
			fcntl(pipes[0], F_SETFD, FD_CLOEXEC);
			fcntl(pipes[1], F_SETFD, FD_CLOEXEC);
		}
	}

} /* io */
