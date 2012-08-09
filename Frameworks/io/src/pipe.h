#ifndef IO_PIPE_H_CZY71GYP
#define IO_PIPE_H_CZY71GYP

#include <oak/misc.h>

namespace io
{
	PUBLIC void create_pipe (int& read_pipe, int& write_pipe, bool close_on_exec);
	
} /* io */

#endif /* end of include guard: IO_PIPE_H_CZY71GYP */
