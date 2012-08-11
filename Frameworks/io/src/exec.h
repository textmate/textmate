#ifndef IO_EXEC_H_4VCQB3PK
#define IO_EXEC_H_4VCQB3PK

#include <oak/misc.h>

namespace io
{
	// takes NULL-terminated list of arguments
	PUBLIC std::string exec (std::string const& cmd, ...);
	PUBLIC std::string exec (std::map<std::string, std::string> const& environment, std::string const& cmd, ...);

} /* io */

#endif /* end of include guard: IO_EXEC_H_4VCQB3PK */
