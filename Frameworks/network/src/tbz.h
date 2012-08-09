#ifndef NETWORK_TBZ_H_NEU56OWR
#define NETWORK_TBZ_H_NEU56OWR

#include <oak/misc.h>

namespace network
{
	PUBLIC pid_t launch_tbz (std::string const& dest, int& input, int& output, std::string& error);
	PUBLIC bool finish_tbz (pid_t pid, int& input, int& output, std::string& error);

} /* network */

#endif /* end of include guard: NETWORK_TBZ_H_NEU56OWR */
