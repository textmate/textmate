#ifndef NETWORK_DOWNLOAD_TBZ_H_MYCOASC1
#define NETWORK_DOWNLOAD_TBZ_H_MYCOASC1

#include "key_chain.h"

namespace network
{
	std::string download_tbz (std::string const& url, key_chain_t const& keyChain, std::string const& destination, std::string& error, double* progress, double progressStart = 0, double progressStop = 1, bool const* stopFlag = NULL);

} /* network */

#endif /* end of include guard: NETWORK_DOWNLOAD_TBZ_H_MYCOASC1 */
