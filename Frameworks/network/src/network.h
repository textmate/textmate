#ifndef NETWORK_H_L3XXH7J6
#define NETWORK_H_L3XXH7J6

#include "constants.h"
#include "download.h"
#include "filter_save.h"
#include "filter_check_signature.h"
#include "filter_header.h"
#include <oak/misc.h>

namespace network
{
	PUBLIC bool can_reach_host (char const* host);

} /* network */

#endif /* end of include guard: NETWORK_H_L3XXH7J6 */
