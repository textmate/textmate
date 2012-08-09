#ifndef PLIST_DELTA_H_10G60Z84
#define PLIST_DELTA_H_10G60Z84

#include "plist.h"
#include <oak/misc.h>

namespace plist
{
	PUBLIC dictionary_t create_delta (dictionary_t const& oldDict, dictionary_t const& newDict);
	PUBLIC dictionary_t merge_delta (std::vector<dictionary_t> const& plists);

} /* plist */

#endif /* end of include guard: PLIST_DELTA_H_10G60Z84 */
