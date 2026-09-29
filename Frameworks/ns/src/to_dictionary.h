#ifndef NS_PLIST_H_JY52POBO
#define NS_PLIST_H_JY52POBO

#include <plist/src/plist.h>

namespace ns
{
	NSDictionary* to_dictionary (plist::any_t const& plist);
	NSMutableDictionary* to_mutable_dictionary (plist::any_t const& plist);

} /* ns */

#endif /* end of include guard: NS_PLIST_H_JY52POBO */
