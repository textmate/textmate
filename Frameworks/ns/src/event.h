#ifndef NS_EVENT_H_KLE3JJCO
#define NS_EVENT_H_KLE3JJCO

#include <oak/misc.h>

namespace ns
{
	PUBLIC std::string normalize_event_string (std::string const& eventString, size_t* startOfKey = NULL);
	PUBLIC std::string glyphs_for_event_string (std::string const& eventString, size_t* startOfKey = NULL);
	PUBLIC std::string glyphs_for_flags (NSUInteger flags);

} /* ns */

#endif /* end of include guard: NS_EVENT_H_KLE3JJCO */
