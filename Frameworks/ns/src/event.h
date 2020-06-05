#ifndef NS_EVENT_H_KLE3JJCO
#define NS_EVENT_H_KLE3JJCO

namespace ns
{
	std::string normalize_event_string (std::string const& eventString, size_t* startOfKey = NULL);
	std::string glyphs_for_event_string (std::string const& eventString, size_t* startOfKey = NULL);
	std::string glyphs_for_flags (NSUInteger flags);

} /* ns */

NSAttributedString* OakAttributedStringForEventString (NSString* eventString, NSFont* font);

#endif /* end of include guard: NS_EVENT_H_KLE3JJCO */
