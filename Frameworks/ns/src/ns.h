#ifndef NS_H_SEBJ2BBY
#define NS_H_SEBJ2BBY

#include "event.h"
#include "to_dictionary.h"

NSString* to_ns (std::string const& str);

std::string to_s (NSString* aString);
std::string to_s (NSAttributedString* anAttributedString);
std::string to_s (NSUUID* identifier);
std::string to_s (NSData* aString);
std::string to_s (NSError* anError);
std::string to_s (NSEvent* anEvent, bool preserveNumPadFlag = false);
std::string to_s (id someObject);

namespace ns
{
	std::string create_event_string (NSString* key, NSUInteger flags);

} /* ns */

#endif /* end of include guard: NS_H_SEBJ2BBY */
