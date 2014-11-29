#ifndef NS_H_SEBJ2BBY
#define NS_H_SEBJ2BBY

#include <oak/misc.h>
#include "event.h"
#include "to_dictionary.h"

PUBLIC std::string to_s (NSString* aString);
PUBLIC std::string to_s (NSAttributedString* anAttributedString);
PUBLIC std::string to_s (NSData* aString);
PUBLIC std::string to_s (NSError* anError);
PUBLIC std::string to_s (NSEvent* anEvent, bool preserveNumPadFlag = false);
PUBLIC std::string to_s (id someObject);

namespace ns
{
	PUBLIC std::string create_event_string (NSString* key, NSUInteger flags);

} /* ns */

#endif /* end of include guard: NS_H_SEBJ2BBY */
