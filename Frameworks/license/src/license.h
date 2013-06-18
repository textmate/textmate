#ifndef LICENSE_H_CDWLTII6
#define LICENSE_H_CDWLTII6

#include <oak/misc.h>
#include "keychain.h"

namespace license
{
	PUBLIC std::map<std::string, std::string> decode (std::string const& license, std::string const& publicKey = NULL_STR);
	PUBLIC bool is_valid (std::map<std::string, std::string> const& license, std::string const& owner);
	PUBLIC bool is_revoked (std::map<std::string, std::string> const& license);
	PUBLIC std::string error_description (std::string const& license, std::string const& owner, std::string const& publicKey = NULL_STR);

} /* license */

#endif /* end of include guard: LICENSE_H_CDWLTII6 */
