#ifndef LICENSE_H_CDWLTII6
#define LICENSE_H_CDWLTII6

#include "keychain.h"

namespace license
{
	std::map<std::string, std::string> decode (std::string const& license, std::string const& publicKey = NULL_STR);
	bool is_valid (std::map<std::string, std::string> const& license, std::string const& owner);
	bool is_revoked (std::map<std::string, std::string> const& license);
	std::string error_description (std::string const& license, std::string const& owner, std::string const& publicKey = NULL_STR);

} /* license */

#endif /* end of include guard: LICENSE_H_CDWLTII6 */
