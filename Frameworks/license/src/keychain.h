#ifndef KEYCHAIN_H_5C73NTLQ
#define KEYCHAIN_H_5C73NTLQ

#include <oak/misc.h>

namespace license
{
	PUBLIC bool add (std::string const& date, std::string const& key, SecKeychainRef keychain = NULL);
	PUBLIC std::string find (std::string const& key, SecKeychainRef keychain = NULL);
	PUBLIC std::vector<std::string> find_all (SecKeychainRef keychain = NULL);

} /* license */

#endif /* end of include guard: KEYCHAIN_H_5C73NTLQ */
