#ifndef KEYCHAIN_H_5C73NTLQ
#define KEYCHAIN_H_5C73NTLQ

#include <oak/misc.h>

namespace license
{
	PUBLIC bool add (std::string const& date, std::string const& key);
	PUBLIC std::string find (std::string const& key);
	PUBLIC std::vector<std::string> find_all ();

} /* license */

#endif /* end of include guard: KEYCHAIN_H_5C73NTLQ */
