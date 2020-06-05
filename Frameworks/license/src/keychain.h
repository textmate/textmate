#ifndef KEYCHAIN_H_5C73NTLQ
#define KEYCHAIN_H_5C73NTLQ

namespace license
{
	bool add (std::string const& date, std::string const& key, std::string* error);
	std::string find (std::string const& key);
	std::vector<std::string> find_all ();

} /* license */

#endif /* end of include guard: KEYCHAIN_H_5C73NTLQ */
