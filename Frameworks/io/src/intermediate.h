#ifndef INTERMEDIATE_H_LWU9YRUW
#define INTERMEDIATE_H_LWU9YRUW

#include <oak/misc.h>

namespace path
{
	struct PUBLIC intermediate_t
	{
		intermediate_t (std::string const& dest);
		bool commit () const;

		operator std::string const& () const { return _intermediate; }
		operator char const* () const        { return _intermediate.c_str(); }

	private:
		std::string _resolved;
		std::string _intermediate;
	};

} /* path */

#endif /* end of include guard: INTERMEDIATE_H_LWU9YRUW */
