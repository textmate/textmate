#ifndef VERSION_H_FY5OTOGA
#define VERSION_H_FY5OTOGA

#include <oak/misc.h>

namespace version
{
	PUBLIC bool less (std::string const& lhs, std::string const& rhs);
	inline bool greater (std::string const& lhs, std::string const& rhs)       { return less(rhs, lhs); }
	inline bool equal (std::string const& lhs, std::string const& rhs)         { return !less(lhs, rhs) && !less(rhs, lhs); }
	inline bool less_or_equal (std::string const& lhs, std::string const& rhs) { return less(lhs, rhs) || !less(rhs, lhs); }

} /* version */

#endif /* end of include guard: VERSION_H_FY5OTOGA */
