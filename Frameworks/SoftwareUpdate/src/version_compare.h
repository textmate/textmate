#ifndef VERSION_COMPARE_H_FY5OTOGA
#define VERSION_COMPARE_H_FY5OTOGA

bool version_less (std::string const& rhs, std::string const& lhs);

inline bool version_equal (std::string const& rhs, std::string const& lhs)
{
	return !version_less(lhs, rhs) && !version_less(rhs, lhs);
}

#endif /* end of include guard: VERSION_COMPARE_H_FY5OTOGA */
