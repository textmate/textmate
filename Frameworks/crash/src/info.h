#ifndef CRASH_REPORTER_UTILITY_H_PWH2E0EP
#define CRASH_REPORTER_UTILITY_H_PWH2E0EP

#include <oak/misc.h>

struct PUBLIC crash_reporter_info_t
{
	crash_reporter_info_t (std::string const& string);
	~crash_reporter_info_t ();
	crash_reporter_info_t& operator= (std::string const& str);
	crash_reporter_info_t& operator<< (std::string const& str);
private:
	std::string _string;
};

#endif /* end of include guard: CRASH_REPORTER_UTILITY_H_PWH2E0EP */
