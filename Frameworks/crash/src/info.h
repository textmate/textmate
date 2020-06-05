#ifndef CRASH_REPORTER_UTILITY_H_PWH2E0EP
#define CRASH_REPORTER_UTILITY_H_PWH2E0EP

#ifndef BSTR
#define BSTR(b) ((b)?"YES":"NO")
#endif

struct crash_reporter_info_t
{
	__attribute__ ((format (printf, 2, 3))) crash_reporter_info_t (char const* format, ...);
	crash_reporter_info_t (std::string const& str);
	~crash_reporter_info_t ();
	crash_reporter_info_t& operator= (std::string const& str);
	crash_reporter_info_t& operator<< (std::string const& str);
};

#endif /* end of include guard: CRASH_REPORTER_UTILITY_H_PWH2E0EP */
