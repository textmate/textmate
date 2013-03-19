#include "utility.h"

/* CrashReporter info */
char const* __crashreporter_info__ = NULL;
asm(".desc ___crashreporter_info__, 0x10");

crash_reporter_info_t::crash_reporter_info_t (std::string const& string) : _string(string)
{
	__crashreporter_info__ = _string.c_str();
}

crash_reporter_info_t::~crash_reporter_info_t ()
{
	__crashreporter_info__ = NULL;
}
