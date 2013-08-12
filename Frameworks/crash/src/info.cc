#include "info.h"

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

crash_reporter_info_t& crash_reporter_info_t::operator= (std::string const& str)
{
	_string = str;
	__crashreporter_info__ = _string.c_str();
	return *this;
}

crash_reporter_info_t& crash_reporter_info_t::operator<< (std::string const& str)
{
	_string.append("\n");
	_string.append(str);
	__crashreporter_info__ = _string.c_str();
	return *this;
}
