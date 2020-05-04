#include "date.h"
#include <oak/debug.h>

namespace oak
{
	static time_t kUnixEpochOffset = 978307200;

	date_t::date_t (time_t unixTime) : at((CFAbsoluteTime)(unixTime - kUnixEpochOffset))
	{
	}

	date_t::date_t (std::string const& str) : at(0)
	{
		if(str == NULL_STR)
			return;

		struct tm bsdDate = { };
		if(strptime(str.c_str(), "%F %T %z", &bsdDate))
		{
			time_t seconds = mktime(&bsdDate);
			if(seconds != -1)
				at = (CFAbsoluteTime)(seconds - kUnixEpochOffset);
		}
		else
		{
			os_log_error(OS_LOG_DEFAULT, "Error parsing date: ‘%{public}s’", str.c_str());
		}
	}

	date_t date_t::now ()
	{
		return CFAbsoluteTimeGetCurrent();
	}

	std::string to_s (date_t const& date, std::string const& dateFormat)
	{
		char buf[64];
		time_t seconds = date.time_value();
		strftime(buf, sizeof(buf), dateFormat.c_str(), localtime(&seconds));
		return std::string(buf);
	}

} /* oak */
