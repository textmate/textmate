#include "date.h"
#include <oak/debug.h>

namespace oak
{
	date_t::date_t (time_t unixTime) : at((CFAbsoluteTime)unixTime - 978307200)
	{
	}

	date_t::date_t (std::string const& str) : at(0)
	{
		if(str == NULL_STR)
			return;

		struct tm bsdDate;
		if(strptime(str.c_str(), "%F %T %z", &bsdDate))
		{
			CFGregorianDate gregorianDate;
			gregorianDate.second = bsdDate.tm_sec;
			gregorianDate.minute = bsdDate.tm_min;
			gregorianDate.hour   = bsdDate.tm_hour;
			gregorianDate.day    = bsdDate.tm_mday;
			gregorianDate.month  = bsdDate.tm_mon + 1;
			gregorianDate.year   = bsdDate.tm_year + 1900;

			at = CFGregorianDateGetAbsoluteTime(gregorianDate, NULL) - bsdDate.tm_gmtoff;
		}
		else
		{
			fprintf(stderr, "*** error parsing date: ‘%s’\n", str.c_str());
		}
	}

	date_t date_t::now ()
	{
		return CFAbsoluteTimeGetCurrent();
	}

	std::string to_s (date_t const& date, std::string const& dateFormat)
	{
		CFGregorianDate gregorianDate = CFAbsoluteTimeGetGregorianDate(date.value(), NULL);

		struct tm bsdDate;
		bsdDate.tm_sec    = (int)gregorianDate.second;
		bsdDate.tm_min    = gregorianDate.minute;
		bsdDate.tm_hour   = gregorianDate.hour;
		bsdDate.tm_mday   = gregorianDate.day;
		bsdDate.tm_mon    = gregorianDate.month - 1;
		bsdDate.tm_year   = gregorianDate.year - 1900;
		bsdDate.tm_wday   = 0;
		bsdDate.tm_yday   = 0;
		bsdDate.tm_isdst  = 0;
		bsdDate.tm_gmtoff = 0;
		bsdDate.tm_zone   = (char*)"UTC";

		char buf[64];
		strftime(buf, sizeof(buf), dateFormat.c_str(), &bsdDate);
		return std::string(buf);
	}

} /* oak */