#include "find_reports.h"
#include <io/path.h>
#include <io/entries.h>

static CFAbsoluteTime parse_date (std::string const& dateStr, std::string const& format)
{
	struct tm bsdDate;
	if(strptime(dateStr.c_str(), format.c_str(), &bsdDate))
	{
		CFGregorianDate gregorianDate;
		gregorianDate.second = bsdDate.tm_sec;
		gregorianDate.minute = bsdDate.tm_min;
		gregorianDate.hour   = bsdDate.tm_hour;
		gregorianDate.day    = bsdDate.tm_mday;
		gregorianDate.month  = bsdDate.tm_mon + 1;
		gregorianDate.year   = bsdDate.tm_year + 1900;

		return CFGregorianDateGetAbsoluteTime(gregorianDate, NULL);
	}
	return 0;
}

std::map<CFAbsoluteTime, std::string> find_reports (std::string const& process)
{
	static std::string const locations[] = {
		path::join(path::home(), "Library/Logs/DiagnosticReports"),
		path::join(path::home(), "Library/Logs/CrashReporter") // before Snow Leopard
	};

	std::map<CFAbsoluteTime, std::string> res;
	iterate(location, locations)
	{
		citerate(entry, path::entries(*location))
		{
			std::string file((*entry)->d_name);
			if((*entry)->d_type == DT_REG && file.find(process) == 0)
				res.emplace(parse_date(file, process + "_%F-\x25H\x25M%S"), path::join(*location, (*entry)->d_name));
		}
	}
	return res;
}
