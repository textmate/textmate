#include "find_reports.h"
#include <io/path.h>
#include <io/entries.h>

std::map<time_t, std::string> find_reports (std::string const& process)
{
	std::string const location    = path::join(path::home(), "Library/Logs/DiagnosticReports");
	std::string const time_format = process + "_%F-%H%M%S";

	std::map<time_t, std::string> res;
	for(auto const& entry : path::entries(location))
	{
		std::string file(entry->d_name);
		if(entry->d_type == DT_REG && file.compare(0, process.size(), process) == 0)
		{
			struct tm bsdDate = { };
			if(strptime(file.c_str(), time_format.c_str(), &bsdDate))
			{
				time_t seconds = mktime(&bsdDate);
				if(seconds != -1)
					res.emplace(seconds, path::join(location, entry->d_name));
			}
		}
	}
	return res;
}
