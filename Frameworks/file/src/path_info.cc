#include "path_info.h"
#include <scm/src/scm.h>
#include <settings/src/settings.h>
#include <io/src/entries.h>
#include <regexp/src/glob.h>
#include <plist/src/ascii.h>
#include <text/src/tokenize.h>
#include <oak/oak.h>
#include <oak/compat.h>

namespace file
{
	std::string path_attributes (std::string const& path)
	{
		std::vector<std::string> res;
		if(path != NULL_STR)
		{
			std::vector<std::string> revPath;
			for(auto const& token : text::tokenize(path.begin(), path.end(), '/'))
			{
				std::string tmp = token;
				for(auto const& subtoken : text::tokenize(tmp.begin(), tmp.end(), '.'))
				{
					if(subtoken.empty())
						continue;
					revPath.push_back(subtoken);
					std::replace(revPath.back().begin(), revPath.back().end(), ' ', '_');
				}
			}
			revPath.push_back("rev-path");
			revPath.push_back("attr");
			std::reverse(revPath.begin(), revPath.end());
			res.push_back(text::join(revPath, "."));
		}
		else
		{
			res.push_back("attr.untitled");
		}

		res.push_back(text::format("attr.os-version.%zu.%zu.%zu", oak::os_major(), oak::os_minor(), oak::os_patch()));
		return text::join(res, " ");
	}

} /* file */
