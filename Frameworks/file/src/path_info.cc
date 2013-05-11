#include "path_info.h"
#include <scm/scm.h>
#include <settings/settings.h>
#include <io/entries.h>
#include <regexp/glob.h>
#include <plist/ascii.h>
#include <text/tokenize.h>
#include <oak/oak.h>
#include <oak/compat.h>

namespace file
{
	std::string path_attributes (std::string const& path, std::string const& dir)
	{
		std::vector<std::string> res;
		if(path != NULL_STR)
		{
			std::vector<std::string> revPath;
			citerate(token, text::tokenize(path.begin(), path.end(), '/'))
			{
				std::string tmp = *token;
				citerate(subtoken, text::tokenize(tmp.begin(), tmp.end(), '.'))
				{
					if((*subtoken).empty())
						continue;
					revPath.push_back(*subtoken);
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

		std::string const parentDir = dir == NULL_STR ? path::parent(path) : dir;
		res.push_back(settings_for_path(path, text::join(res, " "), parentDir).get(kSettingsScopeAttributesKey, ""));
		res.erase(std::remove(res.begin(), res.end(), ""), res.end());
		return text::join(res, " ");
	}

	std::map<std::string, std::string> path_variables (std::string const& path)
	{
		std::map<std::string, std::string> map;
		if(path != NULL_STR)
		{
			map["TM_DISPLAYNAME"] = path::display_name(path);
			map["TM_FILEPATH"]    = path;
			map["TM_FILENAME"]    = path::name(path);
			map["TM_DIRECTORY"]   = path::parent(path);
		}
		else
		{
			map["TM_DISPLAYNAME"] = "untitled";
		}
		return variables_for_path(map, path);
	}
	
} /* file */