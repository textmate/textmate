#include "api.h"
#include <settings/settings.h>
#include <text/case.h>
#include <text/format.h>
#include <text/tokenize.h>
#include <oak/oak.h>

static std::string find_executable (std::string const& name, std::string const& variable)
{
	std::map<std::string, std::string> const& variables = variables_for_path();
	std::vector<std::string> candidates;

	std::map<std::string, std::string>::const_iterator exe = variables.find(variable);
	if(exe != variables.end())
		candidates.push_back(exe->second);

	if(char const* exe = getenv(variable.c_str()))
		candidates.push_back(exe);

	std::map<std::string, std::string>::const_iterator pathList = variables.find("PATH");
	if(pathList != variables.end())
	{
		citerate(path, text::tokenize(pathList->second.begin(), pathList->second.end(), ':'))
			candidates.push_back(path::join(*path, name));
	}

	if(char const* pathList = getenv("PATH"))
	{
		citerate(path, text::tokenize(pathList, pathList + strlen(pathList), ':'))
			candidates.push_back(path::join(*path, name));
	}

	iterate(path, candidates)
	{
		if(path::is_executable(*path))
			return *path;
	}

	return NULL_STR;
}

namespace scm
{
	namespace status
	{
		std::string to_s (type status)
		{
			switch(status)
			{
				case none:         return "N";
				case added:        return "A";
				case versioned:    return "H";
				case modified:     return "M";
				case ignored:      return "I";
				case deleted:      return "D";
				case mixed:        return "X";
				case unversioned:  return "U";
				default:           return text::format("unknown (%d)", status);
			}
		}
	}

	// ============
	// = driver_t =
	// ============

	driver_t::driver_t (std::string const& name, std::string const& wcRootFormatString, std::string const& requiredExecutable) : _name(name), _wc_root_format_string(wcRootFormatString), _required_executable(requiredExecutable), _resolved_executable(NULL_STR)
	{
	}

	driver_t* git_driver ();
	driver_t* hg_driver ();
	driver_t* p4_driver ();

	driver_t const* driver_for_path (std::string const& path, std::string* wcPath)
	{
		if(path == NULL_STR || path == "" || path[0] != '/')
			return NULL;

		static driver_t* const drivers[] = { git_driver(), hg_driver(), p4_driver() };
		for(std::string cwd = path; cwd != "/"; cwd = path::parent(cwd))
		{
			iterate(driver, drivers)
			{
				if(*driver && path::exists(text::format((*driver)->_wc_root_format_string.c_str(), cwd.c_str())))
				{
					if(wcPath)
						*wcPath = cwd;
					if((*driver)->_resolved_executable == NULL_STR && (*driver)->_required_executable != NULL_STR)
					{
						(*driver)->_resolved_executable = find_executable((*driver)->_required_executable, "TM_" + text::uppercase((*driver)->_name));
						if((*driver)->_resolved_executable == NULL_STR)
							fprintf(stderr, "scm: unable to find ‘%s’ executable (set %s or PATH in ~/.tm_properties)\n", (*driver)->_required_executable.c_str(), ("TM_" + text::uppercase((*driver)->_name)).c_str());
					}
					return *driver;
				}
			}
		}
		return NULL;
	}

} /* scm */