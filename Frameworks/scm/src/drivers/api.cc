#include "api.h"
#include <settings/settings.h>
#include <text/case.h>
#include <text/format.h>
#include <text/tokenize.h>
#include <io/path.h>
#include <oak/oak.h>

namespace scm
{
	std::string find_executable (std::string const& name, std::string const& variable)
	{
		std::map<std::string, std::string> const& variables = variables_for_path(oak::basic_environment());
		std::vector<std::string> candidates;

		std::map<std::string, std::string>::const_iterator exe = variables.find(variable);
		if(exe != variables.end())
			candidates.push_back(exe->second);

		if(char const* exe = getenv(variable.c_str()))
			candidates.push_back(exe);

		std::map<std::string, std::string>::const_iterator pathList = variables.find("PATH");
		if(pathList != variables.end())
		{
			for(auto const& path : text::tokenize(pathList->second.begin(), pathList->second.end(), ':'))
				candidates.push_back(path::join(path, name));
		}

		if(char const* pathList = getenv("PATH"))
		{
			for(auto const& path : text::tokenize(pathList, pathList + strlen(pathList), ':'))
				candidates.push_back(path::join(path, name));
		}

		for(auto const& path : candidates)
		{
			if(path::is_executable(path))
				return path;
		}

		return NULL_STR;
	}

	driver_t::driver_t (std::string const& name, std::string const& wcRootFormatString, std::string const& requiredExecutable) : _name(name), _wc_root_format_string(wcRootFormatString), _required_executable(requiredExecutable), _resolved_executable(NULL_STR)
	{
	}

	bool driver_t::has_info_for_directory (std::string const& path)
	{
		bool res = path::exists(text::format(_wc_root_format_string.c_str(), path.c_str()));
		if(res)
			setup();
		return res;
	}

	void driver_t::setup ()
	{
		if(_resolved_executable == NULL_STR && _required_executable != NULL_STR)
		{
			_resolved_executable = find_executable(_required_executable, "TM_" + text::uppercase(_name));
			if(_resolved_executable == NULL_STR)
				fprintf(stderr, "scm: unable to find ‘%s’ executable (set %s or PATH in ~/.tm_properties)\n", _required_executable.c_str(), ("TM_" + text::uppercase(_name)).c_str());
		}
	}

	driver_t* git_driver ();
	driver_t* hg_driver ();
	driver_t* p4_driver ();
	driver_t* svn_driver ();

	driver_t const* driver_for_path (std::string const& path, std::string* wcPath)
	{
		if(!path::is_absolute(path))
			return NULL;

		static driver_t* const drivers[] = { git_driver(), hg_driver(), p4_driver(), svn_driver() };
		for(std::string cwd = path; cwd != "/"; cwd = path::parent(cwd))
		{
			for(auto const& driver : drivers)
			{
				if(driver && driver->has_info_for_directory(cwd))
				{
					if(wcPath)
						*wcPath = cwd;
					return driver;
				}
			}
		}
		return NULL;
	}

} /* scm */