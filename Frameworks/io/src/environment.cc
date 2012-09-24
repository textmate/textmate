#include "environment.h"
#include "path.h"
#include <oak/oak.h>
#include <crt_externs.h>

namespace oak
{
	static bool include_variable (std::string const& variable)
	{
		static std::set<std::string> const WhiteListedVariables = { "COMMAND_MODE", "SHELL", "SHLVL", "SSH_AUTH_SOCK", "__CF_USER_TEXT_ENCODING" };
		return variable.find("Apple") == 0 || WhiteListedVariables.find(variable) != WhiteListedVariables.end();
	}

	std::map<std::string, std::string> setup_basic_environment ()
	{
		passwd* entry = path::passwd_entry();

		int mib[2] = { CTL_USER, USER_CS_PATH };
		size_t len = 0;
		sysctl(mib, 2, NULL, &len, NULL, 0);
		char* path = new char[len];
		sysctl(mib, 2, path, &len, NULL, 0);

		std::map<std::string, std::string> res;

		char*** envPtr = _NSGetEnviron();
		for(char** pair = *envPtr; pair && *pair; ++pair)
		{
			char* value = strchr(*pair, '=');
			if(value && *value == '=' && include_variable(std::string(*pair, value)))
				res[std::string(*pair, value)] = value + 1;
		}

		res["HOME"]     = entry->pw_dir;
		res["PATH"]     = path;
		res["TMPDIR"]   = path::temp();
		res["LOGNAME"]  = entry->pw_name;
		res["USER"]     = entry->pw_name;

		return res;
	}

	std::map<std::string, std::string>& rw_environment ()
	{
		static std::map<std::string, std::string>* environment = new std::map<std::string, std::string>(setup_basic_environment());
		return *environment;
	}

	std::map<std::string, std::string> const& basic_environment ()
	{
		return rw_environment();
	}

	void set_basic_environment (std::map<std::string, std::string> const& newEnvironment)
	{
		rw_environment() = newEnvironment;
	}

} /* io */