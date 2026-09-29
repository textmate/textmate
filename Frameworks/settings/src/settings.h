#ifndef SETTINGS_H_F99MMG5F
#define SETTINGS_H_F99MMG5F

#include "keys.h"
#include <io/src/io.h>
#include <oak/oak.h>
#include <text/src/format.h>
#include <scope/src/scope.h>

struct settings_t
{
	settings_t ()                                                                        { }
	settings_t (std::map<std::string, std::string> const& settings) : settings(settings) { }

	bool has (std::string const& key) const
	{
		return settings.find(key) != settings.end();
	}

	template <typename T> T get (std::string const& key, T const& defaultValue) const
	{
		std::map<std::string, std::string>::const_iterator it = settings.find(key);
		return it == settings.end() ? defaultValue : convert(it->second, defaultValue);
	}

	std::string get (std::string const& key, char const* defaultValue = NULL_STR) const
	{
		return get<std::string>(key, defaultValue);
	}

	std::map<std::string, std::string> all_settings () const
	{
		return settings;
	}

	static std::string raw_get (std::string const& key, std::string const& section = "");

	static void set (std::string const& key, std::string const& value, std::string const& fileType = "", std::string const& path = NULL_STR);
	static void set (std::string const& key, size_t number,   std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::to_string(number),               fileType, path); }
	static void set (std::string const& key, int32_t number,  std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::to_string(number),               fileType, path); }
	static void set (std::string const& key, bool flag,       std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::string(flag ? "true" : "false"), fileType, path); }
	static void set (std::string const& key, char const* str, std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::string(str),                     fileType, path); }

	static void set (std::string const& key, double decimal,  std::string const& fileType = "", std::string const& path = NULL_STR)
	{
		if(decimal == roundl(decimal))
				settings_t::set(key, (int32_t)roundl(decimal), fileType, path);
		else	settings_t::set(key, std::to_string(decimal), fileType, path);
	}

	static void set_default_settings_path (std::string const& path);
	static void set_global_settings_path (std::string const& path);

private:
	std::map<std::string, std::string> settings;

	static bool convert (std::string const& value, bool)                             { return value != "0" && value != "false" ? true : false; }
	static int32_t convert (std::string const& value, int32_t)                       { return atoi(value.c_str());                             }
	static double convert (std::string const& value, double)                         { return atof(value.c_str());                             }
	static std::string const& convert (std::string const& value, std::string const&) { return value;                                           }
	static char const* convert (std::string const& value, char const*)               { return value.c_str();                                   }

	static std::string to_str (bool value)                      { return value ? "1" : "0";         }
	static std::string to_str (int32_t value)                   { return std::to_string(value);     }
	static std::string to_str (double value)                    { return std::to_string(value);     }
	static std::string to_str (std::string const& value)        { return value;                     }
	static std::string to_str (char const* value)               { return value;                     }
};

settings_t settings_for_path (std::string const& path = NULL_STR, scope::scope_t const& scope = "", std::string const& directory = NULL_STR, std::map<std::string, std::string> variables = std::map<std::string, std::string>());
std::map<std::string, std::string> variables_for_path (std::map<std::string, std::string> const& base = std::map<std::string, std::string>(), std::string const& path = NULL_STR, scope::scope_t const& scope = "", std::string const& directory = NULL_STR);

struct setting_info_t
{
	setting_info_t (std::string const& variable, std::string const& value, std::string const& path, size_t lineNumber, std::string const& section) : variable(variable), value(value), path(path), line_number(lineNumber), section(section) { }

	std::string variable;
	std::string value;
	std::string path;
	size_t line_number;
	std::string section;
};

std::vector<setting_info_t> settings_info_for_path (std::string const& path = NULL_STR, scope::scope_t const& scope = "", std::string const& directory = NULL_STR);

#endif /* end of include guard: SETTINGS_H_F99MMG5F */
