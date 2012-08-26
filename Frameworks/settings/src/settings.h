#ifndef SETTINGS_H_F99MMG5F
#define SETTINGS_H_F99MMG5F

#include "keys.h"
#include <io/io.h>
#include <oak/oak.h>
#include <text/format.h>
#include <scope/scope.h>
#include <plist/uuid.h>

struct PUBLIC settings_t
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

	static std::string raw_get (std::string const& key, std::string const& section = "");

	static void set (std::string const& key, std::string const& value, std::string const& fileType = "", std::string const& path = NULL_STR);
	static void set (std::string const& key, double decimal,  std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, text::format("%f", decimal),          fileType, path); }
	static void set (std::string const& key, size_t number,   std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, text::format("%zu", number),          fileType, path); }
	static void set (std::string const& key, int32_t number,  std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, text::format("%d", number),           fileType, path); }
	static void set (std::string const& key, bool flag,       std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::string(flag ? "true" : "false"), fileType, path); }
	static void set (std::string const& key, char const* str, std::string const& fileType = "", std::string const& path = NULL_STR) { settings_t::set(key, std::string(str),                     fileType, path); }

private:
	std::map<std::string, std::string> settings;

	static bool convert (std::string const& value, bool)                             { return value != "0" && value != "false" ? true : false; }
	static int32_t convert (std::string const& value, int32_t)                       { return atoi(value.c_str());                             }
	static double convert (std::string const& value, double)                         { return atof(value.c_str());                             }
	static std::string const& convert (std::string const& value, std::string const&) { return value;                                           }
	static char const* convert (std::string const& value, char const*)               { return value.c_str();                                   }

	static std::string to_str (bool value)                      { return value ? "1" : "0";         }
	static std::string to_str (int32_t value)                   { return text::format("%d", value); }
	static std::string to_str (double value)                    { return text::format("%f", value); }
	static std::string to_str (std::string const& value)        { return value;                     }
	static std::string to_str (char const* value)               { return value;                     }
};

PUBLIC settings_t settings_for_path (std::string const& path = NULL_STR, scope::scope_t const& scope = "", std::string const& directory = NULL_STR, oak::uuid_t const& uuid = oak::uuid_t(), std::map<std::string, std::string> variables = std::map<std::string, std::string>());
PUBLIC std::map<std::string, std::string> variables_for_path (std::string const& documentPath = NULL_STR, scope::scope_t const& scope = "", std::map<std::string, std::string> existingVariables = std::map<std::string, std::string>());

#endif /* end of include guard: SETTINGS_H_F99MMG5F */
