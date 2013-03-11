#ifndef SCM_DRIVERS_API_H_5RTC8RYO
#define SCM_DRIVERS_API_H_5RTC8RYO

#include "../status.h"
#include <oak/misc.h>

namespace scm
{
	struct driver_t
	{
		driver_t (std::string const& name, std::string const& wcRootFormatString, std::string const& requiredExecutable = NULL_STR);

		virtual std::map<std::string, std::string> variables (std::string const& wcPath) const = 0;
		virtual status_map_t status (std::string const& wcPath) const = 0;

		std::string const& name () const           { return _name; }
		virtual bool tracks_directories () const   { return false; }
		virtual bool may_touch_filesystem () const { return false; }

		bool has_info_for_directory (std::string const& path);

	protected:
		std::string const& executable () const { return _resolved_executable; }
		void setup ();

	private:
		std::string _name;
		std::string _wc_root_format_string;
		std::string _required_executable;
		std::string _resolved_executable;
	};

	std::string find_executable (std::string const& name, std::string const& variable);
	driver_t const* driver_for_path (std::string const& path, std::string* wcPath);

} /* scm */

#endif /* end of include guard: SCM_DRIVERS_API_H_5RTC8RYO */
