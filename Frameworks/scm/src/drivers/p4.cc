#include "api.h"
#include <oak/debug.h>

namespace scm
{
	struct p4_driver_t : driver_t
	{
		p4_driver_t () : driver_t("p4", "%s/.p4config") { }

		std::map<std::string, std::string> variables (std::string const& wcPath) const
		{
			return std::map<std::string, std::string>{ { "TM_SCM_NAME", name() } };
		}

		status_map_t status (std::string const& wcPath) const
		{
			return status_map_t();
		}
	};

	driver_t* p4_driver () { return new p4_driver_t; }
}
