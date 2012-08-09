#include "api.h"
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Perforce);

namespace scm
{
	struct p4_driver_t : driver_t
	{
		p4_driver_t () : driver_t("p4", "%s/.p4config") { }

		std::string branch_name (std::string const& wcPath) const
		{
			return NULL_STR;
		}

		status_map_t status (std::string const& wcPath) const
		{
			D(DBF_SCM_Perforce, bug("%s\n", wcPath.c_str()););
			return status_map_t();
		}
	};

	driver_t* p4_driver () { return new p4_driver_t; }
}
