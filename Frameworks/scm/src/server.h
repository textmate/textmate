#ifndef SCM_SERVER_H_BND4Y51
#define SCM_SERVER_H_BND4Y51

#include "snapshot.h"
#include "scm.h"
#include <oak/misc.h>

namespace scm
{
	PUBLIC void background_status (std::string const& path, driver_t const* driver, oak::date_t const& updated, fs::snapshot_t const& snapshot, void(*callback)(bool, std::string const&, fs::snapshot_t const&, scm::status_map_t const&));

} /* scm */

#endif /* end of include guard: SCM_SERVER_H_BND4Y51 */
