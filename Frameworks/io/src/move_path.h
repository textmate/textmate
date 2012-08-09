#ifndef MOVE_PATH_H_PC1KPBCB
#define MOVE_PATH_H_PC1KPBCB

#include <oak/misc.h>

namespace path
{
	PUBLIC bool copy (std::string const& src, std::string const& dst);
	PUBLIC bool move (std::string const& src, std::string const& dst, bool overwrite = false);
	PUBLIC bool remove (std::string const& path);

} /* path */

#endif /* end of include guard: MOVE_PATH_H_PC1KPBCB */
