#ifndef MOVE_PATH_H_PC1KPBCB
#define MOVE_PATH_H_PC1KPBCB

namespace path
{
	bool copy (std::string const& src, std::string const& dst);
	bool move (std::string const& src, std::string const& dst, bool overwrite = false);
	bool remove (std::string const& path);

} /* path */

#endif /* end of include guard: MOVE_PATH_H_PC1KPBCB */
