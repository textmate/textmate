#ifndef FS_CONTROLLER_H_GF1Y738P
#define FS_CONTROLLER_H_GF1Y738P

#include "fs_tree.h"
#include <regexp/glob.h>

namespace fs
{
	struct callback_t
	{
		virtual void did_change (std::map<std::string, fs::node_t> const& heads, std::map< std::string, std::vector<std::string> > const& changes) = 0;
	};

	struct watch_info_t;
	typedef std::shared_ptr<watch_info_t> watch_info_ptr;

	watch_info_ptr watch_paths (std::set<std::string> rootPaths, callback_t* callback, path::glob_t const& dirGlob = "*", path::glob_t const& fileGlob = "*", std::string const& cacheFile = NULL_STR);

} /* fs */

#endif /* end of include guard: FS_CONTROLLER_H_GF1Y738P */
