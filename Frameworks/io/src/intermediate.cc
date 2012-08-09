#include "intermediate.h"
#include "swap_file_data.h"
#include "path.h"
#include <oak/debug.h>

OAK_DEBUG_VAR(IO_Intermediate);

static std::string create_path (std::string const& path)
{
	if(!path::exists(path))
		return path::make_dir(path::parent(path)), path;
	else if(path::device(path) != path::device(path::temp()) && access(path::parent(path).c_str(), W_OK) == 0)
		return path + "~";
	return path::temp("atomic_save");
}

namespace path
{
	intermediate_t::intermediate_t (std::string const& dest)
	{
		_resolved     = path::resolve_head(dest);
		_intermediate = create_path(_resolved);
		D(DBF_IO_Intermediate, bug("%s → %s → %s\n", dest.c_str(), _resolved.c_str(), _intermediate.c_str()););
	}

	bool intermediate_t::commit () const
	{
		D(DBF_IO_Intermediate, bug("%s ⇔ %s (swap: %s)\n", _resolved.c_str(), _intermediate.c_str(), BSTR(_intermediate != _resolved)););
		return _intermediate == _resolved ? true : path::swap_and_unlink(_intermediate, _resolved);
	}

} /* path */
