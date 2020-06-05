#include "io.h"
#include "entries.h"
#include <oak/debug.h>

// ====================
// = Helper Functions =
// ====================

static bool send_move_request_to_auth_server (std::string const& src, std::string const& dst, bool overwrite)
{
	ASSERT(false);
	return false;
}

static bool is_copyable_dir (std::string const& path)
{
	if(access(path.c_str(), X_OK) != 0)
		return false;

	bool res = true;
	for(auto const& it : path::entries(path))
	{
		std::string const& newSrc = path::join(path, it->d_name);
		if(it->d_type == DT_REG)
		{
			if(access(newSrc.c_str(), R_OK) != 0)
				res = false;
		}
		else if(it->d_type == DT_DIR)
		{
			res = is_copyable_dir(newSrc);
		}
		else if(it->d_type == DT_LNK)
		{
			return true;
		}
		else
		{
			res = false;
		}

		if(!res)
			break;
	}
	return res;
}

static bool is_copyable (std::string const& path)
{
	struct stat buf;
	if(lstat(path.c_str(), &buf) != 0)
	{
	}
	else if(S_ISDIR(buf.st_mode))
	{
		return is_copyable_dir(path);
	}
	else if(S_ISLNK(buf.st_mode) || access(path.c_str(), R_OK) == 0)
	{
		return true;
	}
	return false;
}

// ====================
// = Public Functions =
// ====================

namespace path
{
	bool copy (std::string const& src, std::string const& dst)
	{
		ASSERTF(path::exists(src), "%s\n", src.c_str());
		ASSERTF(!path::exists(dst), "%s\n", dst.c_str());

		if(copyfile(src.c_str(), dst.c_str(), nullptr, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) != 0)
			return false;

		bool res = true;
		for(auto const& it : path::entries(src))
		{
			std::string const& newSrc = path::join(src, it->d_name);
			std::string const& newDst = path::join(dst, it->d_name);
			if(it->d_type == DT_DIR)
			{
				res = path::copy(newSrc, newDst) && res;
			}
			else if(it->d_type == DT_REG || it->d_type == DT_LNK)
			{
				if(copyfile(newSrc.c_str(), newDst.c_str(), nullptr, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) != 0)
					res = false;
			}
			else
			{
				res = false;
			}
		}
		return res;
	}

	bool move (std::string const& src, std::string const& dst, bool overwrite)
	{
		std::string const& dst_parent = path::parent(dst);
		std::string const& src_parent = path::parent(src);
		bool src_exists DB_VAR = path::exists(src);
		bool dst_exists = path::exists(dst);

		ASSERT(src_exists);

		if(dst_exists && !overwrite)
		{
			errno = EEXIST;
			return false;
		}

		if(!path::make_dir(dst_parent))
			return false;

		dev_t src_device = path::device(src);
		dev_t dst_device = path::device(dst_parent);

		if(dst_exists)
		{
			if(!path::remove(dst))
				return false; // if errno == EACCES then send_move_request_to_auth_server()
		}

		// when src and dst are on the same device we don’t need to ensure that ‘is_copyable(src)’
		if(!is_copyable(src) || !path::is_writable(src_parent) || !path::is_writable(dst_parent))
			return send_move_request_to_auth_server(src, dst, overwrite);

		if(src_device == dst_device)
		{
			if(::rename(src.c_str(), dst.c_str()) == 0)
				return true;
		}
		else
		{
			return path::copy(src, dst) && path::remove(src);
		}
		return false;
	}

	static bool remove_dir (std::string const& path)
	{
		bool res = true;
		for(auto const& it : path::entries(path))
		{
			std::string const& newSrc = path::join(path, it->d_name);
			if(it->d_type == DT_DIR)
			{
				res = path::remove_dir(newSrc) && res;
			}
			else
			{
				if(unlink(newSrc.c_str()) != 0)
				{
					res = false;
				}
			}
		}

		if(res && rmdir(path.c_str()) != 0)
		{
			res = false;
		}
		return res;
	}

	bool remove (std::string const& path)
	{
		if(path == NULL_STR)
			return false;

		struct stat buf;
		if(lstat(path.c_str(), &buf) != 0)
		{
		}
		else if(S_ISDIR(buf.st_mode))
		{
			return path::remove_dir(path);
		}
		else if(unlink(path.c_str()) == 0)
		{
			return true;
		}
		return false;
	}

} /* path */
