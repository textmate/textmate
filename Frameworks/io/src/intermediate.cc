#include "intermediate.h"
#include "path.h"
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(IO_Intermediate);
OAK_DEBUG_VAR(IO_Swap_File_Data);

__attribute__ ((format (printf, 1, 2))) static std::string format_error (char const* format, ...)
{
	char* err = strerror(errno);
	char* msg = nullptr;

	va_list ap;
	va_start(ap, format);
	vasprintf(&msg, format, ap);
	va_end(ap);

	char* res = nullptr;
	asprintf(&res, "%s: %s\n", msg, err);
	std::string str(res);
	free(res);
	free(msg);
	return str;
}

static bool swap_and_unlink (std::string const& src, std::string const& dst, std::string& errorMsg)
{
	D(DBF_IO_Swap_File_Data, bug("%s → %s\n", src.c_str(), dst.c_str()););
	ASSERT_EQ(access(src.c_str(), F_OK), 0);
	if(access(dst.c_str(), F_OK) != 0 && !path::make_dir(path::parent(dst)))
	{
		errorMsg = format_error("mkdir_p(\"%s\")", path::parent(dst).c_str());
		return false;
	}

	if(exchangedata(src.c_str(), dst.c_str(), 0) == 0)
	{
		bool res = unlink(src.c_str()) == 0;
		if(!res)
			errorMsg = format_error("unlink(\"%s\")", src.c_str());
		return res;
	}

	if(errno != ENOTSUP && errno != ENOENT && errno != EXDEV)
	{
		// ExpanDrive returns EFAULT
		perrorf("exchangedata(\"%s\", \"%s\")", src.c_str(), dst.c_str());
		errno = ENOTSUP;
	}

	D(DBF_IO_Swap_File_Data, bug("exchangedata() failed: %s\n", strerror(errno)););
	if(errno == ENOTSUP || errno == ENOENT)
	{
		if(errno == ENOTSUP && access(dst.c_str(), F_OK) == 0)
		{
			// Skip COPYFILE_METADATA for network drives <rdar://17480649>
			if(path::is_local(src))
			{
				copyfile(dst.c_str(), src.c_str(), nullptr, COPYFILE_METADATA);
				utimes(src.c_str(), nullptr);
			}
			else
			{
				struct stat sbuf;
				if(stat(dst.c_str(), &sbuf) == 0)
					chmod(src.c_str(), sbuf.st_mode & (S_IRWXU|S_IRWXG|S_IRWXO));
			}
		}

		if(::rename(src.c_str(), dst.c_str()) == 0)
			return true;
		errorMsg = format_error("rename(\"%s\", \"%s\")", src.c_str(), dst.c_str());
		D(DBF_IO_Swap_File_Data, bug("rename() failed: %s\n", strerror(errno)););
	}

	if(errno == EXDEV)
	{
		// TODO this should copy to dst under a new name, then re-run swap_files
		if(copyfile(src.c_str(), dst.c_str(), nullptr, COPYFILE_DATA|COPYFILE_MOVE) == 0)
		{
			bool res = unlink(src.c_str()) == 0;
			if(!res)
				errorMsg = format_error("unlink(\"%s\")", src.c_str());
			return res;
		}
		errorMsg = format_error("copyfile(\"%s\", \"%s\", nullptr, COPYFILE_DATA|COPYFILE_MOVE)", src.c_str(), dst.c_str());
		D(DBF_IO_Swap_File_Data, bug("copyfile() failed: %s\n", strerror(errno)););
	}

	return false;
}

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
	intermediate_t::intermediate_t (std::string const& dest) : _resolved(path::resolve_head(dest)), _intermediate(create_path(_resolved))
	{
		D(DBF_IO_Intermediate, bug("%s → %s → %s\n", dest.c_str(), _resolved.c_str(), _intermediate.c_str()););
	}

	bool intermediate_t::commit (std::string* errorMsg) const
	{
		D(DBF_IO_Intermediate, bug("%s ⇔ %s (swap: %s)\n", _resolved.c_str(), _intermediate.c_str(), BSTR(_intermediate != _resolved)););
		std::string unused;
		return _intermediate == _resolved ? true : swap_and_unlink(_intermediate, _resolved, errorMsg ? *errorMsg : unused);
	}

} /* path */
