#include "swap_file_data.h"
#include "path.h"
#include <oak/debug.h>

OAK_DEBUG_VAR(IO_Swap_File_Data);

namespace path
{
	bool swap_and_unlink (std::string const& src, std::string const& dst)
	{
		D(DBF_IO_Swap_File_Data, bug("%s → %s\n", src.c_str(), dst.c_str()););
		ASSERT_EQ(access(src.c_str(), F_OK), 0);
		if(access(dst.c_str(), F_OK) != 0 && !path::make_dir(path::parent(dst)))
			return false;

		if(exchangedata(src.c_str(), dst.c_str(), 0) == 0)
			return unlink(src.c_str()) == 0;

		if(errno == EFAULT) // Workaround for ExpanDrive
			errno = ENOTSUP;

		D(DBF_IO_Swap_File_Data, bug("exchangedata() failed: %s\n", strerror(errno)););
		if(errno == ENOTSUP || errno == ENOENT)
		{
			if(errno == ENOTSUP && access(dst.c_str(), F_OK) == 0)
			{
#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
				copyfile(dst.c_str(), src.c_str(), NULL, COPYFILE_METADATA);
				utimes(src.c_str(), NULL);
#endif
			}

			if(::rename(src.c_str(), dst.c_str()) == 0)
				return true;
			D(DBF_IO_Swap_File_Data, bug("rename() failed: %s\n", strerror(errno)););
		}

		if(errno == EXDEV)
		{
			// TODO this should copy to dst under a new name, then re-run swap_files
#if MAC_OS_X_VERSION_MAX_ALLOWED > MAC_OS_X_VERSION_10_4
			if(copyfile(src.c_str(), dst.c_str(), NULL, COPYFILE_DATA|COPYFILE_MOVE) == 0)
				return unlink(src.c_str()) == 0;
			D(DBF_IO_Swap_File_Data, bug("copyfile() failed: %s\n", strerror(errno)););
#else
			// FIXME we need a fallback copy
#endif
		}

		return false;
	}

} /* path */
