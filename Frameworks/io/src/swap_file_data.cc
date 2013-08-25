#include "swap_file_data.h"
#include "path.h"
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(IO_Swap_File_Data);

namespace path
{
	bool swap_and_unlink (std::string const& src, std::string const& dst)
	{
		D(DBF_IO_Swap_File_Data, bug("%s → %s\n", src.c_str(), dst.c_str()););
		ASSERT_EQ(access(src.c_str(), F_OK), 0);
		if(access(dst.c_str(), F_OK) != 0 && !path::make_dir(path::parent(dst)))
		{
			perror(text::format("mkdir_p(\"%s\")", path::parent(dst).c_str()).c_str());
			return false;
		}

		if(exchangedata(src.c_str(), dst.c_str(), 0) == 0)
		{
			bool res = unlink(src.c_str()) == 0;
			if(!res)
				perror(text::format("unlink(\"%s\")", src.c_str()).c_str());
			return res;
		}

		if(errno != ENOTSUP && errno != ENOENT && errno != EXDEV)
		{
			// ExpanDrive returns EFAULT
			fprintf(stderr, "warning: exchangedata(“%s”, “%s”) failed with “%s”, treating as “%s”.\n", src.c_str(), dst.c_str(), strerror(errno), strerror(ENOTSUP));
			errno = ENOTSUP;
		}

		D(DBF_IO_Swap_File_Data, bug("exchangedata() failed: %s\n", strerror(errno)););
		if(errno == ENOTSUP || errno == ENOENT)
		{
			if(errno == ENOTSUP && access(dst.c_str(), F_OK) == 0)
			{
				copyfile(dst.c_str(), src.c_str(), NULL, COPYFILE_METADATA);
				utimes(src.c_str(), NULL);
			}

			if(::rename(src.c_str(), dst.c_str()) == 0)
				return true;
			perror(text::format("rename(\"%s\", \"%s\")", src.c_str(), dst.c_str()).c_str());
			D(DBF_IO_Swap_File_Data, bug("rename() failed: %s\n", strerror(errno)););
		}

		if(errno == EXDEV)
		{
			// TODO this should copy to dst under a new name, then re-run swap_files
			if(copyfile(src.c_str(), dst.c_str(), NULL, COPYFILE_DATA|COPYFILE_MOVE) == 0)
			{
				bool res = unlink(src.c_str()) == 0;
				if(!res)
					perror(text::format("unlink(\"%s\")", src.c_str()).c_str());
				return res;
			}
			perror(text::format("copyfile(\"%s\", \"%s\", NULL, COPYFILE_DATA|COPYFILE_MOVE)", src.c_str(), dst.c_str()).c_str());
			D(DBF_IO_Swap_File_Data, bug("copyfile() failed: %s\n", strerror(errno)););
		}

		return false;
	}

} /* path */
