#include "status.h"
#include <io/src/path.h>

namespace file
{
	file_status_t status (std::string const& path)
	{
		if(access(path.c_str(), W_OK) == 0)
		{
			return kFileTestWritable;
		}
		else if(errno == EROFS)
		{
			return kFileTestReadOnly;
		}
		else if(errno == ENOENT)
		{
			if(access(path::parent(path).c_str(), W_OK) == 0)
				return kFileTestWritable;
			else if(errno == EROFS)
				return kFileTestReadOnly;
			else if(errno == ENOENT)
				return kFileTestNoParent;
			else if(errno == EACCES)
				return kFileTestWritableByRoot; // ???
			else
				perrorf("file::status: access(\"%s\", W_OK)", path::parent(path).c_str());
		}
		else if(errno == EACCES)
		{
			struct stat sbuf;
			if(stat(path.c_str(), &sbuf) == 0)
			{
				if((sbuf.st_mode & S_IWUSR) == 0)
				{
					if(sbuf.st_uid == getuid())
							return kFileTestNotWritableButOwner;
					else	return kFileTestNotWritable;
				}
				else if(sbuf.st_uid != getuid())
				{
					return kFileTestWritableByRoot; // ???
				}
				else
				{
					os_log_error(OS_LOG_DEFAULT, "Unexpected file mode %x", sbuf.st_mode);
				}
			}
			else if(errno == EACCES)
			{
				return kFileTestWritableByRoot;
			}
			else
			{
				perrorf("file::status: stat(\"%s\")", path.c_str());
			}
		}
		else
		{
			perrorf("file::status: access(\"%s\", W_OK)", path.c_str());
		}
		return kFileTestUnhandled;
	}

} /* file */
