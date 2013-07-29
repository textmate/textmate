#include "entries.h"
#include "path.h"
#include <regexp/glob.h>
#include <text/format.h>

namespace path
{
	static int skip_meta_entries (struct dirent const* entry)
	{
		std::string name(entry->d_name);
		return name != "." && name != "..";
	}

	entries::entries (std::string const& path, std::string const& globString)
	{
		struct dirent** entries;
		typedef int(*scandir_10_8)(const char *, struct dirent ***, int (*)(const struct dirent *), int (*)(const struct dirent **, const struct dirent **));
		int size = ((scandir_10_8)&scandir)(path.c_str(), &entries, &skip_meta_entries, NULL); // typecast required when building with the 10.7 SDK
		if(size != -1)
		{
			int actual = size;
			if(globString != NULL_STR)
			{
				size = 0;
				path::glob_t const glob(globString);
				for(size_t i = 0; i < actual; ++i)
				{
					if(glob.does_match(path::join(path, entries[i]->d_name)))
						std::swap(entries[size++], entries[i]);
				}
			}

			for(size_t i = 0; i < size; ++i)
			{
				if(entries[i]->d_type == 0) // NFS workaround
				{
					struct stat buf;
					if(lstat(path::join(path, entries[i]->d_name).c_str(), &buf) != -1)
					{
						if(S_ISDIR(buf.st_mode))
							entries[i]->d_type = DT_DIR;
						else if(S_ISREG(buf.st_mode))
							entries[i]->d_type = DT_REG;
						else if(S_ISLNK(buf.st_mode))
							entries[i]->d_type = DT_LNK;
					}
					else
					{
						perror(text::format("lstat(“%s/%s”)", path.c_str(), entries[i]->d_name).c_str());
					}
				}
			}
			_helper.reset(new helper_t(entries, size, actual));
		}
		else
		{
			perror(text::format("scandir(\"%s\")", path.c_str()).c_str());
		}
	}

} /* paht */
