#include "entries.h"
#include "path.h"
#include <regexp/src/glob.h>
#include <text/src/format.h>

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
		int size = scandir(path.c_str(), &entries, &skip_meta_entries, nullptr);
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
						perrorf("path::entries: lstat(\"%s\")", path::join(path, entries[i]->d_name).c_str());
					}
				}
			}
			_helper = std::make_shared<helper_t>(entries, size, actual);
		}
		else
		{
			perrorf("path::entries: scandir(\"%s\")", path.c_str());
		}
	}

} /* paht */
