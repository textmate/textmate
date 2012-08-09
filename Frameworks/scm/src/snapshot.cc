#include "snapshot.h"
#include <io/path.h>
#include <io/entries.h>

namespace fs
{
	snapshot_t::snapshot_t ()
	{
	}

	snapshot_t::snapshot_t (std::string const& path) : _entries(collect(path))
	{
	}

	time_t snapshot_t::modified (std::string const& path)
	{
		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
			return buf.st_mtimespec.tv_sec;
		return 0;
	}

	snapshot_t::nodes_ptr snapshot_t::collect (std::string const& dir)
	{
		nodes_ptr res(new std::map<ino_t, node_t>);
		citerate(entry, path::entries(dir))
		{
			std::string const path = path::join(dir, (*entry)->d_name);
			if((*entry)->d_type == DT_REG)
				res->insert(std::make_pair((*entry)->d_ino, node_t((*entry)->d_name, node_t::kNodeTypeFile, modified(path))));
			else if((*entry)->d_type == DT_LNK)
				res->insert(std::make_pair((*entry)->d_ino, node_t((*entry)->d_name, node_t::kNodeTypeLink, modified(path))));
			else if((*entry)->d_type == DT_DIR)
				res->insert(std::make_pair((*entry)->d_ino, node_t((*entry)->d_name, node_t::kNodeTypeDirectory, modified(path), collect(path))));
		}
		return res;
	}

	void snapshot_t::node_t::to_s (size_t indent) const
	{
		char const* typeStr = "OTR";
		switch(type())
		{
			case node_t::kNodeTypeDirectory: typeStr = "DIR"; break;
			case node_t::kNodeTypeLink:      typeStr = "LNK"; break;
			case node_t::kNodeTypeFile:      typeStr = "REG"; break;
		}

		fprintf(stderr, "%s[%s] %s\n", std::string(indent, ' ').c_str(), typeStr, name().c_str());

		if(type() == node_t::kNodeTypeDirectory)
		{
			citerate(pair, *entries())
				pair->second.to_s(indent + 6);
		}
	}

	void to_s (snapshot_t const& snapshot)
	{
		iterate(pair, *snapshot._entries)
			pair->second.to_s();
	}

} /* fs */
