#include "snapshot.h"
#include <io/src/path.h>
#include <io/src/entries.h>

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
		auto res = std::make_shared<std::map<ino_t, node_t>>();
		for(auto const& entry : path::entries(dir))
		{
			std::string const path = path::join(dir, entry->d_name);
			if(entry->d_type == DT_REG)
				res->insert(std::make_pair(entry->d_ino, node_t(entry->d_name, node_t::kNodeTypeFile, modified(path))));
			else if(entry->d_type == DT_LNK)
				res->insert(std::make_pair(entry->d_ino, node_t(entry->d_name, node_t::kNodeTypeLink, modified(path))));
			else if(entry->d_type == DT_DIR)
				res->insert(std::make_pair(entry->d_ino, node_t(entry->d_name, node_t::kNodeTypeDirectory, modified(path), collect(path))));
		}
		return res;
	}

	std::string snapshot_t::node_t::to_s (size_t indent) const
	{
		char const* typeStr = "[OTR]";
		switch(type())
		{
			case node_t::kNodeTypeDirectory: typeStr = "[DIR]"; break;
			case node_t::kNodeTypeLink:      typeStr = "[LNK]"; break;
			case node_t::kNodeTypeFile:      typeStr = "[REG]"; break;
		}

		std::string res = std::string(indent, ' ') + typeStr + " " + name() + "\n";
		if(type() == node_t::kNodeTypeDirectory)
		{
			for(auto pair : *entries())
				res += pair.second.to_s(indent + 6);
		}
		return res;
	}

	std::string to_s (snapshot_t const& snapshot)
	{
		std::string res = "";
		for(auto pair : *snapshot._entries)
			res += pair.second.to_s();
		return res;
	}

} /* fs */
