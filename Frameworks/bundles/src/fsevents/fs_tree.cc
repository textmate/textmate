#include "fs_tree.h"
#include <io/entries.h>
#include <text/format.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(FS_Tree);

namespace fs
{
	node_t::node_t (std::string const& path, bool scan) : _name(path::name(path)), _resolved(NULL_STR), _modified(0)
	{
		struct stat buf;
		if(lstat(path.c_str(), &buf) == 0)
		{
			_modified = buf.st_mtimespec.tv_sec;
			if(S_ISDIR(buf.st_mode))
			{
				_type = kNodeTypeDirectory;
				_entries.reset(new std::vector<node_t>);

				if(scan)
					rescan(path::parent(path), "*", "*", NULL);
			}
			else if(S_ISREG(buf.st_mode))
			{
				_type = kNodeTypeFile;
			}
			else if(S_ISLNK(buf.st_mode))
			{
				_type = kNodeTypeLink;
				char buf[1024];
				ssize_t len = readlink(path.c_str(), &buf[0], sizeof(buf));
				if(len != -1 && len <= sizeof(buf))
						_resolved = std::string(buf, buf + len);
				else	perror(text::format("readlink(‘%s’)", path.c_str()).c_str());
			}
		}
		else if(errno == ENOENT)
		{
			_type = kNodeTypeMissing;
		}
		else
		{
			perror(text::format("lstat(‘%s’)", path.c_str()).c_str());
		}
	}

	struct value_t // helper type for rescan()
	{
		value_t (time_t old_date, nodes_ptr const& entries, time_t new_date = 0) : old_date(old_date), entries(entries), new_date(new_date) { }

		time_t old_date;
		nodes_ptr entries;
		time_t new_date;
	};

	node_t& node_t::rescan (std::string const& cwd, path::glob_t const& dirGlob, path::glob_t const& fileGlob, std::map< std::string, std::vector<std::string> >* changes)
	{
		if(_type != kNodeTypeDirectory)
			return *this;

		typedef boost::tuple<std::string, std::string, node_type_t> key_t;
		std::map<key_t, value_t> entries;
		iterate(entry, *_entries)
			entries.insert(std::make_pair(key_t(entry->_name, entry->_resolved, entry->_type), value_t(entry->_modified, entry->_entries)));

		std::string const basePath = path::join(cwd, _name);
		citerate(entry, path::entries(basePath))
		{
			std::string path = path::join(basePath, (*entry)->d_name);
			int type = (*entry)->d_type;
			if(type == DT_DIR && dirGlob.does_match(path) || type == DT_REG && fileGlob.does_match(path) || type == DT_LNK)
			{
				node_t node(path, false);
				key_t key(node._name, node._resolved, node._type);
				std::map<key_t, value_t>::iterator it = entries.find(key);
				if(it != entries.end())
						it->second.new_date = node._modified;
				else	entries.insert(std::make_pair(key, value_t(0, node._entries, node._modified)));
			}
			else
			{
				D(DBF_FS_Tree, bug("ignore: %s\n", path.c_str()););
			}
		}

		_entries->clear();
		iterate(pair, entries)
		{
			if(pair->second.new_date == 0)                          { D(DBF_FS_Tree, bug("--- %s\n", path::join(basePath, pair->first.get<0>()).c_str());); }
			else if(pair->second.old_date == 0)                     { D(DBF_FS_Tree, bug("+++ %s\n", path::join(basePath, pair->first.get<0>()).c_str());); }
			else if(pair->second.old_date != pair->second.new_date) { D(DBF_FS_Tree, bug("    %s\n", path::join(basePath, pair->first.get<0>()).c_str());); }

			if(changes)
			{
				if(pair->second.new_date == 0)                          { (*changes)["deleted"].push_back(path::join(basePath, pair->first.get<0>())); }
				else if(pair->second.old_date == 0)                     { (*changes)["created"].push_back(path::join(basePath, pair->first.get<0>())); }
				else if(pair->second.old_date != pair->second.new_date) { (*changes)["changed"].push_back(path::join(basePath, pair->first.get<0>())); }
			}

			if(pair->second.new_date == 0)
				continue;

			_entries->push_back(node_t(pair->first.get<0>(), pair->first.get<1>(), pair->first.get<2>(), pair->second.new_date, pair->second.entries));
			if(pair->second.old_date != pair->second.new_date)
				_entries->back().rescan(basePath, dirGlob, fileGlob, changes);
		}
		return *this;
	}

	// =========================
	// = to/from property list =
	// =========================

	plist::dictionary_t to_plist (node_t const& node)
	{
		plist::dictionary_t res;

		res["name"] = node._name;
		if(node._resolved != NULL_STR)
			res["link"] = node._resolved;

		if(node._type != node_t::kNodeTypeMissing)
				res["date"] = oak::date_t(node._modified);
		else	res["missing"] = true;

		if(node._type == node_t::kNodeTypeDirectory)
		{
			plist::array_t nodes;
			iterate(entry, *node._entries)
				nodes.push_back(to_plist(*entry));
			res["nodes"] = nodes;
		}

		return res;
	}

	node_t from_plist (plist::any_t const& plist)
	{
		std::string name, link = NULL_STR;
		bool missing = false;
		oak::date_t date;
		node_t::node_type_t type = node_t::kNodeTypeFile;
		plist::array_t entries;

		plist::get_key_path(plist, "name", name);
		plist::get_key_path(plist, "link", link);
		plist::get_key_path(plist, "date", date);
		plist::get_key_path(plist, "missing", missing);

		if(plist::get_key_path(plist, "nodes", entries))
			type = node_t::kNodeTypeDirectory;
		else if(missing)
			type = node_t::kNodeTypeMissing;
		else if(link != NULL_STR)
			type = node_t::kNodeTypeLink;

		nodes_ptr nodes;
		if(type == node_t::kNodeTypeDirectory)
		{
			nodes.reset(new std::vector<node_t>);
			iterate(entry, entries)
				nodes->push_back(from_plist(*entry));
		}
		return node_t(name, link, type, missing ? 0 : date.time_value(), nodes);
	}

} /* fs */
