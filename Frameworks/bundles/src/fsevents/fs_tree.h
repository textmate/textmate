#ifndef FS_TREE_H_NFRINZFU
#define FS_TREE_H_NFRINZFU

#include <oak/misc.h>
#include <io/path.h>
#include <regexp/glob.h>
#include <plist/plist.h>

namespace fs
{
	struct node_t;
	typedef std::shared_ptr< std::vector<node_t> > nodes_ptr;

	struct PUBLIC node_t
	{
		enum node_type_t { kNodeTypeDirectory, kNodeTypeLink, kNodeTypeFile, kNodeTypeMissing };

		explicit node_t (std::string const& path);

		std::string name () const                            { return _name; }
		std::string path (std::string const& cwd) const      { return path::join(cwd, _name); }
		std::string real_path (std::string const& cwd) const { return _resolved == NULL_STR ? path(cwd) : path::join(path::parent(path::join(cwd, _name)), _resolved); }
		node_type_t type () const                            { return _type; }
		time_t modified () const                             { return _modified; }
		nodes_ptr entries () const                           { return _entries; }
		bool operator== (node_t const& rhs) const            { return _name == rhs._name && _resolved == rhs._resolved && _type == rhs._type && _modified == rhs._modified && (!_entries && !rhs._entries || _entries && rhs._entries && *_entries == *rhs._entries); }
		bool operator!= (node_t const& rhs) const            { return !(*this == rhs); }

		node_t (std::string const& name, std::string const& resolved, node_type_t type, time_t modified = 0, nodes_ptr const& entries = nodes_ptr()) : _name(name), _resolved(resolved), _type(type), _modified(modified), _entries(entries) { }
		node_t& rescan (std::string const& cwd, path::glob_t const& dirGlob, path::glob_t const& fileGlob, std::map< std::string, std::vector<std::string> >* changes = NULL);

		std::string _name;
		std::string _resolved;
		node_type_t _type;
		time_t _modified;
		nodes_ptr _entries; // when _type == kNodeTypeDirectory
	};

	PUBLIC plist::dictionary_t to_plist (node_t const& node);
	PUBLIC node_t from_plist (plist::any_t const& plist);

} /* fs */

#endif /* end of include guard: FS_TREE_H_NFRINZFU */
