#ifndef SNAPSHOT_H_UOFPK7X7
#define SNAPSHOT_H_UOFPK7X7

namespace fs
{
	struct snapshot_t
	{
		snapshot_t ();
		snapshot_t (std::string const& path);

		bool operator== (snapshot_t const& rhs) const { return _entries && rhs._entries ? *_entries == *rhs._entries : _entries == rhs._entries; }
		bool operator!= (snapshot_t const& rhs) const { return _entries && rhs._entries ? *_entries != *rhs._entries : _entries != rhs._entries; }

	private:
		friend std::string to_s (fs::snapshot_t const& snapshot);

		struct node_t;
		typedef std::shared_ptr< std::map<ino_t, node_t> > nodes_ptr;

		struct node_t
		{
			enum node_type_t { kNodeTypeDirectory, kNodeTypeLink, kNodeTypeFile };

			node_t (std::string const& name, node_type_t type, time_t modified = 0, nodes_ptr const& entries = nodes_ptr()) : _name(name), _type(type), _modified(modified), _entries(entries) { }

			std::string name () const                            { return _name; }
			node_type_t type () const                            { return _type; }
			time_t modified () const                             { return _modified; }
			nodes_ptr entries () const                           { return _entries; }
			bool operator== (node_t const& rhs) const            { return _name == rhs._name && _type == rhs._type && _modified == rhs._modified && (!_entries && !rhs._entries || _entries && rhs._entries && *_entries == *rhs._entries); }
			bool operator!= (node_t const& rhs) const            { return !(*this == rhs); }

			std::string to_s (size_t indent = 0) const;

		private:
			std::string _name;
			node_type_t _type;
			time_t _modified;
			nodes_ptr _entries; // when _type == kNodeTypeDirectory
		};

		static time_t modified (std::string const& path);
		static nodes_ptr collect (std::string const& dir);

		nodes_ptr _entries;
	};

	std::string to_s (snapshot_t const& snapshot);

} /* fs */

#endif /* end of include guard: SNAPSHOT_H_UOFPK7X7 */
