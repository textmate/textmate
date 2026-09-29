#ifndef FS_CACHE_H_3ILT90EK
#define FS_CACHE_H_3ILT90EK

#include <io/src/path.h>
#include "plist.h"

namespace plist
{
	struct cache_t
	{
		void load (std::string const& path);
		void load_capnp (std::string const& path);
		void save (std::string const& path) const;
		void save_capnp (std::string const& path) const;

		bool dirty () const        { return _dirty; }
		void set_dirty (bool flag) { _dirty = flag; }

		uint64_t event_id_for_path (std::string const& path) const;
		void set_event_id_for_path (uint64_t eventId, std::string const& path);

		plist::dictionary_t content (std::string const& path);
		std::vector<std::string> entries (std::string const& path, std::string const& globString = NULL_STR);

		bool erase (std::string const& path);
		bool reload (std::string const& path, bool recursive = false);
		bool cleanup (std::vector<std::string> const& rootPaths);

		template <typename _OutputIter>
		_OutputIter copy_heads_for_path (std::string const& path, _OutputIter out)
		{
			*out++ = path;
			return copy_links(_cache.find(path), out);
		}

		void set_content_filter (plist::dictionary_t (*f)(plist::dictionary_t const&)) { _prune_dictionary = f; }
		plist::dictionary_t (*content_filter () const)(plist::dictionary_t const&)     { return _prune_dictionary; }

	private:
		void real_load (std::string const& path);

		static int32_t const kPropertyCacheFormatVersion;
		enum class entry_type_t { file, directory, link, missing, unknown };

		struct entry_t
		{
			entry_t (std::string const& path) : _path(path) { }

			bool is_link () const                                    { return _type == entry_type_t::link; }
			bool is_file () const                                    { return _type == entry_type_t::file; }
			bool is_directory () const                               { return _type == entry_type_t::directory; }
			bool is_missing () const                                 { return _type == entry_type_t::missing; }

			entry_type_t type () const                               { return _type; }
			std::string const& path () const                         { return _path; }
			std::string const& link () const                         { return _link; }
			std::string resolved () const                            { return path::join(path::parent(_path), _link); }
			time_t modified () const                                 { return _modified; }
			uint64_t event_id () const                               { return _event_id; }
			plist::dictionary_t const& content () const              { return _content; }
			std::vector<std::string> const& entries () const         { return _entries; }
			std::string glob_string () const                         { return _glob_string; }

			void set_type (entry_type_t type)                        { _type = type; }
			void set_link (std::string const& link)                  { _link = link; }
			void set_modified (time_t modified)                      { _modified = modified; }
			void set_event_id (uint64_t eventId)                     { _event_id = eventId; }
			void set_content (plist::dictionary_t const& plist)      { _content = plist; }
			void set_entries (std::vector<std::string> const& array) { _entries = array; }
			void set_glob_string (std::string const& globString)     { _glob_string = globString; }

			void set_entries (std::vector<std::string> const& array, std::string const& globString) { _entries = array; _glob_string = globString; }

		private:
			std::string _path;
			entry_type_t _type = entry_type_t::unknown;
			std::string _link;
			std::string _glob_string;
			time_t _modified;
			uint64_t _event_id = 0;
			plist::dictionary_t _content;
			std::vector<std::string> _entries;
		};

		plist::dictionary_t (*_prune_dictionary)(plist::dictionary_t const&) = nullptr;
		std::map<std::string, entry_t> _cache;
		bool _dirty = false;

		entry_t& resolved (std::string const& path, std::string const& globString = NULL_STR);
		static void update_entries (entry_t& entry, std::string const& globString);

		template <typename _InputIter, typename _OutputIter>
		_OutputIter copy_links (_InputIter entryIter, _OutputIter out)
		{
			if(entryIter == _cache.end())
				return out;

			entry_t const& entry = entryIter->second;
			if(entry.is_link())
			{
				*out++ = entry.resolved();
				out = copy_links(_cache.find(entry.resolved()), out);
			}
			else if(entry.is_directory())
			{
				for(auto path : entries(entry.path(), entry.glob_string()))
					out = copy_links(_cache.find(path), out);
			}
			return out;
		}

		template <typename _OutputIter>
		_OutputIter copy_all (std::string const& path, _OutputIter out)
		{
			auto it = _cache.find(path);
			if(it != _cache.end())
			{
				*out++ = path;
				if(it->second.is_directory())
				{
					for(auto child : entries(it->second.path(), it->second.glob_string()))
						out = copy_all(child, out);
				}
				else if(it->second.is_link())
				{
					out = copy_all(it->second.resolved(), out);
				}
			}
			return out;
		}
	};

} /* plist */

#endif /* end of include guard: FS_CACHE_H_3ILT90EK */
