#ifndef PATH_H_SS5GJIGI
#define PATH_H_SS5GJIGI

#include <oak/oak.h>
#include <oak/debug.h>

namespace path
{
	PUBLIC std::string normalize (std::string path);                // remove “./”, “../”, and “//” from path
	PUBLIC std::string resolve (std::string const& path);           // normalize and follow links/aliases
	PUBLIC std::string resolve_head (std::string const& path);      // normalize and ensure path is not a link/alias (but parent components can be)

	PUBLIC std::string name (std::string const& path);              // /Users/me/foo.html.erb → foo.html.erb
	PUBLIC std::string parent (std::string const& path);            // /Users/me/foo.html.erb → /Users/me
	PUBLIC std::string strip_extension (std::string const& path);   // /Users/me/foo.html.erb → /Users/me/foo.html
	PUBLIC std::string strip_extensions (std::string const& path);  // /Users/me/foo.html.erb → /Users/me/foo
	PUBLIC std::string extension (std::string const& path);         // /Users/me/foo.html.erb → .erb
	PUBLIC std::string extensions (std::string const& path);        // /Users/me/foo.html.erb → .html.erb

	PUBLIC size_t rank (std::string const& path, std::string const& ext); // returns a score for how well the “extension” covers the path (smaller values are better, except 0 means “no match”)

	PUBLIC std::string join (std::string const& base, std::string const& path); // this will normalize the (resulting) path

	PUBLIC std::string join (std::initializer_list<std::string> const& components);

	PUBLIC bool is_absolute (std::string const& path);
	PUBLIC bool is_child (std::string const& nonNormalizedChild, std::string const& nonNormalizedParent);
	PUBLIC std::string with_tilde (std::string const& path);        // /Users/me/foo.html.erb → ~/foo.html.erb
	PUBLIC std::string relative_to (std::string const& path, std::string const& base); // /Users/me/foo.html.erb (arg: ~/Desktop) → ../foo.html.erb

	PUBLIC std::string display_name (std::string const& path, size_t nunberOfParents = 0); // «whatever»
	PUBLIC std::vector<size_t> disambiguate (std::vector<std::string> const& paths);

	PUBLIC std::string unique (std::string const& requestedPath, std::string const& suffix = ""); // /foo/bar.txt → /foo/bar«suffix» 2.txt

	struct PUBLIC identifier_t
	{
		identifier_t (bool exists, dev_t device, ino_t inode, std::string const& path);
		identifier_t (std::string const& path = NULL_STR, bool resolve = false);
		bool operator< (identifier_t const& rhs) const;
		bool operator== (identifier_t const& rhs) const;
		bool operator!= (identifier_t const& rhs) const;
		explicit operator bool () const { return exists || path != NULL_STR; }
	private:
		bool can_trust_inode () const;
		bool exists;
		dev_t device;
		ino_t inode;
		std::string path; // TODO we should only store the path for non-existing files (preferably use a union — this to make the size of an ‘identifier’ small since it may be used in std::set when walking a directory hierarchy)
		friend std::string to_s (identifier_t const& identifier);
	};

	PUBLIC identifier_t identifier (std::string const& path);          // opaque type holding device + inode for resolved path (actual path for non-existing files), file_id_t supports operator< for ordering
	PUBLIC std::string to_s (identifier_t const& identifier);

	namespace flag
	{
		PUBLIC extern uint32_t
			meta,             /* for “.” and “..” entries */
			file,
			directory,
			symlink,
			dotfile,
			hidden,           /* file is hidden, does not include ‘dotfile’ */
			alias,
			package,
			application,
			stationery_pad,
			hidden_volume,
			hidden_extension;
	}

	PUBLIC dev_t device (std::string const& path);
	PUBLIC uint32_t info (std::string const& path, uint32_t mask = 0xFFFFFF); // the type of the file, mask given to limit work done, see possible values below

	PUBLIC size_t label_index (std::string const& path);
	PUBLIC bool set_label_index (std::string const& path, size_t labelIndex = 0);

	PUBLIC bool exists (std::string const& path);
	PUBLIC bool is_readable (std::string const& path);
	PUBLIC bool is_writable (std::string const& path);
	PUBLIC bool is_directory (std::string const& path);
	PUBLIC bool is_executable (std::string const& path);
	PUBLIC bool is_local (std::string const& path);
	PUBLIC bool is_trashed (std::string const& path);

	PUBLIC std::string for_fd (int fd);

	// ===========
	// = Actions =
	// ===========

	PUBLIC std::string content (std::string const& path);
	PUBLIC bool set_content (std::string const& path, char const* first, char const* last);
	inline bool set_content (std::string const& path, std::string const& content) { return set_content(path, content.data(), content.data() + content.size()); }

	PUBLIC std::string get_attr (std::string const& path, std::string const& attr);
	PUBLIC void set_attr (std::string const& path, std::string const& attr, std::string const& value);
	PUBLIC std::map<std::string, std::string> attributes (std::string const& path);
	PUBLIC bool set_attributes (std::string const& path, std::map<std::string, std::string> const& attributes);

	PUBLIC bool link (std::string const& from, std::string const& to);
	PUBLIC bool rename (std::string const& from, std::string const& to, bool overwrite = false);
	PUBLIC std::string move_to_trash (std::string const& path);
	PUBLIC std::string duplicate (std::string const& src, std::string dst = NULL_STR, bool overwrite = false);
	PUBLIC bool make_dir (std::string const& path);
	PUBLIC void touch_tree (std::string const& basePath);

	// ===============
	// = Global Info =
	// ===============

	PUBLIC passwd* passwd_entry (); // wrapper for getpwuid() that shows dialog incase of <rdar://10261043>

	PUBLIC std::vector<std::string> volumes ();
	PUBLIC std::string cwd ();
	PUBLIC std::string home ();
	PUBLIC std::string trash (std::string const& forPath);
	PUBLIC std::string temp (std::string const& file = NULL_STR);
	PUBLIC std::string cache (std::string const& file = NULL_STR);
	PUBLIC std::string desktop ();

} /* path */ 

#endif /* end of include guard: PATH_H_SS5GJIGI */
