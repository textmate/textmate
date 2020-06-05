#ifndef PATH_H_SS5GJIGI
#define PATH_H_SS5GJIGI

#include <oak/oak.h>
#include <oak/debug.h>

namespace path
{
	std::string normalize (std::string path);                // remove “./”, “../”, and “//” from path
	std::string resolve (std::string const& path);           // normalize and follow links/aliases
	std::string resolve_head (std::string const& path);      // normalize and ensure path is not a link/alias (but parent components can be)

	std::string name (std::string const& path);              // /Users/me/foo.html.erb → foo.html.erb
	std::string parent (std::string const& path);            // /Users/me/foo.html.erb → /Users/me
	std::string strip_extension (std::string const& path);   // /Users/me/foo.html.erb → /Users/me/foo.html
	std::string strip_extensions (std::string const& path);  // /Users/me/foo.html.erb → /Users/me/foo
	std::string extension (std::string const& path);         // /Users/me/foo.html.erb → .erb
	std::string extensions (std::string const& path);        // /Users/me/foo.html.erb → .html.erb

	std::string escape (std::string const& src);                // Shell escape path
	std::vector<std::string> unescape (std::string const& src); // Split into “shell words”

	size_t rank (std::string const& path, std::string const& ext); // returns a score for how well the “extension” covers the path (smaller values are better, except 0 means “no match”)

	std::string join (std::string const& base, std::string const& path); // this will normalize the (resulting) path

	std::string join (std::initializer_list<std::string> const& components);

	bool is_absolute (std::string const& path);
	bool is_child (std::string const& nonNormalizedChild, std::string const& nonNormalizedParent);
	std::string with_tilde (std::string const& path);        // /Users/me/foo.html.erb → ~/foo.html.erb
	std::string relative_to (std::string const& path, std::string const& base); // /Users/me/foo.html.erb (arg: ~/Desktop) → ../foo.html.erb

	std::string display_name (std::string const& path, size_t nunberOfParents = 0); // «whatever»
	std::vector<size_t> disambiguate (std::vector<std::string> const& paths);

	std::string unique (std::string const& requestedPath, std::string const& suffix = ""); // /foo/bar.txt → /foo/bar«suffix» 2.txt

	dev_t device (std::string const& path);

	bool exists (std::string const& path);
	bool is_readable (std::string const& path);
	bool is_writable (std::string const& path);
	bool is_directory (std::string const& path);
	bool is_executable (std::string const& path);
	bool is_local (std::string const& path);

	std::string for_fd (int fd);

	// ===========
	// = Actions =
	// ===========

	std::string content (std::string const& path);
	bool set_content (std::string const& path, char const* first, char const* last);
	inline bool set_content (std::string const& path, std::string const& content) { return set_content(path, content.data(), content.data() + content.size()); }

	std::string get_attr (std::string const& path, std::string const& attr);
	void set_attr (std::string const& path, std::string const& attr, std::string const& value);
	std::map<std::string, std::string> attributes (std::string const& path);
	bool set_attributes (std::string const& path, std::map<std::string, std::string> const& attributes);

	bool link (std::string const& from, std::string const& to);
	bool rename (std::string const& from, std::string const& to, bool overwrite = false);
	std::string move_to_trash (std::string const& path);
	std::string duplicate (std::string const& src, std::string dst = NULL_STR, bool overwrite = false);
	bool make_dir (std::string const& path);
	bool rename_or_copy (std::string const& src, std::string const& dst, bool createParent = true);

	// ===============
	// = Global Info =
	// ===============

	passwd* passwd_entry (); // wrapper for getpwuid() that shows dialog incase of <rdar://10261043>

	std::vector<std::string> volumes ();
	std::string cwd ();
	std::string home ();
	std::string temp (std::string const& file = NULL_STR, std::string const& content = NULL_STR);
	std::string cache (std::string const& file = NULL_STR);
	std::string desktop ();

} /* path */

#endif /* end of include guard: PATH_H_SS5GJIGI */
