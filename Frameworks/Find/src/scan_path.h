#ifndef SCAN_PATH_H_RRYJZFKK
#define SCAN_PATH_H_RRYJZFKK

#include <regexp/find.h>
#include <oak/debug.h>
#include <text/types.h>
#include <document/document.h>

namespace find
{
	extern std::string const kSearchOpenFiles;

	struct match_t
	{
		WATCH_LEAKS(find::match_t);

		match_t () { }
		match_t (document::document_ptr document, size_t first, size_t last, text::range_t const& range, std::map<std::string, std::string> const& captures) : document(document), first(first), last(last), range(range), captures(captures) { }

		document::document_ptr document;
		size_t first, last;
		text::range_t range;
		std::map<std::string, std::string> captures;
		std::string excerpt;
		size_t excerpt_offset = 0;
		size_t line_number = 0;
	};

	struct scan_path_t
	{
		WATCH_LEAKS(find::scan_path_t);

		scan_path_t ();
		~scan_path_t ();

		void set_search_string (std::string const& str)     { ASSERT(!is_running()); _search_string = str; }
		void set_options (find::options_t searchOptions)    { ASSERT(!is_running()); _options = searchOptions; }
		void set_path (std::string const& path)             { ASSERT(!is_running()); _path = _current_path = path; }
		void set_glob_list (path::glob_list_t const& globs) { ASSERT(!is_running()); _glob_list = globs; }
		void set_follow_links (bool followLinks)            { ASSERT(!is_running()); _follow_links = followLinks; }
		void set_search_links (bool searchLinks)            { ASSERT(!is_running()); _search_links = searchLinks; }
		void set_search_binaries (bool searchBinaries)      { ASSERT(!is_running()); _search_binaries = searchBinaries; }

		void start ();
		void stop ();
		bool is_running () const;

		std::vector<match_t> accept_matches ();
		std::string current_path () const;

		size_t scanned_file_count () const  { return _scanned_file_count; }
		size_t scanned_byte_count () const  { return _scanned_byte_count; }

		void scan_document (document::document_ptr const& document);

	private:
		void server_run ();
		void update_current_path (std::string const& path);

		std::string _search_string;
		find::options_t _options = find::none;

		std::string _path;
		path::glob_list_t _glob_list = path::glob_list_t("*");
		bool _follow_links = false;
		bool _search_links = true;
		bool _search_binaries = false;

		std::string _current_path;
		std::vector<match_t> _matches;
		size_t _scanned_file_count = 0;
		size_t _scanned_byte_count = 0;

		pthread_t _thread;
		mutable pthread_mutex_t _mutex;

		volatile bool _is_running  = false;
		volatile bool _should_stop = false;
	};

} /* find */

typedef std::shared_ptr<find::scan_path_t> scan_path_ptr;

#endif /* end of include guard: SCAN_PATH_H_RRYJZFKK */
