#ifndef SCAN_PATH_H_RRYJZFKK
#define SCAN_PATH_H_RRYJZFKK

#include <regexp/find.h>
#include <oak/debug.h>
#include <text/types.h>
#include <document/document.h>

namespace find
{
	struct folder_scan_settings_t
	{
		static std::string open_files;

		folder_scan_settings_t (std::string const& path = "/", path::glob_list_t const& globs = path::glob_list_t("*"), bool follow_links = false) : path(path), globs(globs), follow_links(follow_links) { }

		std::string path;
		path::glob_list_t globs;
		bool follow_links;
	};

	struct match_t
	{
		WATCH_LEAKS(find::match_t);

		match_t () { }
		match_t (document::document_ptr document) : document(document) { }
		match_t (document::document_ptr document, size_t first, size_t last, text::range_t const& range, std::map<std::string, std::string> const& captures) : document(document), first(first), last(last), range(range), captures(captures) { }

		size_t line_span () const;

		document::document_ptr document;
		size_t first = 0, last = 0;
		text::range_t range = text::range_t::undefined;
		std::map<std::string, std::string> captures;
		std::string excerpt;
		size_t excerpt_offset = 0;
		size_t line_number = 0;
		bool binary = false;
	};

	struct scan_path_t
	{
		WATCH_LEAKS(find::scan_path_t);

		// you can just delete this object and forget about it, it will gracefully terminate thread etc.
		scan_path_t ();
		~scan_path_t ();

		// before doing a search, setup at least a search string and folder
		void set_search_string (std::string const& search_string)
		{
			ASSERT(!is_running());
			_search_string = search_string;
		}

		void set_folder_options (folder_scan_settings_t const& search)
		{
			ASSERT(!is_running());
			_search = search;
			_current_path = search.path;
		}

		// optinally set some options or a file glob
		void set_file_options (find::options_t options)
		{
			ASSERT(!is_running());
			_options = options;
		}

		// then start the search, and potentially prematurely stop it
		void start ();
		void stop ();

		// while running, probe it to see if it is still running, periodically accept results, and update the folder it shows as being scanned
		bool is_running () const;

		std::vector<match_t> accept_matches ();
		std::string current_path () const;

		std::string const& search_string () const             { return _search_string; };
		folder_scan_settings_t const& folder_options () const { return _search; };
		size_t scanned_file_count () const                    { return _scanned_file_count; }

		void scan_document (document::document_ptr const& document);

	private:
		void server_run ();
		void update_current_path (std::string const& path);

		std::string _search_string;
		find::options_t _options = find::none;

		folder_scan_settings_t _search;
		std::string _current_path;

		std::vector<match_t> _matches;

		volatile bool _is_running = false, _should_stop = false;
		size_t _scanned_file_count = 0;

		pthread_t _thread;
		mutable pthread_mutex_t _mutex;
	};

} /* find */

typedef std::shared_ptr<find::scan_path_t> scan_path_ptr;

#endif /* end of include guard: SCAN_PATH_H_RRYJZFKK */
