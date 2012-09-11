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

		match_t (document::document_ptr document, size_t first, size_t last, text::range_t const& range, std::map<std::string, std::string> const& captures, off_t bol_offset, off_t eol_offset, bool binary) : document(document), first(first), last(last), range(range), captures(captures), bol_offset(bol_offset), eol_offset(eol_offset), binary(binary) { }
		match_t () { }
		match_t (document::document_ptr document) : document(document), first(0), last(0), range(text::range_t::undefined) { }

		document::document_ptr document;
		size_t first, last;
		text::range_t range;
		std::map<std::string, std::string> captures;
		off_t bol_offset, eol_offset;
		bool binary;
	};

	typedef std::vector< std::pair<document::document_ptr, match_t> > scan_path_matches_t;

	// NOTE: This class is public _only_ for use in testing
	struct PUBLIC scan_path_t
	{
		WATCH_LEAKS(find::scan_path_t);

		// you can just delete this object and forget about it, it will gracefully terminate thread etc.
		scan_path_t ();
		~scan_path_t ();

		// before doing a search, setup at least a search string and folder
		void set_string (std::string const& aString)   { ASSERT(!is_running()); string = aString; }
		void set_folder_options (folder_scan_settings_t const& aSearch)
		{
			ASSERT(!is_running());
			search = aSearch;
			current_path = aSearch.path;
		}

		// optinally set some options or a file glob
		void set_file_options (find::options_t someOptions) { ASSERT(!is_running()); options = someOptions; }

		// then start the search, and potentially prematurely stop it
		void start ();
		void stop ();

		// while running, probe it to see if it is still running, periodically accept results, and update the folder it shows as being scanned
		bool is_running () const;

		scan_path_matches_t accept_matches ();
		std::string get_current_path () const;
		std::string const& get_string () const { return string; };
		folder_scan_settings_t const& folder_options () const { return search; };
		size_t get_scanned_file_count () const;

		void scan_document (document::document_ptr const& document);

	private:
		void server_run ();
		void update_current_path (std::string const& path);

		std::string string;
		find::options_t options;

		folder_scan_settings_t search;
		std::string current_path;

		scan_path_matches_t matches;

		volatile bool is_running_flag, should_stop_flag;
		size_t scanned_file_count;

		pthread_t thread;
		mutable pthread_mutex_t mutex;
	};

} /* find */ 

typedef std::shared_ptr<find::scan_path_t> scan_path_ptr;

#endif /* end of include guard: SCAN_PATH_H_RRYJZFKK */
