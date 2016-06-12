#include "scan_path.h"
#include <text/utf8.h>
#include <text/newlines.h>
#include <io/entries.h>

OAK_DEBUG_VAR(Find_Scan_Path);

namespace find
{
	std::string const kSearchOpenFiles = "internal://open-files";

	scan_path_t::scan_path_t ()
	{
		pthread_mutex_init(&_mutex, NULL);
	}

	scan_path_t::~scan_path_t ()
	{
		stop();
		D(DBF_Find_Scan_Path, bug("wait for thread\n"););
		pthread_join(_thread, NULL);
		pthread_mutex_destroy(&_mutex);
		D(DBF_Find_Scan_Path, bug("thread has terminated\n"););
	}

	void scan_path_t::start ()
	{
		struct runner_t {
			static void* server (void* arg)  { ((scan_path_t*)arg)->server_run(); return NULL; }
		};
		_is_running = true;
		_scanned_file_count = 0;
		_scanned_byte_count = 0;
		pthread_create(&_thread, NULL, &runner_t::server, this);
	}

	void scan_path_t::stop ()
	{
		D(DBF_Find_Scan_Path, bug("%s → YES\n", BSTR(_should_stop)););
		_should_stop = true;
	}

	bool scan_path_t::is_running () const
	{
		return _is_running;
	}

	std::vector<match_t> scan_path_t::accept_matches ()
	{
		pthread_mutex_lock(&_mutex);
		std::vector<match_t> res;
		res.swap(_matches);
		pthread_mutex_unlock(&_mutex);
		return res;
	}

	std::string scan_path_t::current_path () const
	{
		pthread_mutex_lock(&_mutex);
		std::string res = _current_path;
		pthread_mutex_unlock(&_mutex);
		return res;
	}

	// =========================
	// = The meat of the thing =
	// =========================

	void scan_path_t::server_run ()
	{
		pthread_setname_np("find::scan_path_t");

		if(_path == kSearchOpenFiles)
		{
			for(auto const& doc : document::scanner_t::open_documents())
				scan_document(doc);
			_is_running = false;
			return;
		}

		document::scanner_t scanner(_path, _glob_list);
		scanner.set_follow_directory_links(_follow_links);
		scanner.set_follow_file_links(_search_links);
		scanner.set_include_untitled(true);
		scanner.set_depth_first(true);
		scanner.start();

		bool isRunning = true;
		while(isRunning && !_should_stop)
		{
			isRunning = scanner.is_running();

			std::vector<document::document_ptr> const& documents = scanner.accept_documents();
			for(auto const& doc : documents)
			{
				if(!_should_stop && doc->path() != NULL_STR)
					scan_document(doc);
			}

			if(_should_stop || !isRunning)
				break;

			std::string currentDir = scanner.get_current_path();
			update_current_path(currentDir);
			usleep(40000);
		}
		_is_running = false;

		D(DBF_Find_Scan_Path, bug("leave\n"););
	}

	void scan_path_t::update_current_path (std::string const& path)
	{
		pthread_mutex_lock(&_mutex);
		_current_path = path;
		pthread_mutex_unlock(&_mutex);
	}

	void scan_path_t::scan_document (document::document_ptr const& document)
	{
		struct range_match_t
		{
			range_match_t (ssize_t from, ssize_t to, std::map<std::string, std::string> const& captures) : from(from), to(to), captures(captures) { }

			ssize_t from, to;
			std::map<std::string, std::string> captures;
		};

		D(DBF_Find_Scan_Path, bug("%s (%s)\n", document->path().c_str(), document->display_name().c_str()););
		update_current_path(document->path());

		__block find::find_t f(_search_string, _options | (document->is_open() ? find::none : find::filesize_limit));
		__block std::vector<range_match_t> ranges;
		__block boost::crc_32_type crc32;
		__block ssize_t total = 0;
		document->enumerate_bytes_using_block(^(char const* buf, size_t len, bool* stop){
			if(_should_stop || !_search_binaries && memchr(buf, '\0', len))
			{
				*stop = true;
				return;
			}

			for(ssize_t offset = 0; offset < len; )
			{
				std::map<std::string, std::string> captures;
				std::pair<ssize_t, ssize_t> const& m = f.match(buf + offset, len - offset, &captures);
				if(m.first <= m.second)
					ranges.push_back(range_match_t(total + offset + m.first, total + offset + m.second, captures));
				ASSERT_NE(m.second, 0); ASSERT_LE(m.second, len - offset);
				offset += m.second;
			}

			crc32.process_bytes(buf, len);
			total += len;
		});

		std::map<std::string, std::string> captures;
		std::pair<ssize_t, ssize_t> m = f.match(NULL, 0, &captures);
		while(m.first <= m.second)
		{
			ranges.push_back(range_match_t(total + m.first, total + m.second, captures));
			captures.clear();
			m = f.match(NULL, 0, &captures);
		}

		_scanned_byte_count += total;
		++_scanned_file_count;
		if(ranges.empty())
			return;

		__block std::string text;
		document->enumerate_bytes_using_block(^(char const* buf, size_t len, bool* stop){
			if(*stop = _should_stop)
				return;
			text.insert(text.end(), buf, buf + len);
		});

		// Document has changed, should probably re-scan
		boost::crc_32_type doubleCheck;
		doubleCheck.process_bytes(text.data(), text.size());
		if(crc32.checksum() != doubleCheck.checksum())
			return;

		std::vector<match_t> results;

		std::string newlines = text::estimate_line_endings(std::begin(text), std::end(text));
		newlines = newlines == kMIX ? kLF : newlines;

		size_t bol = 0, nextLine = bol, lfCount = 0;
		for(auto const& it : ranges)
		{
			while(true)
			{
				nextLine = text.find(newlines, bol);
				if(nextLine == std::string::npos || it.from < nextLine + newlines.size())
					break;
				bol = nextLine + newlines.size();
				++lfCount;
			}

			text::pos_t from(lfCount, it.from - bol);
			size_t fromLine = bol;

			while(true)
			{
				nextLine = text.find(newlines, bol);
				if(nextLine == std::string::npos || it.to < nextLine + newlines.size())
					break;
				bol = nextLine + newlines.size();
				++lfCount;
			}

			text::pos_t to(lfCount, it.to - bol);

			size_t eol = bol == it.to ? bol : text.find(newlines, bol);
			eol = eol != std::string::npos ? eol : text.size();

			match_t res(document, crc32.checksum(), it.from, it.to, text::range_t(from, to), it.captures);
			res.excerpt        = text.substr(fromLine, eol - fromLine);
			res.excerpt_offset = fromLine;
			res.line_number    = from.line;
			res.newlines       = newlines;
			results.push_back(res);
		}

		pthread_mutex_lock(&_mutex);
		_matches.insert(_matches.end(), results.begin(), results.end());
		pthread_mutex_unlock(&_mutex);
	}

} /* find */
