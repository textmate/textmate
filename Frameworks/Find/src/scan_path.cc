#include "scan_path.h"
#include <text/utf8.h>
#include <io/entries.h>
#include <oak/compat.h>

OAK_DEBUG_VAR(Find_Scan_Path);

namespace find
{
	std::string folder_scan_settings_t::open_files = "open_files";
} /* find */

namespace scan
{
	struct newlines_t
	{
		WATCH_LEAKS(scan::newlines_t);

		newlines_t () : bol_offsets(1, 0), offset(0), last_character('\0') { }

		void scan (char const* first, ssize_t len)
		{
			for(ssize_t i = 0; i < len; i++)
			{
				if(last_character == '\r' && first[i] == '\n')
				{
					bol_offsets.push_back(offset + i+1);
					last_character = '\0';
				}
				else
				{
					if(last_character == '\n' || last_character == '\r')
						bol_offsets.push_back(offset + i);
					last_character = first[i];
				}
			}
			offset += len;
		}

		void flush ()
		{
			if(last_character == '\n' || last_character == '\r')
				bol_offsets.push_back(offset);
			last_character = '\0';
		}

		size_t line_for_offset (ssize_t anOffset)
		{
			flush();
			std::vector<ssize_t>::const_iterator it = lower_bound(bol_offsets.begin(), bol_offsets.end(), anOffset);
			if(it != bol_offsets.begin() && (it == bol_offsets.end() || *it != anOffset))
				--it;
			return it - bol_offsets.begin();
		}

		size_t col_for_offset (ssize_t anOffset)
		{
			flush();
			std::vector<ssize_t>::const_iterator iter = bol_offsets.end();
			do
			{
				--iter;
			} while(*iter > anOffset);
			return anOffset - *iter;
		}

		size_t bol_for_offset (ssize_t anOffset)
		{
			flush();
			std::vector<ssize_t>::const_iterator it = lower_bound(bol_offsets.begin(), bol_offsets.end(), anOffset);
			if(it != bol_offsets.begin() && (it == bol_offsets.end() || *it != anOffset))
				--it;
			ASSERT(it != bol_offsets.end());
			return *it;
		}

		size_t eol_for_offset (ssize_t anOffset)
		{
			flush();
			std::vector<ssize_t>::const_iterator it = upper_bound(bol_offsets.begin(), bol_offsets.end(), anOffset);
			if(it == bol_offsets.end())
				return offset;
			return *it;
		}

	private:
		std::vector<ssize_t> bol_offsets;
		ssize_t offset;
		char last_character;
	};
	
} /* scan */

namespace find
{
	scan_path_t::scan_path_t () : options(find::none), is_running_flag(false), should_stop_flag(false)
	{
		pthread_mutex_init(&mutex, NULL);
	}

	scan_path_t::~scan_path_t ()
	{
		stop();
		D(DBF_Find_Scan_Path, bug("wait for thread\n"););
		pthread_join(thread, NULL);
		pthread_mutex_destroy(&mutex);
		D(DBF_Find_Scan_Path, bug("thread has terminated\n"););
	}

	void scan_path_t::start ()
	{
		struct runner_t {
			static void* server (void* arg)  { ((scan_path_t*)arg)->server_run(); return NULL; }
		};
		is_running_flag = true;
		scanned_file_count = 0;
		pthread_create(&thread, NULL, &runner_t::server, this);
	}

	void scan_path_t::stop ()
	{
		D(DBF_Find_Scan_Path, bug("%s → YES\n", BSTR(should_stop_flag)););
		should_stop_flag = true;
	}

	bool scan_path_t::is_running () const
	{
		return is_running_flag;
	}

	scan_path_matches_t scan_path_t::accept_matches ()
	{
		pthread_mutex_lock(&mutex);
		scan_path_matches_t res;
		res.swap(matches);
		pthread_mutex_unlock(&mutex);
		return res;
	}

	std::string scan_path_t::get_current_path () const
	{
		pthread_mutex_lock(&mutex);
		std::string res = current_path;
		pthread_mutex_unlock(&mutex);
		return res;
	}

	size_t scan_path_t::get_scanned_file_count () const
	{
		return scanned_file_count;
	}

	// =========================
	// = The meat of the thing =
	// =========================

	void scan_path_t::server_run ()
	{
		oak::set_thread_name("find::scan_path_t");

		if(search.path == find::folder_scan_settings_t::open_files)
		{
			for(auto const& doc : document::scanner_t::open_documents())
				scan_document(doc);
			is_running_flag = false;
			return;
		}

		document::scanner_t scanner(search.path, search.globs, search.follow_links, true /* depth first */);

		bool isRunning = true;
		while(isRunning && !should_stop_flag)
		{
			isRunning = scanner.is_running();

			std::vector<document::document_ptr> const& documents = scanner.accept_documents();
			for(auto const& doc : documents)
			{
				if(!should_stop_flag && doc->path() != NULL_STR)
					scan_document(doc);
			}

			if(should_stop_flag || !isRunning)
				break;

			std::string currentDir = scanner.get_current_path();
			update_current_path(currentDir);
			usleep(40000);
		}
		is_running_flag = false;

		D(DBF_Find_Scan_Path, bug("leave\n"););
	}

	void scan_path_t::update_current_path (std::string const& path)
	{
		pthread_mutex_lock(&mutex);
		current_path = path;
		pthread_mutex_unlock(&mutex);
	}

	namespace
	{
		struct range_match_t
		{
			range_match_t (ssize_t from, ssize_t to, std::map<std::string, std::string> const& captures) : from(from), to(to), captures(captures) { }

			ssize_t from, to;
			std::map<std::string, std::string> captures;
		};
	}

	void scan_path_t::scan_document (document::document_ptr const& document)
	{
		D(DBF_Find_Scan_Path, bug("%s (%s)\n", document->path().c_str(), document->display_name().c_str()););
		update_current_path(document->path());

		if(document::document_t::reader_ptr reader = document->create_reader())
		{
			scan::newlines_t nl;
			utf8::validate_t utf8;
			find::find_t f(string, options);
			std::vector<range_match_t> ranges;

			ssize_t total = 0;
			while(io::bytes_ptr const& data = reader->next())
			{
				if(should_stop_flag)
					break;

				char const* buf = data->get();
				size_t len      = data->size();

				nl.scan(buf, len);
				utf8.scan(buf, buf + len);
				for(ssize_t offset = 0; offset < len; )
				{
					std::map<std::string, std::string> captures;
					std::pair<ssize_t, ssize_t> const& m = f.match(buf + offset, len - offset, &captures);
					if(m.first <= m.second)
						ranges.push_back(range_match_t(total + offset + m.first, total + offset + m.second, captures));
					ASSERT_NE(m.second, 0); ASSERT_LE(m.second, len - offset);
					offset += m.second;
				}
				total += len;
			}

			std::map<std::string, std::string> captures;
			std::pair<ssize_t, ssize_t> m = f.match(NULL, 0, &captures);
			while(m.first <= m.second)
			{
				ranges.push_back(range_match_t(total + m.first, total + m.second, captures));
				captures.clear();
				m = f.match(NULL, 0, &captures);
			}

			++scanned_file_count;

			if(!ranges.empty())
			{
				bool binary = !utf8.is_valid();
				pthread_mutex_lock(&mutex);
				for(auto const& it : ranges)
				{
					text::pos_t from(nl.line_for_offset(it.from), nl.col_for_offset(it.from));
					text::pos_t to(nl.line_for_offset(it.to), nl.col_for_offset(it.to));
					matches.push_back(std::make_pair(document, match_t(document, it.from, it.to, text::range_t(from, to), it.captures, nl.bol_for_offset(it.from), nl.eol_for_offset(it.to), binary)));
				}
				pthread_mutex_unlock(&mutex);
			}
		}
	}
	
} /* find */ 
