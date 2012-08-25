#include "document.h"
#include "watch.h"
#include "merge.h"
#include "reader.h"
#include "collection.h"
#include <io/io.h>
#include <regexp/glob.h>
#include <bundles/bundles.h>
#include <command/parser.h>
#include <cf/cf.h>
#include <cf/timer.h>
#include <cf/run_loop.h>
#include <file/type.h>
#include <file/path_info.h>
#include <plist/ascii.h>
#include <selection/selection.h>
#include <OakSystem/application.h>
#include <oak/debug.h>
#include <text/utf8.h>
#include <text/ctype.h>
#include <text/hexdump.h>
#include <text/tokenize.h>
#include <oak/duration.h>
#include <oak/server.h>
#include <oak/compat.h>
#include <io/entries.h>
#include <io/resource.h>
#include <scm/scm.h>
#include <settings/volume.h>

OAK_DEBUG_VAR(Document_Scanner);
OAK_DEBUG_VAR(Document_LRU);
OAK_DEBUG_VAR(Document_WatchFS);
OAK_DEBUG_VAR(Document_Replace);
OAK_DEBUG_VAR(Document_Tracker);
OAK_DEBUG_VAR(Document_Backup);
OAK_DEBUG_VAR(Document_Binary);
OAK_DEBUG_VAR(Document);

static std::string session_dir ()
{
	std::string const& res = oak::application_t::support("Session");
	return path::make_dir(res) ? res : "/tmp";
}

// ==========
// = Backup =
// ==========

static std::string backup_path (std::string displayName)
{
	std::replace(displayName.begin(), displayName.end(), '/', ':');
	return path::unique(path::join(session_dir(), displayName));
}

namespace
{
	struct backup_record_t
	{
		backup_record_t () : backup_at(DBL_MAX), upper_limit(CFAbsoluteTimeGetCurrent() + 10) { }

		cf::timer_ptr timer;
		CFAbsoluteTime backup_at;
		CFAbsoluteTime upper_limit;
	};
}

static std::map<oak::uuid_t, backup_record_t> records;

static void cancel_backup (oak::uuid_t const& docId)
{
	D(DBF_Document_Backup, bug("%s\n", to_s(docId).c_str()););
	records.erase(docId);
}

static void perform_backup (oak::uuid_t const& docId)
{
	D(DBF_Document_Backup, bug("%s\n", to_s(docId).c_str()););
	if(document::document_ptr document = document::find(docId, false))
		document->backup();
	records.erase(docId);
}

static void schedule_backup (oak::uuid_t const& docId)
{
	D(DBF_Document_Backup, bug("%s\n", to_s(docId).c_str()););

	backup_record_t& record = records[docId];
	CFAbsoluteTime backupAt = std::min(CFAbsoluteTimeGetCurrent() + 2, record.upper_limit);
	if(!record.timer || record.backup_at < backupAt)
	{
		record.timer = cf::setup_timer(backupAt - CFAbsoluteTimeGetCurrent(), cf::create_callback(&perform_backup, docId));
		record.backup_at = backupAt;
	}
}

// ==========

static std::multimap<text::range_t, document::document_t::mark_t> parse_marks (std::string const& str)
{
	std::multimap<text::range_t, document::document_t::mark_t> marks;
	if(str != NULL_STR)
	{
		plist::any_t const& plist = plist::parse(str);
		if(plist::array_t const* array = boost::get<plist::array_t>(&plist))
		{
			iterate(bm, *array)
			{
				if(std::string const* str = boost::get<std::string>(&*bm))
					marks.insert(std::make_pair(*str, "bookmark"));
			}
		}
	}
	return marks;
}

namespace document
{
	// ================
	// = File Watcher =
	// ================

	struct watch_t : watch_base_t
	{
		WATCH_LEAKS(document::watch_t);

		watch_t (std::string const& path, document_weak_ptr document) : watch_base_t(path), document(document) { }

		void callback (int flags, std::string const& newPath)
		{
			if(document_ptr doc = document.lock())
				doc->watch_callback(flags, newPath);
		}

	private:
		document_weak_ptr document;
	};

	// ====================
	// = Document Tracker =
	// ====================

	static OSSpinLock spinlock = 0;
	static pthread_t MainThread = pthread_self();

	static struct document_tracker_t
	{
		ssize_t lock_count;
		document_tracker_t () : lock_count(0) { }

		struct lock_t
		{
			lock_t (document_tracker_t* tracker) : tracker(tracker), locked(false) { retain(); }
			~lock_t ()                                                             { release(); }

			void retain ()  { OSSpinLockLock(&spinlock); DB(++tracker->lock_count); locked = true; };
			void release () { if(!locked) return; DB(--tracker->lock_count); OSSpinLockUnlock(&spinlock); locked = false; }

		private:
			document_tracker_t* tracker;
			bool locked;
		};

		document_ptr create (std::string const& path, path::identifier_t const& key)
		{
			lock_t lock(this);
			D(DBF_Document_Tracker, bug("%s\n", path.c_str()););

			std::map<path::identifier_t, document_weak_ptr>::const_iterator it = documents_by_path.find(key);
			if(it != documents_by_path.end())
			{
				if(document_ptr res = it->second.lock())
				{
					D(DBF_Document_Tracker, bug("re-use instance (%s)\n", res->path().c_str()););
					lock.release();
					if(pthread_self() == MainThread)
						res->set_path(path);
					return res;
				}
				else
				{
					D(DBF_Document_Tracker, bug("*** old instance gone\n"););
				}
			}

			document_ptr res = document_ptr(new document_t);
			res->_identifier.generate();
			res->_path = path;
			res->_key  = key;

			add(res);
			return res;
		}

		document_ptr find (oak::uuid_t const& uuid, bool searchBackups)
		{
			lock_t lock(this);
			D(DBF_Document_Tracker, bug("%s\n", to_s(uuid).c_str()););
			std::map<oak::uuid_t, document_weak_ptr>::const_iterator it = documents.find(uuid);
			if(it != documents.end())
			{
				if(document_ptr res = it->second.lock())
				{
					D(DBF_Document_Tracker, bug("re-use instance\n"););
					return res;
				}
				else
				{
					D(DBF_Document_Tracker, bug("*** old instance gone\n"););
				}
			}

			path::walker_ptr walker = path::open_for_walk(session_dir());
			iterate(path, *walker)
			{
				std::string const& attr = path::get_attr(*path, "com.macromates.backup.identifier");
				if(attr != NULL_STR && uuid == oak::uuid_t(attr))
				{
					document_ptr res = document_ptr(new document_t);

					res->_identifier     = uuid;
					res->_backup_path    = *path;

					res->_path           = path::get_attr(*path, "com.macromates.backup.path");
					res->_key            = path::identifier_t(res->_path);
					res->_file_type      = path::get_attr(*path, "com.macromates.backup.file-type");
					res->_disk_encoding  = path::get_attr(*path, "com.macromates.backup.encoding");
					res->_disk_bom       = path::get_attr(*path, "com.macromates.backup.bom") == "YES";
					res->_disk_newlines  = path::get_attr(*path, "com.macromates.backup.newlines");
					res->_untitled_count = atoi(path::get_attr(*path, "com.macromates.backup.untitled-count").c_str());
					res->_custom_name    = path::get_attr(*path, "com.macromates.backup.custom-name");
					res->_modified       = path::get_attr(*path, "com.macromates.backup.modified") == "YES";

					add(res);
					return res;
				}
			}
			D(DBF_Document_Tracker, bug("no instance found\n"););
			return document_ptr();
		}

		void remove (oak::uuid_t const& uuid, path::identifier_t const& key)
		{
			lock_t lock(this);
			D(DBF_Document_Tracker, bug("%s, %s\n", to_s(uuid).c_str(), to_s(key).c_str()););
			if(key)
			{
				std::map<path::identifier_t, document_weak_ptr>::iterator it = documents_by_path.find(key);
				ASSERTF(it != documents_by_path.end(), "%s, %s", to_s(key).c_str(), to_s(uuid).c_str());
				if(!it->second.lock())
				{
					documents_by_path.erase(it);
				}
				else
				{
					D(DBF_Document_Tracker, bug("*** old instance replaced\n"););
				}
			}
			ASSERT(documents.find(uuid) != documents.end());
			documents.erase(uuid);
		}

		path::identifier_t const& update_path (document_ptr doc, path::identifier_t const& oldKey, path::identifier_t const& newKey)
		{
			lock_t lock(this);
			D(DBF_Document_Tracker, bug("%s → %s\n", to_s(oldKey).c_str(), to_s(newKey).c_str()););
			if(oldKey)
			{
				ASSERT(documents_by_path.find(oldKey) != documents_by_path.end());
				documents_by_path.erase(oldKey);
			}

			if(newKey)
			{
				ASSERT(documents_by_path.find(newKey) == documents_by_path.end());
				documents_by_path.insert(std::make_pair(newKey, doc));
			}

			return newKey;
		}

		size_t untitled_counter ()
		{
			lock_t lock(this);

			std::set<size_t> reserved;
			iterate(pair, documents)
			{
				if(document_ptr doc = pair->second.lock())
				{
					if(doc->path() == NULL_STR)
						reserved.insert(doc->untitled_count());
				}
			}

			size_t res = 1;
			while(reserved.find(res) != reserved.end())
				++res;
			return res;
		}

		std::map<oak::uuid_t, document_weak_ptr> documents;
		std::map<path::identifier_t, document_weak_ptr> documents_by_path;

	private:
		void add (document_ptr doc)
		{
			ASSERT_EQ(lock_count, 1); // we assert that a lock has been obtained by the caller
			documents.insert(std::make_pair(doc->identifier(), doc));
			if(doc->_key)
			{
				D(DBF_Document_Tracker, bug("%s\n", doc->path().c_str()););
				std::map<path::identifier_t, document_weak_ptr>::iterator it = documents_by_path.find(doc->_key);
				ASSERTF(it == documents_by_path.end() || !it->second.lock(), "%s, %s\n", to_s(doc->_key).c_str(), to_s(doc->identifier()).c_str());
				if(it == documents_by_path.end())
						documents_by_path.insert(std::make_pair(doc->_key, doc));
				else  it->second = doc;
			}
		}

	} documents;

	document_ptr create (std::string const& rawPath)                             { std::string const path = path::resolve(rawPath); return path::is_text_clipping(path) ? from_content(path::resource(path, typeUTF8Text, 256)) : documents.create(path, path::identifier_t(path)); }
	document_ptr create (std::string const& path, path::identifier_t const& key) { return documents.create(path, key); }
	document_ptr find (oak::uuid_t const& uuid, bool searchBackups)              { return documents.find(uuid, searchBackups); }

	document_ptr from_content (std::string const& content, std::string const& fileType)
	{
		D(DBF_Document, bug("%s\n", fileType.c_str()););
		document_ptr doc = create();
		if(fileType != NULL_STR)
			doc->set_file_type(fileType);
		doc->set_content(io::bytes_ptr(new io::bytes_t(content)));
		return doc;
	}

	// ===============
	// = LRU Tracker =
	// ===============

	static struct lru_tracker_t
	{
		lru_tracker_t () : did_load(false) { }

		oak::date_t get (std::string const& path) const
		{
			if(path == NULL_STR)
				return oak::date_t();

			load();
			std::map<std::string, oak::date_t>::const_iterator it = map.find(path);
			D(DBF_Document_LRU, bug("%s → %s\n", path.c_str(), it != map.end() ? to_s(it->second).c_str() : "not found"););
			return it == map.end() ? oak::date_t() : it->second;
		}

		void set (std::string const& path, oak::date_t const& date)
		{
			if(path == NULL_STR)
				return;

			D(DBF_Document_LRU, bug("%s → %s\n", path.c_str(), to_s(date).c_str()););
			load();
			map[path] = date;
			save();
		}

	private:
		void load () const
		{
			if(did_load)
				return;
			did_load = true;

			if(CFPropertyListRef cfPlist = CFPreferencesCopyAppValue(CFSTR("LRUDocumentPaths"), kCFPreferencesCurrentApplication))
			{
				plist::dictionary_t const& plist = plist::convert(cfPlist);
				D(DBF_Document_LRU, bug("%s\n", to_s(plist).c_str()););
				CFRelease(cfPlist);

				plist::array_t paths;
				if(plist::get_key_path(plist, "paths", paths))
				{
					oak::date_t t = oak::date_t::now();
					iterate(path, paths)
					{
						if(std::string const* str = boost::get<std::string>(&*path))
							map.insert(std::make_pair(*str, t - (1.0 + map.size())));
					}
				}
			}
		}

		void save () const
		{
			std::map<oak::date_t, std::string> sorted;
			iterate(item, map)
				sorted.insert(std::make_pair(item->second, item->first));

			std::map< std::string, std::vector<std::string> > plist;
			std::vector<std::string>& paths = plist["paths"];
			riterate(item, sorted)
			{
				paths.push_back(item->second);
				if(paths.size() == 50)
					break;
			}

			D(DBF_Document_LRU, bug("%s\n", text::join(paths, ", ").c_str()););
			CFPreferencesSetAppValue(CFSTR("LRUDocumentPaths"), cf::wrap(plist), kCFPreferencesCurrentApplication);
		}

		mutable std::map<std::string, oak::date_t> map;
		mutable bool did_load;

	} lru;

	// =========
	// = Marks =
	// =========

	static struct mark_tracker_t
	{
		typedef std::multimap<text::range_t, document_t::mark_t> marks_t;

		marks_t get (std::string const& path)
		{
			if(path == NULL_STR)
				return marks_t();

			std::map<std::string, marks_t>::const_iterator it = marks.find(path);
			if(it == marks.end())
				it = marks.insert(std::make_pair(path, parse_marks(path::get_attr(path, "com.macromates.bookmarks")))).first;
			return it->second;
		}

		void set (std::string const& path, marks_t const& m)
		{
			if(m.empty())
					marks.erase(path);
			else	marks[path] = m;
		}

		std::map<std::string, marks_t> marks;

	} marks;

	// ==============
	// = document_t =
	// ==============

	document_t::~document_t ()
	{
		D(DBF_Document, bug("%s\n", display_name().c_str()););
		if(_grammar)
			_grammar->remove_callback(&_grammar_callback);
		if(_path != NULL_STR && _buffer)
			document::marks.set(_path, marks());
		documents.remove(_identifier, _key);
	}

	std::string document_t::display_name () const
	{
		if(_custom_name != NULL_STR)
			return _custom_name;
		if(_path != NULL_STR)
			return path::display_name(_path);

		if(!_untitled_count)
			_untitled_count = documents.untitled_counter();

		return _untitled_count == 1 ? "untitled" : text::format("untitled %zu", _untitled_count);
	}

	std::string document_t::backup_path () const
	{
		if(_backup_path == NULL_STR)
			_backup_path = ::backup_path(display_name());
		return _backup_path;
	}

	bool is_binary (std::string const& path)
	{
		D(DBF_Document_Binary, bug("%s\n", path.c_str()););
		if(path == NULL_STR)
			return false;

		settings_t const& settings = settings_for_path(path);
		if(settings.has(kSettingsBinaryKey))
		{
			D(DBF_Document_Binary, bug(".tm_properties reports it as binary: %s\n", BSTR(path::glob_t(settings.get(kSettingsBinaryKey, "")).does_match(path))););
			return path::glob_t(settings.get(kSettingsBinaryKey, "")).does_match(path);
		}

		return false;
	}

	std::string document_t::file_type () const
	{
		D(DBF_Document, bug("%s, %s\n", display_name().c_str(), _file_type.c_str()););
		return _file_type;
	}

	std::map<std::string, std::string> document_t::variables (std::map<std::string, std::string> map, bool sourceFileSystem) const
	{
		map["TM_DISPLAYNAME"]   = display_name();
		map["TM_DOCUMENT_UUID"] = to_s(identifier());

		if(path() != NULL_STR)
		{
			map["TM_FILEPATH"]  = path();
			map["TM_FILENAME"]  = path::name(path());
			map["TM_DIRECTORY"] = path::parent(path());
			map["PWD"]          = path::parent(path());

			if(scm::info_ptr info = scm::info(path()))
			{
				std::string const& branch = info->branch();
				if(branch != NULL_STR)
					map["TM_SCM_BRANCH"] = branch;
			}
		}

		return sourceFileSystem ? variables_for_path(path(), scope(), map) : map;
	}

	void document_t::setup_buffer ()
	{
		D(DBF_Document, bug("%s, %s\n", display_name().c_str(), _file_type.c_str()););
		if(_file_type != NULL_STR)
		{
			citerate(item, bundles::query(bundles::kFieldGrammarScope, _file_type, scope::wildcard, bundles::kItemTypeGrammar))
			{
				if(parse::grammar_ptr grammar = parse::parse_grammar(*item))
				{
					if(_grammar)
						_grammar->remove_callback(&_grammar_callback);

					_grammar = grammar;
					_grammar->add_callback(&_grammar_callback);

					_buffer->set_grammar(*item);

					break;
				}
			}
		}

		settings_t const& settings = this->settings();
		_buffer->indent() = text::indent_t(settings.get(kSettingsTabSizeKey, 4), SIZE_T_MAX, settings.get(kSettingsSoftTabsKey, false));
		_buffer->set_spelling_language(settings.get(kSettingsSpellingLanguageKey, "en"));
		_buffer->set_live_spelling(settings.get(kSettingsSpellCheckingKey, false));

		const_cast<document_t*>(this)->broadcast(callback_t::did_change_indent_settings);

		D(DBF_Document, bug("done\n"););
	}

	void document_t::grammar_did_change ()
	{
		_buffer->set_grammar(bundles::lookup(_grammar->uuid())); // Preferably we’d pass _grammar to the buffer but then the buffer couldn’t get at the root scope and folding markers. Perhaps this should be exposed by grammar_t, but ideally the buffer itself would setup a callback to be notified about grammar changes. We only moved it to document_t because with a callback, buffer_t can’t get copy constructors for free.
	}

	void document_t::mark_pristine ()
	{
		ASSERT(_buffer);
		_pristine_buffer = _buffer->substr(0, _buffer->size()); // TODO We should use a cheap ng::detail::storage_t copy
	}

	void document_t::post_load (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& newlines)
	{
		_open_callback.reset();
		if(!content)
		{
			_open_count = 0;
			return;
		}

		_path_attributes = pathAttributes;

		_disk_encoding = encoding;
		_disk_newlines = newlines;
		_disk_bom      = bom;

		if(_file_type == NULL_STR)
			_file_type = fileType;

		if(_selection == NULL_STR)
		{
			std::map<std::string, std::string>::const_iterator sel  = attributes.find("com.macromates.selectionRange");
			std::map<std::string, std::string>::const_iterator rect = attributes.find("com.macromates.visibleRect");
			_selection    = sel != attributes.end()  ? sel->second  : NULL_STR;
			_visible_rect = rect != attributes.end() ? rect->second : NULL_STR;
		}

		_is_on_disk = _path != NULL_STR && access(_path.c_str(), F_OK) == 0;
		if(_is_on_disk)
			_file_watcher.reset(new watch_t(_path, shared_from_this()));

		_buffer.reset(new ng::buffer_t);
		setup_buffer();
		if(content)
		{
			_buffer->insert(0, std::string(content->begin(), content->end()));
			setup_marks(path, *_buffer);

			std::map<std::string, std::string>::const_iterator folded = attributes.find("com.macromates.folded");
			if(folded != attributes.end())
				_folded = folded->second;
		}
		_buffer->bump_revision();
		check_modified(_buffer->revision(), _buffer->revision());
		mark_pristine();
		_undo_manager.reset(new ng::undo_manager_t(buffer()));

		broadcast(callback_t::did_change_open_status);
	}

	void document_t::post_save (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, bool success)
	{
		if(success)
		{
			_key        = documents.update_path(shared_from_this(), _key, path::identifier_t(_path));
			_is_on_disk = true;

			_path_attributes = pathAttributes;

			_disk_encoding = encoding;
			_disk_bom      = bom;
			_disk_newlines = lineFeeds;

			check_modified(revision(), revision());
			mark_pristine();
			broadcast(callback_t::did_save);

			D(DBF_Document, bug("search for ‘did save’ hooks in scope ‘%s’\n", to_s(scope()).c_str()););
			citerate(item, bundles::query(bundles::kFieldSemanticClass, "callback.document.did-save", scope()))
			{
				D(DBF_Document, bug("%s\n", (*item)->name().c_str()););
				document::run(parse_command(*item), buffer(), ng::ranges_t(), shared_from_this());
			}
		}

		if(_is_on_disk)
			_file_watcher.reset(new watch_t(_path, shared_from_this()));
	}

	void document_t::try_save (document::save_callback_ptr callback)
	{
		struct save_callback_wrapper_t : file::save_callback_t
		{
			save_callback_wrapper_t (document::document_ptr doc, document::save_callback_ptr callback, bool close) : _document(doc), _callback(callback), _close(close) { }

			void select_path (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)                                     { _callback->select_path(path, content, context); }
			void select_make_writable (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)                            { _callback->select_make_writable(path, content, context); }
			void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, file::save_context_ptr context) { _callback->obtain_authorization(path, content, auth, context); }
			void select_encoding (std::string const& path, io::bytes_ptr content, std::string const& encoding, file::save_context_ptr context)    { _callback->select_encoding(path, content, encoding, context); }

			void did_save (std::string const& path, io::bytes_ptr content, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, bool success, std::string const& message, oak::uuid_t const& filter)
			{
				_document->post_save(path, content, pathAttributes, encoding, bom, lineFeeds, success);
				_callback->did_save_document(_document, path, success, message, filter);
				if(_close)
					_document->close();
			}

		private:
			document::document_ptr _document;
			document::save_callback_ptr _callback;
			bool _close;
		};

		D(DBF_Document, bug("save ‘%s’\n", _path.c_str()););

		bool closeAfterSave = false;
		if(!is_open())
		{
			if(!_content && _backup_path == NULL_STR)
				return callback->did_save(_path, io::bytes_ptr(), _path_attributes, _disk_encoding, _disk_bom, _disk_newlines, false, NULL_STR, oak::uuid_t());
			open();
			closeAfterSave = true;
		}

		_file_watcher.reset();

		io::bytes_ptr bytes(new io::bytes_t(content()));

		std::map<std::string, std::string> attributes;
		if(volume::settings(_path).extended_attributes())
		{
			attributes["com.macromates.selectionRange"] = _selection;
			attributes["com.macromates.visibleRect"]    = _visible_rect;
			attributes["com.macromates.bookmarks"]      = marks_as_string();
			attributes["com.macromates.folded"]         = _folded;
		}

		save_callback_wrapper_t* cb = new save_callback_wrapper_t(shared_from_this(), callback, closeAfterSave);
		save_callback_ptr sharedPtr((save_callback_t*)cb);
		file::save(_path, sharedPtr, _authorization, bytes, attributes, _file_type, _disk_encoding, _disk_bom, _disk_newlines, std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
	}

	bool document_t::save ()
	{
		struct stall_t : save_callback_t
		{
			stall_t (bool& res) : _res(res), _run_loop(CFSTR("OakThreadSignalsRunLoopMode")) { }

			void did_save_document (document_ptr document, std::string const& path, bool success, std::string const& message, oak::uuid_t const& filter)
			{
				_res = success;
				_run_loop.stop();
			}

			void wait () { _run_loop.start(); }

		private:
			bool& _res;
			cf::run_loop_t _run_loop;
		};

		bool res = false;
		stall_t* cb = new stall_t(res);
		save_callback_ptr sharedPtr((save_callback_t*)cb);
		try_save(sharedPtr);
		cb->wait();

		return res;
	}

	bool document_t::backup ()
	{
		ASSERT(_buffer);
		std::string const& dst = backup_path();
		if(path::set_content(dst, content()))
		{
			path::set_attr(dst, "com.macromates.backup.path",           _path);
			path::set_attr(dst, "com.macromates.backup.identifier",     to_s(_identifier));
			path::set_attr(dst, "com.macromates.selectionRange",        _selection);
			path::set_attr(dst, "com.macromates.visibleRect",           _visible_rect);
			path::set_attr(dst, "com.macromates.backup.file-type",      _file_type);
			path::set_attr(dst, "com.macromates.backup.encoding",       _disk_encoding);
			path::set_attr(dst, "com.macromates.backup.bom",            _disk_bom ? "YES" : "NO");
			path::set_attr(dst, "com.macromates.backup.newlines",       _disk_newlines);
			path::set_attr(dst, "com.macromates.backup.untitled-count", text::format("%zu", _untitled_count));
			path::set_attr(dst, "com.macromates.backup.custom-name",    _custom_name);
			path::set_attr(dst, "com.macromates.bookmarks",             marks_as_string());
			path::set_attr(dst, "com.macromates.folded",                NULL_STR);
			if(is_modified())
				path::set_attr(dst, "com.macromates.backup.modified", "YES");

			// TODO tab size, spell checking, soft wrap, etc. should go into session!?!

			_backup_revision = revision();

			return true;
		}
		return false;
	}

	void document_t::check_modified (ssize_t diskRev, ssize_t rev)
	{
		_disk_revision = diskRev;
		_revision = rev;

		set_modified(_revision != _disk_revision && (!buffer().empty() || is_on_disk()));
		if(is_modified())
				schedule_backup(identifier());
		else	cancel_backup(identifier());
	}

	bool document_t::is_modified () const
	{
		return _modified;
	}

	void document_t::set_modified (bool flag)
	{
		if(_modified != flag)
		{
			_modified = flag;

			broadcast(callback_t::did_change_modified_status);
			if(!_modified && _backup_path != NULL_STR && access(_backup_path.c_str(), F_OK) == 0)
				unlink(_backup_path.c_str());
		}
	}

	void document_t::set_path (std::string const& newPath)
	{
		std::string const& normalizedPath = path::resolve(newPath);
		if(_path == normalizedPath)
			return;

		_path = normalizedPath;
		_key  = documents.update_path(shared_from_this(), _key, path::identifier_t(normalizedPath));
		if(is_open())
		{
			_is_on_disk = access(_path.c_str(), F_OK) == 0;
			_file_watcher.reset(_is_on_disk ? new watch_t(_path, shared_from_this()) : NULL);

			std::string newFileType = file::type(_path, io::bytes_ptr(new io::bytes_t(content())), _virtual_path);
			if(newFileType != NULL_STR)
				set_file_type(newFileType);
		}
		_custom_name = NULL_STR;
		broadcast(callback_t::did_change_path);
	}

	bool document_t::try_open (document::open_callback_ptr callback)
	{
		if(++_open_count == 1)
		{
			if(_backup_path != NULL_STR)
			{
				bool modified = _modified;
				post_load(_path, io::bytes_ptr(new io::bytes_t(path::content(_backup_path))), path::attributes(_backup_path), _file_type, file::path_attributes(_path), _disk_encoding, _disk_bom, _disk_newlines);
				if(modified)
					set_revision(buffer().bump_revision());
				return true;
			}

			_open_callback.reset(new open_callback_wrapper_t(shared_from_this(), callback));
			file::open(_path, _authorization, _open_callback, _content, _virtual_path);
			_content.reset();
			return false;
		}
		else if(_open_callback)
		{
			_open_callback->add_callback(callback);
			return false;
		}
		else
		{
			ASSERT(_buffer); // load completed
			return true;
		}
	}

	void document_t::open ()
	{
		struct stall_t : document::open_callback_t
		{
			stall_t () : _run_loop(CFSTR("OakThreadSignalsRunLoopMode")) { }
			void show_document (std::string const& path, document_ptr document)                                                     { _run_loop.stop(); }
			void show_error (std::string const& path, document_ptr document, std::string const& message, oak::uuid_t const& filter) { _run_loop.stop(); }
			void wait () { _run_loop.start(); }

		private:
			cf::run_loop_t _run_loop;
		};

		stall_t* cb = new stall_t;
		document::open_callback_ptr sharedPtr((document::open_callback_t*)cb);
		if(!try_open(sharedPtr))
			cb->wait();
	}

	void document_t::close ()
	{
		if(--_open_count != 0)
			return;

		broadcast(callback_t::did_change_open_status);
		_file_watcher.reset();

		if(_path != NULL_STR && !is_modified() && volume::settings(_path).extended_attributes())
		{
			D(DBF_Document, bug("save attributes for ‘%s’\n", _path.c_str()););
			path::set_attr(_path, "com.macromates.selectionRange", _selection);
			path::set_attr(_path, "com.macromates.visibleRect",    _visible_rect);
			path::set_attr(_path, "com.macromates.bookmarks",      marks_as_string());
		}

		if(_backup_path != NULL_STR && access(_backup_path.c_str(), F_OK) == 0)
			unlink(_backup_path.c_str());
		_backup_path = NULL_STR;

		check_modified(-1, -1);
		_undo_manager.reset();
		_buffer.reset();
		_pristine_buffer = NULL_STR;
	}

	void document_t::show ()
	{
		_has_lru = true;
		document::lru.set(_path, _lru = oak::date_t::now());
	}

	void document_t::hide ()
	{
		_has_lru = true;
		document::lru.set(_path, _lru = oak::date_t::now());
	}

	oak::date_t const& document_t::lru () const
	{
		if(!_has_lru)
		{
			_has_lru = true;
			_lru = document::lru.get(_path);
		}
		return _lru;
	}

	void document_t::watch_callback (int flags, std::string const& newPath, bool async)
	{
		ASSERT(_file_watcher);
		ASSERT(is_open());

		// NOTE_ATTRIB
		if((flags & NOTE_RENAME) == NOTE_RENAME)
		{
			set_path(newPath);
		}
		else if((flags & NOTE_DELETE) == NOTE_DELETE)
		{
			D(DBF_Document_WatchFS, bug("%s deleted\n", _path.c_str()););
			if(_is_on_disk && !(_is_on_disk = access(_path.c_str(), F_OK) == 0))
				broadcast(callback_t::did_change_on_disk_status);
		}
		else if((flags & NOTE_WRITE) == NOTE_WRITE || (flags & NOTE_CREATE) == NOTE_CREATE)
		{
			struct open_callback_t : file::open_callback_t
			{
				open_callback_t (document::document_ptr doc, bool async) : _document(doc), _wait(!async) { }

				void select_encoding (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)   { context->set_encoding(_document->_disk_encoding); }
				void select_line_feeds (std::string const& path, io::bytes_ptr content, file::open_context_ptr context) { context->set_line_feeds(_document->_disk_newlines); }
				void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)  { context->set_file_type(_document->_file_type); }
				void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)        { fprintf(stderr, "%s: %s\n", path.c_str(), message.c_str()); }

				void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, std::string const& pathAttributes, std::string const& encoding, bool bom, std::string const& lineFeeds, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
				{
					if(!_document->is_open())
						return;

					std::string const& yours = std::string(content->begin(), content->end());
					std::string const& mine  = _document->content();

					if(yours == mine)
					{
						D(DBF_Document_WatchFS, bug("yours == mine, marking document as not modified\n"););
						_document->set_disk_revision(_document->revision());
						_document->mark_pristine();
					}
					else if(!_document->is_modified())
					{
						D(DBF_Document_WatchFS, bug("changed on disk and we have no local changes, so reverting to that\n"););
						_document->_buffer->replace(0, _document->_buffer->size(), yours);
						_document->_buffer->bump_revision();
						_document->check_modified(_document->_buffer->revision(), _document->_buffer->revision());
						_document->mark_pristine();
					}
					else
					{
						bool conflict = false;
						std::string const& merged = merge(_document->_pristine_buffer, mine, yours, &conflict);
						D(DBF_Document_WatchFS, bug("changed on disk and we have local changes, merge conflict %s.\n%s\n", BSTR(conflict), merged.c_str()););
						_document->_buffer->replace(0, _document->_buffer->size(), merged);
						_document->set_revision(_document->_buffer->bump_revision());
						// TODO if there was a conflict, we shouldn’t take the merged content (but ask user what to do)
						// TODO mark_pristine() but using ‘yours’
					}

					if(_wait)
						_run_loop.stop();
				}

				void wait () { if(_wait) _run_loop.start(); }

			private:
				document::document_ptr _document;
				bool _wait;
				cf::run_loop_t _run_loop;
			};

			if(!_is_on_disk && (_is_on_disk = access(_path.c_str(), F_OK) == 0))
				broadcast(callback_t::did_change_on_disk_status);

			open_callback_t* raw = new open_callback_t(shared_from_this(), async);
			file::open_callback_ptr cb((file::open_callback_t*)raw);
			file::open(_path, _authorization, cb);
			raw->wait();
		}
	}

	void document_t::set_file_type (std::string const& newFileType)
	{
		D(DBF_Document, bug("%s → %s (%s)\n", _file_type.c_str(), newFileType.c_str(), display_name().c_str()););
		if(_file_type != newFileType)
		{
			_file_type = newFileType;
			if(_buffer)
				setup_buffer();
			broadcast(callback_t::did_change_file_type);
		}
	}

	void document_t::set_content (io::bytes_ptr const& bytes)
	{
		D(DBF_Document, bug("%.*s… (%zu bytes), file type %s\n", std::min<int>(32, bytes->size()), bytes->get(), bytes->size(), _file_type.c_str()););
		ASSERT(!_buffer);
		_content = bytes;
	}

	namespace
	{
		struct file_reader_t : reader::open_t
		{
			WATCH_LEAKS(file_reader_t);
			file_reader_t (document_const_ptr const& document) : reader::open_t(document->path()), document(document) { }
		private:
			document_const_ptr document;
		};

		struct buffer_reader_t : document::document_t::reader_t
		{
			WATCH_LEAKS(buffer_reader_t);
			buffer_reader_t (io::bytes_ptr const& data) : _data(data) { }

			io::bytes_ptr next ()
			{
				io::bytes_ptr res = _data;
				_data.reset();
				return res;
			}

		private:
			io::bytes_ptr _data;
		};
	}

	document_t::reader_ptr document_t::create_reader () const
	{
		if(is_open())
			return reader_ptr(new buffer_reader_t(io::bytes_ptr(new io::bytes_t(content()))));
		return reader_ptr(new file_reader_t(shared_from_this()));
	}

	// ===========
	// = Replace =
	// ===========

	void document_t::replace (std::multimap<text::range_t, std::string> const& replacements)
	{
		ASSERT(!is_open());

		if(replacements.empty())
			return;

		ASSERT(_path != NULL_STR);
		ASSERT(!_buffer)

		ng::buffer_t buf;
		buf.insert(0, path::content(_path));

		riterate(pair, replacements)
		{
			D(DBF_Document_Replace, bug("replace %s with ‘%s’\n", std::string(pair->first).c_str(), pair->second.c_str()););
			buf.replace(buf.convert(pair->first.min()), buf.convert(pair->first.max()), pair->second);
		}

		_content.reset(new io::bytes_t(buf.substr(0, buf.size())));
	}

	static ng::index_t cap (ng::buffer_t const& buf, text::pos_t const& pos)
	{
		size_t line = oak::cap<size_t>(0, pos.line,   buf.lines()-1);
		size_t col  = oak::cap<size_t>(0, pos.column, buf.eol(line) - buf.begin(line));
		ng::index_t res = buf.sanitize_index(buf.convert(text::pos_t(line, col)));
		if(pos.offset && res.index < buf.size() && buf[res.index] == "\n")
			res.carry = pos.offset;
		return res;
	}

	// =========
	// = Marks =
	// =========

	void document_t::load_marks (std::string const& src) const
	{
		if(_did_load_marks)
			return;

		if(src != NULL_STR)
		{
			_marks = document::marks.get(src);
			document::marks.set(src, std::multimap<text::range_t, document_t::mark_t>());
		}

		_did_load_marks = true;
	}

	static void copy_marks (ng::buffer_t& buf, std::multimap<text::range_t, document_t::mark_t> const& marks)
	{
		iterate(pair, marks)
			buf.set_mark(cap(buf, pair->first.from).index, pair->second.type);
	}

	void document_t::setup_marks (std::string const& src, ng::buffer_t& buf) const
	{
		if(_did_load_marks)
		{
			copy_marks(buf, _marks);
		}
		else if(src != NULL_STR)
		{
			copy_marks(buf, document::marks.get(src));
			document::marks.set(src, std::multimap<text::range_t, document_t::mark_t>());
		}
	}

	std::multimap<text::range_t, document_t::mark_t> document_t::marks () const
	{
		if(_buffer)
		{
			std::multimap<text::range_t, document_t::mark_t> res;
			citerate(pair, _buffer->get_marks(0, _buffer->size()))
				res.insert(std::make_pair(_buffer->convert(pair->first), pair->second));
			return res;
		}
		return document::marks.get(_path);
	}

	void document_t::add_mark (text::range_t const& range, mark_t const& mark)
	{
		if(_buffer)
		{
			_buffer->set_mark(_buffer->convert(range.from), mark.type);
		}
		else
		{
			load_marks(_path);
			_marks.insert(std::make_pair(range, mark));
		}
		broadcast(callback_t::did_change_marks);
	}

	void document_t::remove_all_marks (std::string const& typeToClear)
	{
		if(_buffer)
		{
			_buffer->remove_all_marks(typeToClear);
		}
		else
		{
			load_marks(_path);

			std::multimap<text::range_t, mark_t> newMarks;
			if(typeToClear != NULL_STR)
			{
				iterate(it, _marks)
				{
					if(it->second.type != typeToClear)
						newMarks.insert(*it);
				}
			}

			_marks.swap(newMarks);
		}
		broadcast(callback_t::did_change_marks);
	}

	std::string document_t::marks_as_string () const
	{
		std::vector<std::string> v;
		if(_buffer)
		{
			citerate(pair, _buffer->get_marks(0, _buffer->size()))
			{
				if(pair->second == "bookmark")
					v.push_back(text::format("'%s'", std::string(_buffer->convert(pair->first)).c_str()));
			}
		}
		else
		{
			load_marks(_path);
			iterate(mark, _marks)
			{
				if(mark->second.type == "bookmark")
					v.push_back(text::format("'%s'", std::string(mark->first).c_str()));
			}
		}
		return v.empty() ? NULL_STR : "( " + text::join(v, ", ") + " )";
	}

	// ===========
	// = Symbols =
	// ===========

	std::map<text::pos_t, std::string> document_t::symbols ()
	{
		if(!_buffer)
			return std::map<text::pos_t, std::string>();

		_buffer->wait_for_repair();

		std::map<text::pos_t, std::string> res;
		citerate(pair, _buffer->symbols())
			res.insert(std::make_pair(_buffer->convert(pair->first), pair->second));
		return res;
	}

	// ====================
	// = Document scanner =
	// ====================

	std::vector<document_ptr> scanner_t::open_documents ()
	{
		std::vector<document_ptr> res;

		document_tracker_t::lock_t lock(&document::documents);
		iterate(pair, document::documents.documents)
		{
			document_ptr doc = pair->second.lock();
			if(doc && doc->is_open())
				res.push_back(doc);
		}

		return res;
	}

	scanner_t::scanner_t (std::string const& path, std::string const& glob, std::string const& excludeGlob, bool follow_links, bool follow_hidden_folders, bool depth_first) : path(path), glob(glob), exclude_glob(excludeGlob), follow_links(follow_links), follow_hidden_folders(follow_hidden_folders), depth_first(depth_first), is_running_flag(true), should_stop_flag(false)
	{
		D(DBF_Document_Scanner, bug("%s / %s (excl. %s), links %s, include hidden %s\n", path.c_str(), glob.c_str(), excludeGlob.c_str(), BSTR(follow_links), BSTR(follow_hidden_folders)););

		document_tracker_t::lock_t lock(&document::documents);
		iterate(pair, document::documents.documents)
		{
			document_ptr doc = pair->second.lock();
			if(doc && doc->path() == NULL_STR)
				documents.push_back(doc);
		}

		struct bootstrap_t { static void* main (void* arg) { ((scanner_t*)arg)->thread_main(); return NULL; } };
		pthread_mutex_init(&mutex, NULL);
		pthread_create(&thread, NULL, &bootstrap_t::main, this);
	}

	scanner_t::~scanner_t ()
	{
		D(DBF_Document_Scanner, bug("\n"););
		stop();
		wait();
		pthread_mutex_destroy(&mutex);
	}

	std::vector<document_ptr> scanner_t::accept_documents ()
	{
		pthread_mutex_lock(&mutex);
		std::vector<document_ptr> res;
		res.swap(documents);
		pthread_mutex_unlock(&mutex);
		return res;
	}

	std::string scanner_t::get_current_path () const
	{
		pthread_mutex_lock(&mutex);
		std::string res = current_path;
		pthread_mutex_unlock(&mutex);
		return res;
	}

	void scanner_t::thread_main ()
	{
		oak::set_thread_name("document::scanner_t");

		scan_dir(path);
		D(DBF_Document_Scanner, bug("running %s → NO\n", BSTR(is_running_flag)););
		is_running_flag = false;
	}

	void scanner_t::scan_dir (std::string const& initialPath)
	{
		D(DBF_Document_Scanner, bug("%s, running %s\n", initialPath.c_str(), BSTR(is_running_flag)););

		path::glob_t ptrn(glob), exclPtrn(exclude_glob);
		std::deque<std::string> dirs(1, initialPath);
		std::vector<std::string> links;
		while(!dirs.empty())
		{
			std::string dir = dirs.front();
			dirs.pop_front();

			struct stat buf;
			if(lstat(initialPath.c_str(), &buf) != 0) // get st_dev so we don’t need to stat each path entry (unless it is a symbolic link)
				continue;

			ASSERT(S_ISDIR(buf.st_mode) || S_ISLNK(buf.st_mode));

			pthread_mutex_lock(&mutex);
			current_path = dir;
			pthread_mutex_unlock(&mutex);

			std::vector<std::string> newDirs;
			std::multimap<std::string, path::identifier_t, text::less_t> files;
			citerate(it, path::entries(dir))
			{
				if(should_stop_flag)
					break;

				std::string const& path = path::join(dir, (*it)->d_name);
				if(exclPtrn.does_match(path))
					continue;

				if((*it)->d_type == DT_DIR)
				{
					if((*it)->d_name[0] == '.' && !follow_hidden_folders)
						continue;

					if(seen_paths.insert(std::make_pair(buf.st_dev, (*it)->d_ino)).second)
							newDirs.push_back(path);
					else	D(DBF_Document_Scanner, bug("skip known path: ‘%s’\n", path.c_str()););
				}
				else if((*it)->d_type == DT_REG)
				{
					if(!ptrn.does_match(path))
						continue;

					if(seen_paths.insert(std::make_pair(buf.st_dev, (*it)->d_ino)).second)
							files.insert(std::make_pair(path, path::identifier_t(true, buf.st_dev, (*it)->d_ino, path)));
					else	D(DBF_Document_Scanner, bug("skip known path: ‘%s’\n", path.c_str()););
				}
				else if((*it)->d_type == DT_LNK)
				{
					links.push_back(path); // handle later since link may point to another device plus if link is “local” and will be seen later, we reported the local path rather than this link
				}
			}

			std::sort(newDirs.begin(), newDirs.end(), text::less_t());
			dirs.insert(depth_first ? dirs.begin() : dirs.end(), newDirs.begin(), newDirs.end());

			if(dirs.empty())
			{
				iterate(link, links)
				{
					std::string path = path::resolve(*link);
					if(lstat(path.c_str(), &buf) == 0)
					{
						if(S_ISDIR(buf.st_mode) && follow_links && seen_paths.insert(std::make_pair(buf.st_dev, buf.st_ino)).second)
						{
							D(DBF_Document_Scanner, bug("follow link: %s → %s\n", link->c_str(), path.c_str()););
							dirs.push_back(path);
						}
						else if(S_ISREG(buf.st_mode))
						{
							if(!ptrn.does_match(path::name(*link)))
								continue;

							if(seen_paths.insert(std::make_pair(buf.st_dev, buf.st_ino)).second)
									files.insert(std::make_pair(path, path::identifier_t(true, buf.st_dev, buf.st_ino, path)));
							else	D(DBF_Document_Scanner, bug("skip known path: ‘%s’\n", path.c_str()););
						}
					}
				}
				links.clear();
			}

			pthread_mutex_lock(&mutex);
			iterate(file, files)
				documents.push_back(document::create(file->first, file->second));
			pthread_mutex_unlock(&mutex);
		}
	}
	
} /* document */
