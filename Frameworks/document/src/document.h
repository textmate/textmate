#ifndef DOCUMENT_H_MIJOONQT
#define DOCUMENT_H_MIJOONQT

#include <buffer/buffer.h>
#include <undo/undo.h>
#include <plist/uuid.h>
#include <plist/date.h>
#include <settings/settings.h>
#include <text/types.h>
#include <authorization/authorization.h>
#include <file/bytes.h>
#include <file/open.h>
#include <file/save.h>
#include <file/encoding.h>
#include <scope/scope.h>
#include <regexp/glob.h>
#include <oak/debug.h>

namespace document
{
	struct watch_t;
	struct document_t;
	typedef std::shared_ptr<watch_t>          watch_ptr;
	typedef std::shared_ptr<document_t>       document_ptr;
	typedef std::shared_ptr<document_t const> document_const_ptr;
	typedef std::weak_ptr<document_t>         document_weak_ptr;

	struct PUBLIC open_callback_t : file::open_callback_t
	{
		virtual ~open_callback_t () { }
		virtual void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters) { }
		virtual void show_document (std::string const& path, document_ptr document) = 0;
		virtual void show_error (std::string const& path, document_ptr document, std::string const& message, oak::uuid_t const& filter) = 0;
		virtual void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter) { }
	};

	typedef std::shared_ptr<open_callback_t> open_callback_ptr;

	struct PUBLIC save_callback_t : file::save_callback_t
	{
		virtual ~save_callback_t () { }
		virtual void did_save_document (document_ptr document, std::string const& path, bool success, std::string const& message, oak::uuid_t const& filter) = 0;
		virtual void did_save (std::string const& path, io::bytes_ptr content, encoding::type const& encoding, bool success, std::string const& message, oak::uuid_t const& filter) { }
	};

	typedef std::shared_ptr<save_callback_t> save_callback_ptr;

	struct inode_t
	{
		inode_t () { }
		inode_t (dev_t device, ino_t inode, std::string const& path);
		inode_t (std::string const& path);

		operator bool () const { return valid; }
		bool operator== (inode_t const& rhs) const { return valid == rhs.valid && inode == rhs.inode && device == rhs.device; }
		bool operator!= (inode_t const& rhs) const { return valid != rhs.valid || inode != rhs.inode || device != rhs.device; }
		bool operator< (inode_t const& rhs) const;

		dev_t device = 0;
		ino_t inode  = 0;
		bool valid   = false;
	};

	struct PUBLIC document_t : std::enable_shared_from_this<document_t>
	{
		WATCH_LEAKS(document_t);

		document_t () : _did_load_marks(false), _selection(NULL_STR), _folded(NULL_STR), _disable_callbacks(false), _revision(0), _disk_revision(0), _modified(false), _path(NULL_STR), _open_count(0), _has_lru(false), _is_on_disk(false), _recent_tracking(true), _backup_path(NULL_STR), _backup_revision(0), _virtual_path(NULL_STR), _custom_name(NULL_STR), _untitled_count(0), _file_type(NULL_STR), /*_folder(NULL_STR),*/ _disk_encoding(NULL_STR), _disk_newlines(NULL_STR), _disk_bom(false) { }
		~document_t ();

		bool operator== (document_t const& rhs) const { return _identifier == rhs._identifier; }
		bool operator!= (document_t const& rhs) const { return _identifier != rhs._identifier; }

		// ============================================================
		// = Doing one-pass reading of file (find in arbitrary files) =
		// ============================================================

		struct reader_t { virtual io::bytes_ptr next () = 0; virtual ~reader_t () { } };
		typedef std::shared_ptr<reader_t> reader_ptr;
		reader_ptr create_reader () const;

		// ======================================================
		// = Performing replacements (from outside a text view) =
		// ======================================================

		void replace (std::multimap<std::pair<size_t, size_t>, std::string> const& replacements);

		// ===================================================================
		// = Controlling marks (bookmarks, warnings, errors, search matches) =
		// ===================================================================

		struct mark_t
		{
			WATCH_LEAKS(document_t::mark_t);

			mark_t (char const* type = "bookmark") : type(type), info("") { }
			mark_t (std::string const& type, std::string const& info = "") : type(type), info(info) { }
			bool operator== (mark_t const& rhs) const { return type == rhs.type && info == rhs.info; }
			bool operator!= (mark_t const& rhs) const { return type != rhs.type || info != rhs.info; }
			std::string type, info;
		};

		void add_mark (text::range_t const& range, mark_t const& mark = mark_t());
		void remove_all_marks (std::string const& typeToClear = NULL_STR);
		std::multimap<text::range_t, mark_t> marks () const;

	private:
		void load_marks (std::string const& src) const;
		void setup_marks (std::string const& src, ng::buffer_t& buf) const;
		std::string marks_as_string () const;
		mutable std::multimap<text::range_t, mark_t> _marks;
		mutable bool _did_load_marks;

		std::string _selection;
		std::string _folded;
		ng::index_t _visible_index;
		io::bytes_ptr _content;

		// ===============
		// = Symbol list =
		// ===============
	public:
		std::map<text::pos_t, std::string> symbols ();

		// ===================
		// = Callback system =
		// ===================

		struct callback_t
		{
			WATCH_LEAKS(document_t::callback_t);

			enum event_t
			{
				did_save,

				did_change_open_status,
				did_change_modified_status,
				did_change_on_disk_status,
				did_change_path,
				did_change_file_type,
				did_change_indent_settings,
				// did_change_display_name,
				did_change_marks,
				// did_change_symbols,
			};

			virtual ~callback_t () { }
			virtual void handle_document_event (document_ptr document, event_t event) = 0;
		};

		void add_callback (callback_t* callback)         { _callbacks.add(callback); }
		void remove_callback (callback_t* callback)      { _callbacks.remove(callback); }

	private:
		void check_modified (ssize_t diskRev, ssize_t rev);

		void broadcast (callback_t::event_t event, bool cascade = true)
		{
			if(_disable_callbacks)
				return;

			_disable_callbacks = !cascade;
			_callbacks(&callback_t::handle_document_event, shared_from_this(), event);
			_disable_callbacks = false;
		}

		oak::callbacks_t<callback_t> _callbacks;
		bool _disable_callbacks;

		// ===================
		// = For OakTextView =
		// ===================

		void post_load (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding);

		struct open_callback_wrapper_t : file::open_callback_t
		{
			open_callback_wrapper_t (document::document_ptr doc, document::open_callback_ptr callback) : _document(doc), _callbacks(1, callback) { }

			void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)    { _callbacks[0]->select_charset(path, content, context); }
			void select_line_feeds (std::string const& path, io::bytes_ptr content, file::open_context_ptr context) { _callbacks[0]->select_line_feeds(path, content, context); }
			void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)  { if(_document->file_type() == NULL_STR) _callbacks[0]->select_file_type(path, content, context); else context->set_file_type(_document->file_type()); }
			void add_callback (document::open_callback_ptr callback)                                                { _callbacks.push_back(callback); }

			void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
			{
				// we are deleted in post_load() so make a copy of relevant data
				std::vector<document::open_callback_ptr> callbacks(_callbacks);
				document::document_ptr doc = _document;

				_document->post_load(path, content, attributes, fileType, encoding);
				iterate(cb, callbacks)
					(*cb)->show_document(path, doc);
			}

			void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
			{
				// we are deleted in post_load() so make a copy of relevant data
				std::vector<document::open_callback_ptr> callbacks(_callbacks);
				document::document_ptr doc = _document;

				_document->post_load(path, io::bytes_ptr(), std::map<std::string, std::string>(), NULL_STR, encoding::type());
				iterate(cb, callbacks)
					(*cb)->show_error(path, doc, message, filter);
			}

		private:
			document::document_ptr _document;
			std::vector<document::open_callback_ptr> _callbacks;
		};

		typedef std::shared_ptr<open_callback_wrapper_t> open_callback_wrapper_ptr;
		open_callback_wrapper_ptr _open_callback;

		void post_save (std::string const& path, io::bytes_ptr content, encoding::type const& encoding, bool succes);

	public:
		bool try_open (document::open_callback_ptr callback);
		void open ();
		void close ();

		void show ();
		void hide ();
		oak::date_t const& lru () const;

		void try_save (document::save_callback_ptr callback);
		bool save ();
		bool backup ();
		void detach_backup () { _backup_path = NULL_STR; }

		void set_path (std::string const& newPath);
		void set_virtual_path (std::string const& virtualPath)    { _virtual_path = virtualPath; }
		void set_custom_name (std::string const& newCustomName)   { _custom_name = newCustomName; }
		void set_file_type (std::string const& newFileType);

		std::string path () const             { return _path; }
		std::string virtual_path () const     { return _virtual_path == NULL_STR ? _path : _virtual_path; }
		std::string custom_name () const      { return _custom_name; }
		std::string backup_path () const;
		std::string display_name () const;

		void set_disk_encoding (encoding::type const& encoding) { _disk_newlines = encoding.newlines(); _disk_encoding = encoding.charset(); _disk_bom = encoding.byte_order_mark(); }
		encoding::type disk_encoding () const                   { return encoding::type(_disk_newlines, _disk_encoding, _disk_bom); }

		void set_indent (text::indent_t const& indent);
		text::indent_t const& indent () const;

		encoding::type encoding_for_save_as_path (std::string const& path);

		bool recent_tracking () const         { return _recent_tracking && _path != NULL_STR; }
		void set_recent_tracking (bool flag)  { _recent_tracking = flag; }

		bool sticky () const                  { return _sticky; }
		void set_sticky (bool flag)           { _sticky = flag; }

		ng::buffer_t& buffer ()               { ASSERT(_buffer); return *_buffer; }
		ng::buffer_t const& buffer () const   { ASSERT(_buffer); return *_buffer; }

		ng::undo_manager_t& undo_manager ()               { ASSERT(_undo_manager); return *_undo_manager; }
		ng::undo_manager_t const& undo_manager () const   { ASSERT(_undo_manager); return *_undo_manager; }

		std::string content () const;
		void set_content (std::string const& str);

		// =============
		// = Accessors =
		// =============

		oak::uuid_t identifier () const       { return _identifier; }
		ssize_t revision () const             { return _revision; }
		void set_revision (ssize_t rev)       { check_modified(_disk_revision, rev); }
		bool is_open () const                 { return _open_count != 0 && !_open_callback; }

		std::string file_type () const;

		std::map<std::string, std::string> document_variables () const;

		bool is_modified () const;
		bool is_on_disk () const                            { return is_open() ? _is_on_disk : path::exists(path());                }
		void set_disk_revision (ssize_t rev)                { check_modified(rev, _revision);                                       }
		std::string const& selection () const               { return _selection;                                                    }
		std::string const& folded () const                  { return _folded;                                                       }
		ng::index_t visible_index () const                  { return _visible_index;                                                }

		void set_selection (std::string const& sel)         { _selection = sel; _visible_index = ng::index_t();                     }
		void set_folded (std::string const& folded)         { _folded = folded;                                                     }
		void set_visible_index (ng::index_t index)          { _visible_index = index;                                               }

		void set_authorization (osx::authorization_t const& auth) { _authorization = auth; }

	private:
		void setup_buffer ();
		void set_modified (bool flag);

		// ==============
		// = Properties =
		// ==============

		friend document_ptr create (std::string const& path);
		friend document_ptr from_content (std::string const& content, std::string fileType);
		friend document_ptr find (oak::uuid_t const& uuid, bool searchBackups);

		oak::uuid_t _identifier;              // to identify this document when there is no path
		inode_t _inode;
		ssize_t _revision;
		ssize_t _disk_revision;
		bool _modified;

		std::string _path;                    // does not imply there actually is a file
		size_t _open_count;                   // document open in some window/tab
		mutable oak::date_t _lru;             // last time document was shown
		mutable bool _has_lru;
		bool _is_on_disk;
		bool _recent_tracking;
		bool _sticky = false;

		mutable std::string _backup_path;     // if there is a backup, this is set — we can have a backup even when there is no path
		mutable ssize_t _backup_revision;

		std::string _virtual_path;
		std::string _custom_name;
		mutable size_t _untitled_count;       // this is ≠ 0 if the document is untitled

		mutable std::string _file_type;       // this may also be in the settings
		// oak::uuid_t _grammar_uuid;

		std::shared_ptr<ng::buffer_t> _buffer;
		std::string _pristine_buffer = NULL_STR;
		std::shared_ptr<ng::undo_manager_t> _undo_manager;
		void mark_pristine ();

		// std::string _folder;                   // when there is no path, this value is where the document will likely end up, i.e, used for retrieving settings and default save location
		osx::authorization_t _authorization;   // when opened via sudo

		friend struct document_tracker_t;
		size_t untitled_count () const        { return _untitled_count; }

		std::string _disk_encoding;
		std::string _disk_newlines;
		bool _disk_bom;

		text::indent_t _indent;

	protected: // so that we can trigger the callback in unit tests
		watch_ptr _file_watcher;
		friend struct watch_t;
		void watch_callback (int flags, std::string const& newPath, bool async = true);
	};

	PUBLIC document_ptr create (std::string const& path = NULL_STR);
	PUBLIC document_ptr find (oak::uuid_t const& uuid, bool searchBackups = false);
	PUBLIC document_ptr from_content (std::string const& content, std::string fileType = NULL_STR);

	// ====================
	// = Document scanner =
	// ====================

	struct PUBLIC scanner_t
	{
		WATCH_LEAKS(scanner_t);

		scanner_t (std::string const& path, path::glob_list_t const& glob, bool follow_links = false, bool depth_first = false, bool includeUntitled = true);
		~scanner_t ();

		bool is_running () const { return is_running_flag; }
		void stop ()             { should_stop_flag = true; }
		void wait () const       { pthread_join(thread, NULL); }

		static std::vector<document_ptr> open_documents ();

		std::vector<document_ptr> accept_documents ();
		std::string get_current_path () const;

	private:
		std::string path;
		path::glob_list_t glob;
		bool follow_links, depth_first;

		pthread_t thread;
		mutable pthread_mutex_t mutex;
		volatile bool is_running_flag, should_stop_flag;

		void thread_main ();
		void scan_dir (std::string const& dir);

		std::string current_path;
		std::vector<document_ptr> documents;
		std::set< std::pair<dev_t, ino_t> > seen_paths;
	};

	typedef std::shared_ptr<scanner_t> scanner_ptr;
	
} /* document */

#endif /* end of include guard: DOCUMENT_H_MIJOONQT */
