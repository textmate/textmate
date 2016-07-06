#ifndef DOCUMENT_H_MIJOONQT
#define DOCUMENT_H_MIJOONQT

#include <buffer/buffer.h>
#include <undo/undo.h>
#include <plist/uuid.h>
#include <plist/date.h>
#include <text/types.h>
#include <authorization/authorization.h>
#include <file/bytes.h>
#include <file/open.h>
#include <file/save.h>
#include <file/encoding.h>
#include <scope/scope.h>
#include <regexp/glob.h>
#include <oak/debug.h>
#include <objc/objc.h>

#ifdef __OBJC__
@class OakDocument;
@class OakDocumentObserver;
#else
typedef struct objc_object OakDocument;
typedef struct objc_object OakDocumentObserver;
#endif

namespace document
{
	PUBLIC extern std::string const kBookmarkIdentifier;

	struct document_t;
	typedef std::shared_ptr<document_t>       document_ptr;
	typedef std::weak_ptr<document_t>         document_weak_ptr;

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

		document_t (OakDocument* document);
		~document_t ();

		bool operator== (document_t const& rhs) const { return this == &rhs || identifier() == rhs.identifier(); }
		bool operator!= (document_t const& rhs) const { return this != &rhs && identifier() != rhs.identifier(); }

		OakDocument* document ()      { observer(); return _document; }

		// Only in document_t
		bool sticky () const          { return _sticky; }
		void set_sticky (bool flag)   { _sticky = flag; }

		void sync_open (CFStringRef runLoopMode = kCFRunLoopDefaultMode);
		bool sync_save (CFStringRef runLoopMode = kCFRunLoopDefaultMode);

		void show ();
		void hide ();
		oak::date_t lru () const;

		// ===================
		// = Simple Wrappers =
		// ===================

		oak::uuid_t identifier () const;
		std::string path () const;
		std::string virtual_path () const;
		std::string logical_path () const { return virtual_path() == NULL_STR ? path() : virtual_path(); }
		std::string custom_name () const;
		std::string backup_path () const;
		std::string display_name () const;
		encoding::type disk_encoding () const;
		std::string file_type () const;
		ssize_t revision () const;
		std::string content () const;
		bool is_open () const;
		bool is_modified () const;
		bool is_on_disk () const;
		text::indent_t indent () const;
		bool recent_tracking () const;
		std::string selection () const;
		std::string folded () const;
		ng::index_t visible_index () const;

		void set_path (std::string const& newPath);
		void set_authorization (osx::authorization_t const& auth);
		void set_virtual_path (std::string const& virtualPath);
		void set_custom_name (std::string const& newCustomName);
		void set_file_type (std::string const& newFileType);
		void set_revision (ssize_t rev);
		void set_content (std::string const& str);
		void set_disk_revision (ssize_t rev);
		void set_disk_encoding (encoding::type const& encoding);
		void set_indent (text::indent_t const& indent);
		void set_recent_tracking (bool flag);
		void set_selection (std::string const& sel);
		void set_folded (std::string const& folded);
		void set_visible_index (ng::index_t index);

		ng::buffer_t& buffer ();
		ng::undo_manager_t& undo_manager ();

		// ===========
		// = Methods =
		// ===========

		std::map<std::string, std::string> document_variables () const;

		bool backup ();
		void detach_backup ();

		void enumerate_bytes_using_block (void(^block)(char const* bytes, size_t len, bool* stop));
		bool replace (std::multimap<std::pair<size_t, size_t>, std::string> const& replacements, uint32_t crc32);

		void try_save (document::save_callback_ptr callback);
		void close ();

		// ===================

		void add_mark (text::pos_t const& pos, std::string const& mark, std::string const& value = std::string());
		void remove_mark (text::pos_t const& pos, std::string const& mark);
		void remove_all_marks (std::string const& typeToClear = NULL_STR);

		// ===================
		// = Callback system =
		// ===================

		struct callback_t
		{
			enum event_t
			{
				did_save,

				did_change_open_status,
				did_change_modified_status,
				did_change_on_disk_status,
				did_change_path,
				did_change_file_type,
				did_change_indent_settings,
				did_change_marks,
				did_change_content,
			};

			virtual ~callback_t () { }
			virtual void handle_document_event (document_ptr document, event_t event) = 0;
			virtual void document_will_delete (document_t* document) { }
		};

		void add_callback (callback_t* callback);
		void remove_callback (callback_t* callback);

		// ==========
		// = Legacy =
		// ==========

		inode_t _inode;

	private:
		OakDocument* _document;
		OakDocumentObserver* _observer;
		bool _sticky = false;

		OakDocumentObserver* observer ();
	};

	PUBLIC document_ptr create (std::string const& path = NULL_STR);
	PUBLIC document_ptr find (oak::uuid_t const& uuid);
	PUBLIC document_ptr from_content (std::string const& content, std::string fileType = NULL_STR);

	PUBLIC void remove_marks (std::string const& typeToClear = NULL_STR);

	// ====================
	// = Document scanner =
	// ====================

	struct PUBLIC scanner_t
	{
		WATCH_LEAKS(scanner_t);

		scanner_t (std::string const& path, path::glob_list_t const& glob);
		~scanner_t ();

		void set_follow_directory_links (bool flag) { follow_directory_links = flag; }
		void set_follow_file_links (bool flag)      { follow_file_links = flag; }
		void set_include_untitled (bool flag)       { include_untitled = flag; }
		void set_depth_first (bool flag)            { depth_first = flag; }

		void start ();
		void stop ()             { should_stop_flag = true; }
		bool is_running () const { return is_running_flag; }
		void wait () const       { pthread_join(thread, NULL); }

		static std::vector<document_ptr> open_documents ();

		std::vector<document_ptr> accept_documents ();
		std::string get_current_path () const;

	private:
		std::string path;
		path::glob_list_t glob;
		bool follow_directory_links = false;
		bool follow_file_links      = true;
		bool include_untitled       = false;
		bool depth_first            = false;

		pthread_t thread;
		mutable pthread_mutex_t mutex;
		volatile bool is_running_flag  = false;
		volatile bool should_stop_flag = false;

		void thread_main ();
		void scan_dir (std::string const& dir);

		std::string current_path;
		std::vector<document_ptr> documents;
		std::set< std::pair<dev_t, ino_t> > seen_paths;
	};

	typedef std::shared_ptr<scanner_t> scanner_ptr;

} /* document */

#endif /* end of include guard: DOCUMENT_H_MIJOONQT */
