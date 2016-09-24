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
#include <oak/debug.h>
#include <objc/objc.h>

#ifdef __OBJC__
@class OakDocument;
#else
typedef struct objc_object OakDocument;
#include <objc/NSObjCRuntime.h> // NSInteger
#endif

namespace document
{
	struct document_t;
	typedef std::shared_ptr<document_t>       document_ptr;

	struct PUBLIC document_t : std::enable_shared_from_this<document_t>
	{
		WATCH_LEAKS(document_t);

		document_t (OakDocument* document);

		bool operator== (document_t const& rhs) const { return this == &rhs || identifier() == rhs.identifier(); }
		bool operator!= (document_t const& rhs) const { return this != &rhs && identifier() != rhs.identifier(); }

		OakDocument* document ()      { return _document; }

		void sync_load (CFStringRef runLoopMode = kCFRunLoopDefaultMode);
		bool sync_save (CFStringRef runLoopMode = kCFRunLoopDefaultMode);

		void show ();
		void hide ();
		NSInteger lru () const;

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
		std::string content () const;
		bool is_loaded () const;
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
		void set_content (std::string const& str);
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

		void close ();

		// ===================

		void add_mark (text::pos_t const& pos, std::string const& mark, std::string const& value = std::string());
		void remove_mark (text::pos_t const& pos, std::string const& mark);
		void remove_all_marks (std::string const& typeToClear = NULL_STR);

	private:
		OakDocument* _document;
	};

	PUBLIC document_ptr create (std::string const& path = NULL_STR);
	PUBLIC document_ptr find (oak::uuid_t const& uuid);
	PUBLIC document_ptr from_content (std::string const& content, std::string fileType = NULL_STR);

	PUBLIC void remove_marks (std::string const& typeToClear = NULL_STR);

} /* document */

#endif /* end of include guard: DOCUMENT_H_MIJOONQT */
