#include "document.h"
#include "OakDocument Private.h"
#include "OakDocumentController.h"
#include <io/io.h>
#include <io/entries.h>
#include <io/resource.h>
#include <regexp/glob.h>
#include <text/ctype.h>
#include <cf/cf.h>
#include <cf/run_loop.h>
#include <ns/ns.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Document_Scanner);
OAK_DEBUG_VAR(Document_LRU);
OAK_DEBUG_VAR(Document_Tracker);
OAK_DEBUG_VAR(Document);

namespace document
{
	document_ptr create (std::string const& rawPath)
	{
		std::string const path = path::resolve(rawPath);
		if(path::is_text_clipping(path))
			return from_content(path::resource(path, typeUTF8Text, 256));
		return std::make_shared<document_t>([OakDocument documentWithPath:to_ns(path)]);
	}

	document_ptr find (oak::uuid_t const& uuid)
	{
		if(OakDocument* document = [OakDocument documentWithIdentifier:[[NSUUID alloc] initWithUUIDBytes:uuid.data]])
			return std::make_shared<document_t>(document);
		return document_ptr();
	}

	document_ptr from_content (std::string const& content, std::string fileType)
	{
		D(DBF_Document, bug("%s\n", fileType.c_str()););
		NSData* data = content != NULL_STR ? [NSData dataWithBytesNoCopy:(void*)content.data() length:content.size() freeWhenDone:NO] : nil;
		return std::make_shared<document_t>([OakDocument documentWithData:data fileType:to_ns(fileType) customName:nil]);
	}

} /* document */

namespace document
{
	// =========
	// = Marks =
	// =========

	void remove_marks (std::string const& typeToClear)
	{
		[OakDocument removeAllMarksOfType:to_ns(typeToClear)];
	}

	// ==============
	// = document_t =
	// ==============

	document_t::document_t (OakDocument* document) : _document(document)
	{
	}

	oak::uuid_t document_t::identifier () const        { return to_s(_document.identifier.UUIDString); }
	std::string document_t::path () const              { return to_s(_document.path); }
	std::string document_t::virtual_path () const      { return to_s(_document.virtualPath); }
	std::string document_t::custom_name () const       { return to_s(_document.customName); }
	std::string document_t::backup_path () const       { return to_s(_document.backupPath); }
	std::string document_t::display_name () const      { return to_s(_document.displayName); }
	encoding::type document_t::disk_encoding () const  { return encoding::type(to_s(_document.diskNewlines), to_s(_document.diskEncoding)); }
	std::string document_t::file_type () const         { return to_s(_document.fileType); }
	std::string document_t::content () const           { return to_s(_document.content); }
	bool document_t::is_loaded () const                { return _document.isLoaded; }
	bool document_t::is_modified () const              { return _document.isDocumentEdited; }
	bool document_t::is_on_disk () const               { return _document.isOnDisk; }
	text::indent_t document_t::indent () const         { return text::indent_t(_document.tabSize, SIZE_T_MAX, _document.softTabs); }
	bool document_t::recent_tracking () const          { return !_document.isRecentTrackingDisabled; }
	std::string document_t::selection () const         { return to_s(_document.selection); }
	std::string document_t::folded () const            { return to_s(_document.folded); }
	ng::index_t document_t::visible_index () const     { return _document.visibleIndex; }

	void document_t::set_path (std::string const& newPath)                   { _document.path = to_ns(newPath); }
	void document_t::set_authorization (osx::authorization_t const& auth)    { _document.authorization = auth; }
	void document_t::set_virtual_path (std::string const& virtualPath)       { _document.virtualPath = to_ns(virtualPath); }
	void document_t::set_custom_name (std::string const& newCustomName)      { _document.customName = to_ns(newCustomName); }
	void document_t::set_file_type (std::string const& newFileType)          { _document.fileType = to_ns(newFileType); }
	void document_t::set_content (std::string const& str)                    { _document.content = to_ns(str); }
	void document_t::set_disk_encoding (encoding::type const& encoding)      { _document.diskNewlines = to_ns(encoding.newlines()); _document.diskEncoding = to_ns(encoding.charset()); }
	void document_t::set_indent (text::indent_t const& indent)               { _document.tabSize = indent.tab_size(); _document.softTabs = indent.soft_tabs(); }
	void document_t::set_recent_tracking (bool flag)                         { _document.recentTrackingDisabled = !flag; }
	void document_t::set_selection (std::string const& sel)                  { _document.selection = to_ns(sel); set_visible_index(ng::index_t()); }
	void document_t::set_folded (std::string const& folded)                  { _document.folded = to_ns(folded); }
	void document_t::set_visible_index (ng::index_t index)                   { _document.visibleIndex = index; }

	ng::buffer_t& document_t::buffer ()                                      { return [_document buffer]; }
	ng::undo_manager_t& document_t::undo_manager ()                          { return [_document undoManager]; }

	void document_t::sync_load (CFStringRef runLoopMode)
	{
		__block bool didStop = false;

		auto runLoop = std::make_shared<cf::run_loop_t>(runLoopMode);
		[_document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
			didStop = true;
			runLoop->stop();
		}];

		if(!didStop)
			runLoop->start();
	}

	bool document_t::sync_save (CFStringRef runLoopMode)
	{
		__block bool res = false;
		__block bool didStop = false;

		auto runLoop = std::make_shared<cf::run_loop_t>(runLoopMode);
		[_document saveModalForWindow:nil completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
			res = result == OakDocumentIOResultSuccess;
			didStop = true;
			runLoop->stop();
		}];

		if(!didStop)
			runLoop->start();

		return res;
	}

	std::map<std::string, std::string> document_t::document_variables () const
	{
		return _document.variables;
	}

	void document_t::show ()              { [OakDocumentController.sharedInstance didTouchDocument:_document]; }
	void document_t::hide ()              { [OakDocumentController.sharedInstance didTouchDocument:_document]; }
	NSInteger document_t::lru () const    { return [OakDocumentController.sharedInstance lruRankForDocument:_document]; }

	bool document_t::backup ()            { return [_document saveBackup:nil]; }
	void document_t::detach_backup ()     { _document.backupPath = nil; }

	void document_t::enumerate_bytes_using_block (void(^block)(char const* bytes, size_t len, bool* stop))
	{
		[_document enumerateByteRangesUsingBlock:^(char const* bytes, NSRange range, BOOL* stop){
			bool shouldStop = false;
			block(bytes, range.length, &shouldStop);
			*stop = shouldStop;
		}];
	}

	void document_t::close ()
	{
		[_document close];
	}

	void document_t::add_mark (text::pos_t const& pos, std::string const& mark, std::string const& value)
	{
		[_document setMarkOfType:to_ns(mark) atPosition:pos content:to_ns(value)];
	}

	void document_t::remove_mark (text::pos_t const& pos, std::string const& mark)
	{
		[_document removeMarkOfType:to_ns(mark) atPosition:pos];
	}

	void document_t::remove_all_marks (std::string const& typeToClear)
	{
		[_document removeAllMarksOfType:to_ns(typeToClear)];
	}

	// ===========
	// = Replace =
	// ===========

	bool document_t::replace (std::multimap<std::pair<size_t, size_t>, std::string> const& replacements, uint32_t crc32)
	{
		return [_document performReplacements:replacements checksum:crc32];
	}

} /* document */
