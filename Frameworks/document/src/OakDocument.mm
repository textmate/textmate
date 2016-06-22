#import "OakDocument Private.h"
#import "watch.h"
#import "merge.h"
#import "document.h" // {open,save}_callback_ptr + kBookmarkIdentifier
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <cf/run_loop.h>
#import <ns/ns.h>
#import <settings/settings.h>
#import <settings/volume.h>
#import <buffer/buffer.h>
#import <undo/undo.h>
#import <selection/types.h>
#import <text/newlines.h>
#import <text/utf8.h>
#import <io/entries.h>
#import <file/type.h>
#import <file/open.h>
#import <file/save.h>
#import <file/reader.h>
#import <encoding/encoding.h>

namespace document
{
	// =========
	// = Marks =
	// =========

	static struct mark_tracker_t
	{
		void move_to_buffer (std::string const& path, ng::buffer_t& buf)
		{
			for(auto const& type : marks_for(path))
			{
				for(auto const& pos : type.second)
					buf.set_mark(cap(buf, pos.first).index, type.first, pos.second);
			}
			_paths.erase(path);
		}

		void copy_from_buffer (std::string const& path, ng::buffer_t const& buf)
		{
			std::map<std::string, std::map<text::pos_t, std::string>> marks;
			for(auto const& pair : buf.get_marks(0, buf.size()))
				marks[pair.second.first].emplace(buf.convert(pair.first), pair.second.second);
			_paths[path] = marks;
		}

		void add (std::string const& path, text::pos_t const& pos, std::string const& mark, std::string const& value)
		{
			marks_for(path)[mark].emplace(pos, value);
		}

		std::map<text::pos_t, std::string> const& get (std::string const& path, std::string const& mark)
		{
			return marks_for(path)[mark];
		}

		void remove (std::string const& path, text::pos_t const& pos, std::string const& mark)
		{
			marks_for(path)[mark].erase(pos);
		}

		void remove_all (std::string const& path, std::string const& mark)
		{
			auto marks = _paths.find(path);
			if(marks != _paths.end())
				remove_all(marks->second, mark);
		}

		void remove_all (std::string const& mark)
		{
			for(auto& marks : _paths)
				remove_all(marks.second, mark);
		}

	private:
		void remove_all (std::map<std::string, std::map<text::pos_t, std::string>>& marks, std::string const& mark)
		{
			if(mark == NULL_STR)
				marks.clear();
			else if(!mark.empty() && mark.back() == '/')
				oak::erase_descendent_keys(marks, mark);
			else
				marks.erase(mark);
		}

		std::map<std::string, std::map<text::pos_t, std::string>>& marks_for (std::string const& path)
		{
			auto marks = _paths.find(path);
			if(marks == _paths.end())
				marks = _paths.emplace(path, std::map<std::string, std::map<text::pos_t, std::string>>{ { kBookmarkIdentifier, load_bookmarks(path) } }).first;
			return marks->second;
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

		static std::map<text::pos_t, std::string> load_bookmarks (std::string const& path)
		{
			std::map<text::pos_t, std::string> res;

			std::string const str = path::get_attr(path, "com.macromates.bookmarks");
			if(str == NULL_STR)
				return res;

			plist::any_t const& plist = plist::parse(str);
			if(plist::array_t const* array = boost::get<plist::array_t>(&plist))
			{
				for(auto const& bm : *array)
				{
					if(std::string const* str = boost::get<std::string>(&bm))
						res.emplace(*str, std::string());
				}
			}

			return res;
		}

		// path → mark type → position → mark
		std::map<std::string, std::map<std::string, std::map<text::pos_t, std::string>>> _paths;

	} marks;

} /* document */

// ==============================
// = OakDocument Implementation =
// ==============================

NSString* OakDocumentContentDidChangeNotification = @"OakDocumentContentDidChangeNotification";
NSString* OakDocumentMarksDidChangeNotification   = @"OakDocumentMarksDidChangeNotification";
NSString* OakDocumentDidSaveNotification          = @"OakDocumentDidSaveNotification";

static NSString* FileExtensionForGrammar (parse::grammar_ptr grammar)
{
	if(!grammar)
		return nil;

	bundles::item_ptr grammarItem = bundles::lookup(grammar->uuid());
	if(!grammarItem)
		return nil;

	plist::array_t fileTypes;
	if(!plist::get_key_path(grammarItem->plist(), "fileTypes", fileTypes))
		return nil;

	for(auto const& type : fileTypes)
	{
		if(std::string const* ext = boost::get<std::string>(&type))
			return to_ns(*ext);
	}

	return nil;
}

@interface OakDocument ()
{
	OBJC_WATCH_LEAKS(OakDocument);

	std::unique_ptr<ng::buffer_t> _buffer;
	std::unique_ptr<ng::detail::storage_t> _snapshot;
	ng::callback_t* _callback;

	std::unique_ptr<ng::undo_manager_t> _undoManager;
	std::unique_ptr<document::watch_base_t> _fileSystemObserver;

	NSTimer* _backupTimer;
}
@property (nonatomic) NSUInteger untitledCount;
@property (nonatomic) NSUInteger openCount;
@property (nonatomic, getter = isBufferEmpty) BOOL bufferEmpty;
@property (nonatomic) NSInteger backupRevision;
@property (nonatomic) BOOL observeFileSystem;
@property (nonatomic) BOOL needsImportDocumentChanges;

// These are also exposed in ‘OakDocument Private.h’
@property (nonatomic) NSInteger   revision;
@property (nonatomic) NSInteger   savedRevision;
@property (nonatomic) NSString*   backupPath;
@property (nonatomic) NSString*   folded;
@property (nonatomic) NSString*   selection;
@property (nonatomic) ng::index_t visibleIndex;
@end

static struct document_container_t
{
	void add (OakDocument* document)
	{
		std::lock_guard<std::mutex> lock(_lock);
		[_documents addObject:document];
	}

	void set_untitled_count (OakDocument* document)
	{
		std::lock_guard<std::mutex> lock(_lock);

		std::set<NSUInteger> reserved;
		for(OakDocument* document in _documents)
		{
			if(!document.path && !document.customName)
				reserved.insert(document.untitledCount);
		}

		NSUInteger available = 1;
		while(reserved.find(available) != reserved.end())
			++available;
		document.untitledCount = available;
	}

	void remove_all_marks (NSString* mark)
	{
		std::lock_guard<std::mutex> lock(_lock);
		for(OakDocument* document : _documents)
			[document removeAllMarksOfType:mark];
	}

private:
	std::mutex _lock;
	NSHashTable* _documents = [NSHashTable weakObjectsHashTable];

} DocumentContainer;

@implementation OakDocument
+ (NSSet*)keyPathsForValuesAffectingDisplayName
{
	return [NSSet setWithObjects:@"path", @"customName", @"untitledCount", nil];
}

+ (NSSet*)keyPathsForValuesAffectingOpen
{
	return [NSSet setWithObjects:@"openCount", nil];
}

+ (NSSet*)keyPathsForValuesAffectingDocumentEdited
{
	return [NSSet setWithObjects:@"onDisk", @"revision", @"savedRevision", @"bufferEmpty", nil];
}

// ==================
// = Helper Methods =
// ==================

- (void)setBufferGrammarForCurrentFileType
{
	if(!_fileType || !_buffer)
		return;

	for(auto const& item : bundles::query(bundles::kFieldGrammarScope, to_s(_fileType), scope::wildcard, bundles::kItemTypeGrammar))
	{
		_buffer->set_grammar(item);
		break;
	}
}

- (void)snapshot
{
	ASSERT(_buffer);
	_snapshot = std::make_unique<ng::detail::storage_t>(_buffer->storage());
}

- (void)updateSpellingSettings:(BOOL)updateSpelling andIndentSettings:(BOOL)updateIndent
{
	if(!_buffer)
		return;

	std::map<std::string, std::string> variables = {
		{ "TM_DISPLAYNAME",   to_s(self.displayName)           },
		{ "TM_DOCUMENT_UUID", to_s(self.identifier.UUIDString) },
	};

	if(_path)
	{
		variables["TM_FILEPATH"]  = to_s(_path);
		variables["TM_FILENAME"]  = path::name(to_s(_path));
		variables["TM_DIRECTORY"] = path::parent(to_s(_path));
	}

	settings_t const settings = settings_for_path(to_s(_virtualPath), to_s(_fileType), path::parent(to_s(_path)), variables);
	if(updateSpelling)
	{
		self.spellingLanguage = to_ns(settings.get(kSettingsSpellingLanguageKey, ""));
		self.continuousSpellCheckingEnabled = settings.get(kSettingsSpellCheckingKey, false);
	}

	if(updateIndent)
	{
		self.tabSize = std::max(1, settings.get(kSettingsTabSizeKey, 4));
		self.softTabs = settings.get(kSettingsSoftTabsKey, false);
	}
}

// ==================

- (instancetype)init
{
	if(self = [super init])
	{
		_identifier = [NSUUID UUID];
		DocumentContainer.add(self);
	}
	return self;
}

- (instancetype)initWithPath:(NSString*)aPath
{
	if(self = [self init])
	{
		_path = aPath;
	}
	return self;
}

- (instancetype)initWithData:(NSData*)someData fileType:(NSString*)aFileType customName:(NSString*)aName
{
	if(self = [self init])
	{
		self.customName = aName;
		self.fileType   = aFileType;

		[self createBuffer];
		[someData enumerateByteRangesUsingBlock:^(void const* buf, NSRange range, BOOL*){
			_buffer->insert(range.location, (char const*)buf, range.length);
		}];
	}
	return self;
}

- (instancetype)initWithIdentifier:(NSUUID*)anIdentifier
{
	std::string const dir = to_s([[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) firstObject] stringByAppendingPathComponent:@"TextMate/Session"]);
	for(auto dirEntry : path::entries(dir))
	{
		std::string const path = path::join(dir, dirEntry->d_name);
		std::string const uuid = path::get_attr(path, "com.macromates.backup.identifier");
		if(uuid == NULL_STR || ![anIdentifier isEqual:[[NSUUID alloc] initWithUUIDString:to_ns(uuid)]])
			continue;

		if(self = [self init])
		{
			_identifier     = anIdentifier;
			_backupPath     = to_ns(path);

			_path           = to_ns(path::resolve(path::get_attr(path, "com.macromates.backup.path")));
			_fileType       = to_ns(path::get_attr(path, "com.macromates.backup.file-type"));
			_diskEncoding   = to_ns(path::get_attr(path, "com.macromates.backup.encoding"));
			_diskNewlines   = to_ns(path::get_attr(path, "com.macromates.backup.newlines"));
			_customName     = to_ns(path::get_attr(path, "com.macromates.backup.custom-name"));
			_untitledCount  = atoi(path::get_attr(path, "com.macromates.backup.untitled-count").c_str());

			if(path::get_attr(path, "com.macromates.backup.modified") == "YES")
				_savedRevision = _revision-1;

			break;
		}
	}
	return self;
}

+ (instancetype)documentWithPath:(NSString*)aPath
{
	return [[OakDocument alloc] initWithPath:aPath];
}

+ (instancetype)documentWithData:(NSData*)someData fileType:(NSString*)aFileType customName:(NSString*)aName
{
	return [[OakDocument alloc] initWithData:someData fileType:aFileType customName:aName];
}

+ (instancetype)documentWithIdentifier:(NSUUID*)anIdentifier
{
	return [[OakDocument alloc] initWithIdentifier:anIdentifier];
}

- (void)dealloc
{
	[self removeBackup];
}

- (NSString*)sniffFileType
{
	io::bytes_ptr firstLine = _buffer ? std::make_shared<io::bytes_t>(_buffer->substr(_buffer->begin(0), std::min<size_t>(_buffer->eol(0), 2048))) : io::bytes_ptr();
	return to_ns(file::type(to_s(_path), firstLine, to_s(_virtualPath)));
}

- (NSString*)displayName
{
	if(_customName)
		return _customName;
	if(_path)
		return [[NSFileManager defaultManager] displayNameAtPath:_path];

	if(self.untitledCount == 0)
		DocumentContainer.set_untitled_count(self);

	return _untitledCount == 1 ? @"untitled" : [NSString stringWithFormat:@"untitled %lu", _untitledCount];
}

- (void)setPath:(NSString*)newPath
{
	NSString* path = to_ns(path::resolve(to_s(newPath)));
	if([_path isEqualToString:path])
		return;

	_path = path;
	self.customName = nil;

	if(_observeFileSystem)
	{
		self.observeFileSystem = NO;
		self.observeFileSystem = YES;
	}

	if(self.isOpen)
	{
		self.onDisk = access([_path fileSystemRepresentation], F_OK) == 0;
		if(NSString* fileType = [self sniffFileType])
			self.fileType = fileType;
	}
}

- (void)setFileType:(NSString*)newType
{
	if(_fileType == newType || [_fileType isEqual:newType])
		return;

	_fileType = newType;
	[self setBufferGrammarForCurrentFileType];
	[self updateSpellingSettings:YES andIndentSettings:YES];
}

- (std::map<std::string, std::string>)extendedAttributeds
{
	std::map<std::string, std::string> res = {
		{ "com.macromates.bookmarks",      to_s([self stringifyMarksOfType:to_ns(document::kBookmarkIdentifier)]) },
		{ "com.macromates.selectionRange", to_s(_selection) },
		{ "com.macromates.visibleIndex",   _visibleIndex ? to_s(_visibleIndex) : NULL_STR },
		{ "com.macromates.crc32",          NULL_STR },
		{ "com.macromates.folded",         NULL_STR },
		{ "com.macromates.visibleRect",    NULL_STR }, // Clear legacy attribute
	};

	if(_buffer && OakNotEmptyString(_folded))
	{
		boost::crc_32_type crc32;
		_buffer->visit_data([&crc32](char const* bytes, size_t offset, size_t len, bool*){
			crc32.process_bytes(bytes, len);
		});

		res["com.macromates.crc32"]  = text::format("%04x", crc32.checksum());
		res["com.macromates.folded"] = to_s(_folded);
	}

	return res;
}

// ==========
// = Backup =
// ==========

- (void)scheduleBackup
{
	[_backupTimer invalidate];
	_backupTimer = [NSTimer scheduledTimerWithTimeInterval:2 target:self selector:@selector(backupTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)backupTimerDidFire:(NSTimer*)aTimer
{
	_backupTimer = nil;
	[self saveBackup:self];
}

- (void)removeBackup
{
	if(_backupPath)
	{
		[[NSFileManager defaultManager] removeItemAtPath:_backupPath error:nullptr];
		self.backupPath = nil;
	}
}

- (NSString*)createAndReturnBackupPath
{
	NSString* path = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) firstObject] stringByAppendingPathComponent:@"TextMate/Session"];
	if(![[NSFileManager defaultManager] createDirectoryAtPath:path withIntermediateDirectories:YES attributes:nil error:nullptr])
		return nil;

	path = [path stringByAppendingPathComponent:[self.displayName stringByReplacingOccurrencesOfString:@"/" withString:@":"]];

	if(_buffer && (!_path || _customName && OakIsEmptyString([_customName pathExtension])))
	{
		if(NSString* ext = FileExtensionForGrammar(_buffer->grammar()))
			path = [path stringByAppendingPathExtension:ext];
	}

	return to_ns(path::unique(to_s(path)));
}

- (BOOL)saveBackup:(id)sender
{
	if(_buffer && (_onDisk ? _savedRevision != _revision : !_bufferEmpty))
	{
		if(_backupPath && _backupRevision == _revision)
			return NO;

		if(!_backupPath)
			self.backupPath = [self createAndReturnBackupPath];

		int fd = open([_backupPath fileSystemRepresentation], O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		if(fd == -1)
			return NO;

		_buffer->visit_data([fd](char const* bytes, size_t offset, size_t len, bool*){
			write(fd, bytes, len);
		});

		close(fd);

		auto attr = [self extendedAttributeds];

		attr["com.macromates.backup.path"]           = to_s(_path);
		attr["com.macromates.backup.identifier"]     = to_s(_identifier.UUIDString);
		attr["com.macromates.backup.file-type"]      = to_s(_fileType);
		attr["com.macromates.backup.encoding"]       = to_s(_diskEncoding);
		attr["com.macromates.backup.newlines"]       = to_s(_diskNewlines);
		attr["com.macromates.backup.untitled-count"] = _path || _customName ? NULL_STR : std::to_string(_untitledCount);
		attr["com.macromates.backup.custom-name"]    = to_s(_customName);
		attr["com.macromates.backup.modified"]       = self.isDocumentEdited ? "YES" : NULL_STR;
		attr["com.macromates.backup.tab-size"]       = std::to_string(self.tabSize);
		attr["com.macromates.backup.soft-tabs"]      = self.softTabs ? "YES" : NULL_STR;

		path::set_attributes(to_s(_backupPath), attr);

		self.backupRevision = _revision;

		return YES;
	}
	else if(_backupPath)
	{
		[self removeBackup];
	}

	return NO;
}

// =================
// = Load Document =
// =================

- (void)didLoadContent:(io::bytes_ptr)content attributes:(std::map<std::string, std::string> const&)attributes fileType:(NSString*)fileType encoding:(encoding::type)encoding
{
	if(!content) // Loading failed
	{
		self.openCount = 0;
		return;
	}

	[self createBuffer];
	_buffer->insert(0, content->get(), content->size());

	if(_path)
		document::marks.move_to_buffer(to_s(_path), *_buffer);

	auto folded = attributes.find("com.macromates.folded");
	if(folded != attributes.end())
	{
		auto crc32 = attributes.find("com.macromates.crc32");
		if(crc32 != attributes.end() && crc32->second == text::format("%04x", content->crc32()))
			_folded = to_ns(folded->second);
	}

	if(!_selection)
	{
		auto sel = attributes.find("com.macromates.selectionRange");
		_selection = sel != attributes.end() ? to_ns(sel->second) : nil;

		auto idx = attributes.find("com.macromates.visibleIndex");
		if(idx != attributes.end())
		{
			size_t index = SIZE_T_MAX, carry = 0;
			sscanf(idx->second.c_str(), "%zu:%zu", &index, &carry);
			_visibleIndex = ng::index_t(_buffer->sanitize_index(index), carry);
		}
	}

	if(_fileType)
	{
		[self setBufferGrammarForCurrentFileType];
		[self updateSpellingSettings:YES andIndentSettings:YES];
	}
	else
	{
		self.fileType = fileType;
	}

	_undoManager = std::make_unique<ng::undo_manager_t>(*_buffer);
	_buffer->set_async_parsing(true);
	_buffer->bump_revision();

	self.onDisk         = _path && access([_path fileSystemRepresentation], F_OK) == 0;
	self.savedRevision  = _buffer->revision();
	self.backupRevision = _buffer->revision(); // This is ignored when backupPath is nil
	self.revision       = _buffer->revision();
	self.diskEncoding   = to_ns(encoding.charset());
	self.diskNewlines   = to_ns(encoding.newlines());

	[self snapshot];
	self.observeFileSystem = YES;
}

- (BOOL)tryOpenUsingCallback:(document::open_callback_ptr)callback forDocument:(document::document_ptr)document
{
	if(++self.openCount == 1)
	{
		if(_backupPath)
		{
			ASSERT(!_buffer);

			BOOL isModified = self.isDocumentEdited;
			std::string const path = to_s(_backupPath);
			[self didLoadContent:std::make_shared<io::bytes_t>(path::content(path)) attributes:path::attributes(path) fileType:_fileType encoding:encoding::type(to_s(_diskNewlines), to_s(_diskEncoding))];

			if(isModified)
			{
				_snapshot.reset();
				self.savedRevision = _revision-1;
			}

			return YES;
		}

		struct callback_t : file::open_callback_t
		{
			callback_t (OakDocument* document, document::document_ptr cppDocument, std::string const& fileType, document::open_callback_ptr callback) : _document(document), _cppDocument(cppDocument), _file_type(fileType), _callbacks(1, callback) { }

			void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)    { _callbacks.front()->select_charset(path, content, context); }
			void select_line_feeds (std::string const& path, io::bytes_ptr content, file::open_context_ptr context) { _callbacks.front()->select_line_feeds(path, content, context); }
			void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)  { if(_file_type != NULL_STR) context->set_file_type(_file_type); else _callbacks.front()->select_file_type(path, content, context); }
			void add_callback (document::open_callback_ptr callback)                                                { _callbacks.push_back(callback); }

			void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
			{
				[_document didLoadContent:content attributes:attributes fileType:to_ns(fileType) encoding:encoding];
				for(auto const& cb : _callbacks)
					cb->show_document(path, _cppDocument);
			}

			void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
			{
				[_document didLoadContent:io::bytes_ptr() attributes:{ } fileType:nil encoding:{ }];
				for(auto const& cb : _callbacks)
					cb->show_error(path, _cppDocument, message, filter);
			}

		private:
			OakDocument* _document;
			document::document_ptr _cppDocument;
			std::string _file_type;
			std::vector<document::open_callback_ptr> _callbacks;
		};

		io::bytes_ptr content;
		if(_buffer)
		{
			content = std::make_shared<io::bytes_t>(_buffer->size());
			_buffer->visit_data([&](char const* bytes, size_t offset, size_t len, bool*){
				memcpy(content->get() + offset, bytes, len);
			});
			[self deleteBuffer];
		}

		auto cb = std::make_shared<callback_t>(self, document, to_s(_fileType), callback);
		file::open(to_s(_path), _authorization, cb, content, to_s(_virtualPath));
		return NO;
	}
	else
	{
		// TODO If we haev a callback_t instance then cb->add_callback(callback);
	}

	return YES;
}

// =================
// = Save Document =
// =================

- (void)didSaveContent:(io::bytes_ptr)content atPath:(NSString*)aPath withEncoding:(encoding::type)encoding success:(BOOL)success
{
	if(success)
	{
		self.onDisk        = YES;
		self.diskEncoding  = to_ns(encoding.charset());
		self.diskNewlines  = to_ns(encoding.newlines());
		self.savedRevision = _revision;

		[self snapshot];
		[self removeBackup];
		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentDidSaveNotification object:self];
	}
	self.observeFileSystem = self.isOpen;
}

- (void)trySaveUsingCallback:(document::save_callback_ptr)callback forDocument:(document::document_ptr)document
{
	if(_buffer && _path)
	{
		self.observeFileSystem = NO;

		io::bytes_ptr content = std::make_shared<io::bytes_t>(_buffer->size());
		_buffer->visit_data([&](char const* bytes, size_t offset, size_t len, bool*){
			memcpy(content->get() + offset, bytes, len);
		});

		std::map<std::string, std::string> attributes;
		if(volume::settings(to_s(_path)).extended_attributes())
			attributes = [self extendedAttributeds];

		encoding::type encoding = encoding::type(to_s(_diskNewlines), to_s(_diskEncoding));

		settings_t const settings = settings_for_path(to_s(_path));
		if(encoding.charset() == kCharsetNoEncoding)
			encoding.set_charset(settings.get(kSettingsEncodingKey, kCharsetUTF8));
		if(encoding.newlines() == NULL_STR)
			encoding.set_newlines(settings.get(kSettingsLineEndingsKey, kLF));

		struct callback_t : file::save_callback_t
		{
			callback_t (OakDocument* document, document::document_ptr cppdocument, document::save_callback_ptr callback, bool closeDocument) : _document(document), _cppDocument(cppdocument), _callback(callback), _close_document(closeDocument)
			{
			}

			void select_path (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)                                     { _callback->select_path(path, content, context); }
			void select_make_writable (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)                            { _callback->select_make_writable(path, content, context); }
			void select_create_parent (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)                            { _callback->select_create_parent(path, content, context); }
			void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, file::save_context_ptr context) { _callback->obtain_authorization(path, content, auth, context); }
			void select_charset (std::string const& path, io::bytes_ptr content, std::string const& charset, file::save_context_ptr context)      { _callback->select_charset(path, content, charset, context); }

			void did_save (std::string const& path, io::bytes_ptr content, encoding::type const& encoding, bool success, std::string const& message, oak::uuid_t const& filter)
			{
				if(_close_document)
					[_document didSaveContent:content atPath:to_ns(path) withEncoding:encoding success:success];
				_callback->did_save_document(_cppDocument, path, success, message, filter);
				if(_close_document)
					[_document close];
			}

		private:
			OakDocument* _document;
			document::document_ptr _cppDocument;
			document::save_callback_ptr _callback;
			bool _close_document;
		};

		BOOL closeDocument = self.isOpen;
		if(closeDocument)
			++self.openCount;

		auto cb = std::make_shared<callback_t>(self, document, callback, closeDocument);
		file::save(to_s(_path), cb, _authorization, content, attributes, to_s(_fileType), encoding, std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
	}
}

- (void)close
{
	if(--self.openCount != 0)
		return;

	if(_path && _buffer)
	{
		document::marks.copy_from_buffer(to_s(_path), *_buffer);
		if(!self.isDocumentEdited && volume::settings(to_s(_path)).extended_attributes())
			path::set_attributes(to_s(_path), [self extendedAttributeds]);
	}

	self.observeFileSystem = NO;
	self.revision = _savedRevision; // Clears isDocumentEdited
	[self removeBackup];

	_undoManager.reset();
	[self deleteBuffer];
}

// ==========================
// = Properties and Content =
// ==========================

- (void)createBuffer
{
	if(_buffer)
		return;

	struct callback_t : ng::callback_t
	{
		callback_t (OakDocument* self) : _self(self) { }

		void did_replace (size_t from, size_t to, char const* buf, size_t len)
		{
			_size += len - (to - from);
			_self.bufferEmpty = _size == 0;
			[_self scheduleBackup];
		}

	private:
		size_t _size = 0;
		OakDocument* _self;
	};

	_callback = new callback_t(self);

	_buffer = std::make_unique<ng::buffer_t>();
	_buffer->add_callback(_callback);
}

- (void)deleteBuffer
{
	if(!_buffer)
		return;

	_buffer->remove_callback(_callback);
	delete _callback;
	_buffer.reset();
	_snapshot.reset();
}

- (BOOL)isOpen                                        { return _openCount != 0; }
- (BOOL)isDocumentEdited                              { return _revision != _savedRevision && (_onDisk || !_bufferEmpty); }

- (BOOL)canUndo                                       { return _undoManager && _undoManager->can_undo(); }
- (BOOL)canRedo                                       { return _undoManager && _undoManager->can_redo(); }

// We currently store these in buffer_t
- (BOOL)isContinuousSpellCheckingEnabled              { return _buffer && _buffer->live_spelling(); }
- (NSString*)spellingLanguage                         { return _buffer ? to_ns(_buffer->spelling_language()) : nil; }
- (NSUInteger)tabSize                                 { return _buffer ? _buffer->indent().tab_size() : 4; }
- (BOOL)softTabs                                      { return _buffer && _buffer->indent().soft_tabs(); }

- (void)setContinuousSpellCheckingEnabled:(BOOL)value { if(_buffer) _buffer->set_live_spelling(value); }
- (void)setSpellingLanguage:(NSString*)value          { if(_buffer) _buffer->set_spelling_language(to_s(value)); }
- (void)setTabSize:(NSUInteger)value                  { if(_buffer) _buffer->indent().set_tab_size(value); }
- (void)setSoftTabs:(BOOL)value                       { if(_buffer) _buffer->indent().set_soft_tabs(value); }

- (ng::buffer_t&)buffer                               { return *_buffer; }
- (ng::undo_manager_t&)undoManager                    { return *_undoManager; }

- (void)enumerateByteRangesUsingBlock:(void(^)(char const* bytes, NSRange byteRange, BOOL* stop))block
{
	BOOL stop = NO;
	if(_buffer)
	{
		_buffer->visit_data([&](char const* bytes, size_t offset, size_t len, bool* tmp){
			block(bytes, NSMakeRange(offset, len), &stop);
			*tmp = stop;
		});
	}
	else if(_path)
	{
		file::reader_t reader(to_s(_path));
		io::bytes_ptr bytes;
		size_t offset = 0;
		while(!stop && (bytes = reader.next()))
		{
			block(bytes->get(), NSMakeRange(offset, bytes->size()), &stop);
			offset += bytes->size();
		}
	}
}

- (NSString*)content
{
	if(!_buffer)
		return nil;

	NSMutableString* str = [NSMutableString string];
	_buffer->visit_data([str](char const* bytes, size_t offset, size_t len, bool*){
		[str appendString:[NSString stringWithUTF8String:bytes length:len]];
	});
	return str;
}

- (void)setContent:(NSString*)newContent
{
	if(newContent)
	{
		[self createBuffer];
		_buffer->replace(0, _buffer->size(), to_s(newContent));
		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentContentDidChangeNotification object:self];
	}
	else if(_buffer)
	{
		ASSERT_EQ(_openCount, 0);
		[self deleteBuffer];
	}
}

// =========
// = Marks =
// =========

- (void)setMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos content:(NSString*)value
{
	if(_buffer)
	{
		_buffer->set_mark(_buffer->convert(aPos), to_s(aMark), to_s(value));
		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentMarksDidChangeNotification object:self];
	}
	else if(_path)
	{
		document::marks.add(to_s(_path), aPos, to_s(aMark), to_s(value));
	}
}

- (void)removeMarkOfType:(NSString*)aMark atPosition:(text::pos_t const&)aPos
{
	if(aPos == text::pos_t::undefined)
		return [self removeAllMarksOfType:aMark];

	if(_buffer)
	{
		_buffer->remove_mark(_buffer->convert(aPos), to_s(aMark));
		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentMarksDidChangeNotification object:self];
	}
	else if(_path)
	{
		document::marks.remove(to_s(_path), aPos, to_s(aMark));
	}
}

- (void)removeAllMarksOfType:(NSString*)aMark
{
	if(_buffer)
	{
		_buffer->remove_all_marks(to_s(aMark));
		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentMarksDidChangeNotification object:self];
	}
	else if(_path)
	{
		document::marks.remove_all(to_s(_path), to_s(aMark));
	}
}

+ (void)removeAllMarksOfType:(NSString*)aMark
{
	DocumentContainer.remove_all_marks(aMark);
	document::marks.remove_all(to_s(aMark));
}

- (NSString*)stringifyMarksOfType:(NSString*)aMark
{
	std::vector<std::string> v;
	if(_buffer)
	{
		for(auto const& mark : _buffer->get_marks(0, _buffer->size(), to_s(aMark)))
			v.push_back(text::format("'%s'", std::string(_buffer->convert(mark.first)).c_str()));
	}
	else
	{
		for(auto const& mark : document::marks.get(to_s(_path), to_s(aMark)))
			v.push_back(text::format("'%s'", std::string(mark.first).c_str()));
	}
	return to_ns(v.empty() ? NULL_STR : "( " + text::join(v, ", ") + " )");
}

// =======================
// = Observe File System =
// =======================

- (void)setObserveFileSystem:(BOOL)flag
{
	if(_observeFileSystem == flag)
		return;

	struct watch_t : document::watch_base_t
	{
		watch_t (std::string const& path, OakDocument* document) : watch_base_t(path), _self(document) { }

		void callback (int flags, std::string const& newPath)
		{
			[_self fileSystemDidChangeToPath:to_ns(newPath) flags:flags];
		}

	private:
		__weak OakDocument* _self;
	};

	_observeFileSystem = flag;
	_fileSystemObserver.reset();

	if(flag && _path)
		_fileSystemObserver = std::make_unique<watch_t>(to_s(_path), self);
}

- (void)fileSystemDidChangeToPath:(NSString*)newPath flags:(int)flags
{
	ASSERT_NE(_openCount, 0);

	if((flags & NOTE_RENAME) == NOTE_RENAME)
	{
		self.observeFileSystem = NO;
		self.path = newPath;
		self.observeFileSystem = YES;
	}
	else if((flags & NOTE_DELETE) == NOTE_DELETE)
	{
		self.onDisk = access([_path fileSystemRepresentation], F_OK) == 0;
	}
	else if((flags & NOTE_WRITE) == NOTE_WRITE || (flags & NOTE_CREATE) == NOTE_CREATE)
	{
		self.onDisk = access([_path fileSystemRepresentation], F_OK) == 0;
		self.needsImportDocumentChanges = YES;
	}
}

- (void)setNeedsImportDocumentChanges:(BOOL)flag
{
	if(_needsImportDocumentChanges == flag)
		return;

	if(_needsImportDocumentChanges = flag)
	{
		if(NSApp.isActive)
		{
			[self importDocumentChanges:self];
		}
		else
		{
			__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidBecomeActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
				[[NSNotificationCenter defaultCenter] removeObserver:observerId];
				if(self.isOpen)
					[self importDocumentChanges:self];
			}];
		}
	}
}

- (void)importDocumentChanges:(id)sender
{
	self.needsImportDocumentChanges = NO;

	struct open_callback_t : file::open_callback_t
	{
		open_callback_t (OakDocument* self) : _self(self) { }

		void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			if(_encoding_state == kEncodingUseDisk)
			{
				_encoding_state = kEncodingTestProbability;
				context->set_charset(to_s(_self.diskEncoding));
			}
			else if(_encoding_state == kEncodingTestProbability)
			{
				_encoding_state = kEncodingUseFallback;

				encoding::classifier_t db;
				static std::string const kEncodingFrequenciesPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/EncodingFrequencies.binary");
				db.load(kEncodingFrequenciesPath);

				std::multimap<double, std::string> probabilities;
				for(auto const& charset : db.charsets())
					probabilities.emplace(1 - db.probability(content->begin(), content->end(), charset), charset);
				if(!probabilities.empty() && probabilities.begin()->first < 1)
						context->set_charset(probabilities.begin()->second);
				else	context->set_charset("ISO-8859-1");
			}
			else if(_encoding_state == kEncodingUseFallback)
			{
				_encoding_state = kEncodingAbort;
				context->set_charset("WINDOWS-1252");
			}
			else
			{
				abort();
			}
		}

		void select_line_feeds (std::string const& path, io::bytes_ptr content, file::open_context_ptr context) { context->set_line_feeds(to_s(_self.diskNewlines)); }
		void select_file_type (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)  { context->set_file_type(to_s(_self.fileType)); }
		void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)        { fprintf(stderr, "%s: %s\n", path.c_str(), message.c_str()); }

		void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, std::string const& fileType, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
		{
			if(!_self.isOpen)
				return;

			ng::buffer_t& buffer = [_self buffer];
			ng::undo_manager_t& undoManager = [_self undoManager];

			std::string const yours = std::string(content->begin(), content->end());
			std::string const mine  = buffer.substr(0, buffer.size());

			if(yours == mine)
			{
				_self.savedRevision = _self.revision;
			}
			else if(!_self.isDocumentEdited)
			{
				undoManager.begin_undo_group(ng::ranges_t(0));
				buffer.replace(0, buffer.size(), yours);
				undoManager.end_undo_group(ng::ranges_t(0), true); // This bumps the revision

				_self.revision      = buffer.revision();
				_self.savedRevision = _self.revision;
				[_self snapshot];
				[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentContentDidChangeNotification object:_self];
			}
			else if(_self->_snapshot)
			{
				std::string const original = _self->_snapshot->substr(0, _self->_snapshot->size());
				_self->_snapshot = std::make_unique<ng::detail::storage_t>(ng::buffer_t(yours.c_str()).storage());

				bool conflict = false;
				std::string const merged = merge(original, mine, yours, &conflict);

				if(utf8::is_valid(merged.begin(), merged.end()))
				{
					undoManager.begin_undo_group(ng::ranges_t(0));
					buffer.replace(0, buffer.size(), merged);
					undoManager.end_undo_group(ng::ranges_t(0), true); // This bumps the revision

					_self.revision      = buffer.revision();
					_self.savedRevision = merged == yours ? _self.revision : -1;
					[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentContentDidChangeNotification object:_self];
				}
			}
			else
			{
				NSLog(@"OakDocument: File changed on disk and we lack snapshot for 3-way merge");
				// TODO Warn user that we failed to do 3-way merge
			}

			_run_loop.stop();
		}

		void wait () { _run_loop.start(); }

	private:
		OakDocument* _self;
		enum { kEncodingUseDisk, kEncodingTestProbability, kEncodingUseFallback, kEncodingAbort } _encoding_state = kEncodingUseDisk;
		cf::run_loop_t _run_loop;
	};

	auto cb = std::make_shared<open_callback_t>(self);
	file::open(to_s(_path), _authorization, cb);
	cb->wait();
}

// =========
// = Other =
// =========

- (BOOL)performReplacements:(std::multimap<std::pair<size_t, size_t>, std::string> const&)someReplacements checksum:(uint32_t)crc32
{
	ASSERT_EQ(_openCount, 0);
	ASSERT(_path);
	ASSERT(!_buffer);

	if(someReplacements.empty())
		return NO;

	[self createBuffer];

	boost::crc_32_type check;
	file::reader_t reader(to_s(_path));
	while(io::bytes_ptr bytes = reader.next())
	{
		check.process_bytes(bytes->get(), bytes->size());
		_buffer->insert(_buffer->size(), bytes->get(), bytes->size());
	}

	if(crc32 != check.checksum())
	{
		[self deleteBuffer];
		return NO;
	}

	self.diskEncoding = to_ns(reader.encoding().charset());
	self.diskNewlines = to_ns(reader.encoding().newlines());

	// TODO Preserve visibleIndex, selectionRange, and folded
	document::marks.move_to_buffer(to_s(_path), *_buffer);
	riterate(pair, someReplacements)
		_buffer->replace(pair->first.first, pair->first.second, pair->second);
	document::marks.copy_from_buffer(to_s(_path), *_buffer);

	return YES;
}
@end
