#import "OakDocument Private.h"
#import "OakDocumentEditor.h"
#import "EncodingView.h"
#import "Printing.h"
#import "watch.h"
#import "merge.h"
#import "document.h" // kBookmarkIdentifier
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <OakAppKit/OakSavePanel.h>
#import <OakAppKit/NSAlert Additions.h>
#import <BundlesManager/BundlesManager.h>
#import <authorization/constants.h>
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
NSString* OakDocumentWillReloadNotification       = @"OakDocumentWillReloadNotification";
NSString* OakDocumentDidReloadNotification        = @"OakDocumentDidReloadNotification";
NSString* OakDocumentWillSaveNotification         = @"OakDocumentWillSaveNotification";
NSString* OakDocumentDidSaveNotification          = @"OakDocumentDidSaveNotification";
NSString* OakDocumentWillCloseNotification        = @"OakDocumentWillCloseNotification";
NSString* OakDocumentWillShowAlertNotification    = @"OakDocumentWillShowAlertNotification";

@interface OakDocument ()
{
	OBJC_WATCH_LEAKS(OakDocument);

	NSHashTable* _documentEditors;

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
@property (nonatomic, readonly) BOOL shouldSniffFileType;

@property (nonatomic) NSArray<void(^)(OakDocumentIOResult, NSString*, oak::uuid_t const&)>* loadCompletionHandlers;

// These are also exposed in ‘OakDocument Private.h’
@property (nonatomic) NSInteger   revision;
@property (nonatomic) NSInteger   savedRevision;
@property (nonatomic) NSString*   backupPath;
@property (nonatomic) NSString*   folded;
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

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@>", [self class], self.displayName];
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

- (std::map<std::string, std::string>)variables
{
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

	return variables;
}

- (void)updateSpellingSettings:(BOOL)updateSpelling andIndentSettings:(BOOL)updateIndent
{
	if(!_buffer)
		return;

	settings_t const settings = settings_for_path(to_s(_virtualPath ?: _path), to_s(_fileType), to_s(_directory ?: [_path stringByDeletingLastPathComponent]), self.variables);
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
		_documentEditors = [NSHashTable weakObjectsHashTable];

		DocumentContainer.add(self);
	}
	return self;
}

- (instancetype)initWithPath:(NSString*)aPath
{
	if(self = [self init])
	{
		_path   = aPath;
		_onDisk = access([_path fileSystemRepresentation], F_OK) == 0;
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
		if([someData respondsToSelector:@selector(enumerateByteRangesUsingBlock:)]) // MAC_OS_X_VERSION_10_9
		{
			[someData enumerateByteRangesUsingBlock:^(void const* buf, NSRange range, BOOL*){
				_buffer->insert(range.location, (char const*)buf, range.length);
			}];
		}
		else
		{
			_buffer->insert(0, (char const*)[someData bytes], [someData length]);
		}
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
			_onDisk         = access([_path fileSystemRepresentation], F_OK) == 0;
			_fileType       = to_ns(path::get_attr(path, "com.macromates.backup.file-type"));
			_diskEncoding   = to_ns(path::get_attr(path, "com.macromates.backup.encoding"));
			_diskNewlines   = to_ns(path::get_attr(path, "com.macromates.backup.newlines"));
			_customName     = to_ns(path::get_attr(path, "com.macromates.backup.custom-name"));
			_untitledCount  = atoi(path::get_attr(path, "com.macromates.backup.untitled-count").c_str());

			if(path::get_attr(path, "com.macromates.backup.modified") == "YES")
				_savedRevision = _revision-1;

			return self;
		}
	}
	return nil;
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

- (NSString*)displayNameWithExtension:(BOOL)flag
{
	NSString* res = [self.displayName stringByReplacingOccurrencesOfString:@"/" withString:@":"];
	if(!flag)
		return res;

	if(_customName && OakNotEmptyString([res pathExtension]))
		return res;

	if(_path)
		return [_path lastPathComponent];

	if(_buffer && _buffer->grammar())
	{
		if(bundles::item_ptr item = bundles::lookup(_buffer->grammar()->uuid()))
		{
			if(NSString* ext = to_ns(item->value_for_field(bundles::kFieldGrammarExtension)))
				return [res stringByAppendingPathExtension:ext];
		}
	}
	else if(_fileType)
	{
		for(auto const& item : bundles::query(bundles::kFieldGrammarScope, to_s(_fileType)))
		{
			if(NSString* ext = to_ns(item->value_for_field(bundles::kFieldGrammarExtension)))
				return [res stringByAppendingPathExtension:ext];
		}
	}
	return res;
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
	if(self.isOpen)
	{
		[self setBufferGrammarForCurrentFileType];
		[self updateSpellingSettings:YES andIndentSettings:YES];
	}
}

- (std::map<std::string, std::string>)extendedAttributeds
{
	for(OakDocumentEditor* editor in self.documentEditors)
		[editor documentWillSave:self];

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
	if(_keepBackupFile)
	{
		[_backupTimer invalidate];
		_backupTimer = [NSTimer scheduledTimerWithTimeInterval:2 target:self selector:@selector(backupTimerDidFire:) userInfo:nil repeats:NO];
	}
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

	path = [path stringByAppendingPathComponent:[self displayNameWithExtension:YES]];
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

		std::string const temp = path::temp("backup");
		int fd = open(temp.c_str(), O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
		if(fd == -1)
		{
			perrorf("saveBackup: open(\"%s\")", temp.c_str());
			return NO;
		}

		bool error = _buffer->visit_data([fd](char const* bytes, size_t offset, size_t len, bool* stop){
			if(*stop = write(fd, bytes, len) != len)
				perrorf("saveBackup: write");
		});

		close(fd);

		if(error)
			return unlink(temp.c_str()), NO;

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

		path::set_attributes(temp, attr);

		if(!path::rename_or_copy(temp, to_s(self.backupPath)))
			return NO;

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

- (void)updateRecentDocumentMenu
{
	if(_recentTrackingDisabled || !_path || _virtualPath)
		return;

	NSURL* url = [NSURL fileURLWithPath:_path];
	CFRunLoopPerformBlock(CFRunLoopGetMain(), kCFRunLoopCommonModes, ^{
		// This is not thread-safe so we ensure that we are on the main thread
		[[NSDocumentController sharedDocumentController] noteNewRecentDocumentURL:url];
	});
}

- (BOOL)isLoading
{
	return _loadCompletionHandlers != nil;
}

- (void)loadModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID))block
{
	block = block ?: ^(OakDocumentIOResult, NSString*, oak::uuid_t const&){ };

	if(++self.openCount != 1)
	{
		if(!_loadCompletionHandlers)
				block(OakDocumentIOResultSuccess, nil, oak::uuid_t());
		else	_loadCompletionHandlers = [_loadCompletionHandlers arrayByAddingObject:block];
		return;
	}

	if(_backupPath)
	{
		ASSERT(!_buffer);

		BOOL isModified = self.isDocumentEdited;
		std::string const path = to_s(_backupPath);
		[self didLoadContent:std::make_shared<io::bytes_t>(path::content(path)) attributes:path::attributes(path) encoding:encoding::type(to_s(_diskNewlines), to_s(_diskEncoding))];

		if(isModified)
		{
			_snapshot.reset();
			self.savedRevision = _revision-1;
		}

		return block(OakDocumentIOResultSuccess, nil, oak::uuid_t());
	}

	struct callback_t : file::open_callback_t
	{
		callback_t (OakDocument* self, NSWindow* window, void(^block)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID)) : _self(self), _window(window), _block(block) { }

		void select_charset (std::string const& path, io::bytes_ptr content, file::open_context_ptr context)
		{
			[_window.attachedSheet orderOut:_self];

			EncodingWindowController* controller = [[EncodingWindowController alloc] initWithFirst:content->begin() last:content->end()];
			controller.displayName = _self.displayName;

			__block encoding::classifier_t db;
			static std::string const kEncodingFrequenciesPath = path::join(path::home(), "Library/Caches/com.macromates.TextMate/EncodingFrequencies.binary");
			db.load(kEncodingFrequenciesPath);

			std::multimap<double, std::string> probabilities;
			for(auto const& charset : db.charsets())
				probabilities.emplace(1 - db.probability(content->begin(), content->end(), charset), charset);
			if(!probabilities.empty() && probabilities.begin()->first < 1)
				controller.encoding = [NSString stringWithCxxString:probabilities.begin()->second];

			[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillShowAlertNotification object:_self];
			[controller beginSheetModalForWindow:_window completionHandler:^(NSModalResponse response){
				if(response != NSModalResponseAbort)
				{
					context->set_charset(to_s(controller.encoding));
					if(controller.trainClassifier)
					{
						db.learn(content->begin(), content->end(), to_s(controller.encoding));
						db.save(kEncodingFrequenciesPath);
					}
				}
			}];
		}

		void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
		{
			[_self didLoadContent:io::bytes_ptr() attributes:{ } encoding:{ }];
			_block(OakDocumentIOResultFailure, to_ns(message), filter);
		}

		void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
		{
			NSString* docPath = _self.virtualPath ?: _self.path;

			// Check if user has an explicit binding for this path (e.g. *.md → Markdown)
			if(!_self.fileType && docPath)
				_self.fileType = to_ns(settings_for_path(to_s(docPath), scope::scope_t(), to_s(_self.directory ?: [_self.path stringByDeletingLastPathComponent])).get(kSettingsFileTypeKey, NULL_STR));

			// Check if a grammar recognizes the content (e.g. #!/usr/bin/ruby → Ruby)
			if(!_self.fileType)
				_self.fileType = to_ns(file::type_from_bytes(content));

			// Check if a grammar recognizes the path extension (.git/config → Git Config)
			if(!_self.fileType && docPath)
				_self.fileType = to_ns(file::type_from_path(to_s(docPath)));

			[_self didLoadContent:content attributes:attributes encoding:encoding];
			_block(OakDocumentIOResultSuccess, nil, oak::uuid_t());
		}

	private:
		OakDocument* _self;
		NSWindow* _window;
		void(^_block)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID);
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

	self.loadCompletionHandlers = @[ block ];
	auto cb = std::make_shared<callback_t>(self, aWindow, ^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
		auto blocks = self.loadCompletionHandlers;
		self.loadCompletionHandlers = nil;
		for(void(^f)(OakDocumentIOResult, NSString*, oak::uuid_t const&) in blocks)
			f(result, errorMessage, filterUUID);
	});
	file::open(to_s(_path), _authorization, cb, content);
}

- (void)didLoadContent:(io::bytes_ptr)content attributes:(std::map<std::string, std::string> const&)attributes encoding:(encoding::type)encoding
{
	if(!content) // Loading failed
	{
		--self.openCount;
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

	[self setBufferGrammarForCurrentFileType];
	[self updateSpellingSettings:YES andIndentSettings:YES];

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
	[self updateRecentDocumentMenu];
	self.observeFileSystem = YES;
}

// =================
// = Save Document =
// =================

- (void)didSaveAtPath:(NSString*)aPath withEncoding:(encoding::type)encoding success:(BOOL)success
{
	if(success)
	{
		self.onDisk        = YES;
		self.diskEncoding  = to_ns(encoding.charset());
		self.diskNewlines  = to_ns(encoding.newlines());
		self.savedRevision = _revision;

		[self snapshot];
		[self removeBackup];
		[self updateRecentDocumentMenu];

		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentDidSaveNotification object:self];
	}
	self.observeFileSystem = self.isOpen;
}

- (void)saveModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID))block
{
	if(!self.isOpen && self.isDocumentEdited && _backupPath)
	{
		[self loadModalForWindow:aWindow completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
			if(result == OakDocumentIOResultSuccess)
			{
				[self saveModalForWindow:aWindow completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
					block(result, errorMessage, filterUUID);
					[self close];
				}];
			}
			else
			{
				NSString* errorMessage = [NSString stringWithFormat:@"Cannot save ‘%@’: Failed to restore backup ‘%@’.", self.displayName, [_backupPath stringByAbbreviatingWithTildeInPath]];
				block(OakDocumentIOResultFailure, errorMessage, oak::uuid_t());
			}
		}];
		return;
	}

	if(_buffer)
	{
		self.observeFileSystem = NO;

		std::map<std::string, std::string> attributes;
		if(volume::settings(to_s(_path)).extended_attributes())
			attributes = [self extendedAttributeds];

		encoding::type encoding = encoding::type(to_s(_diskNewlines), to_s(_diskEncoding));

		settings_t const settings = settings_for_path(to_s(_path), to_s(_fileType), to_s(_directory ?: [_path stringByDeletingLastPathComponent]));
		if(encoding.charset() == kCharsetNoEncoding)
			encoding.set_charset(settings.get(kSettingsEncodingKey, kCharsetUTF8));
		if(encoding.newlines() == NULL_STR)
			encoding.set_newlines(settings.get(kSettingsLineEndingsKey, kLF));

		struct callback_t : file::save_callback_t
		{
			callback_t (OakDocument* document, NSWindow* window, void(^block)(OakDocumentIOResult result, NSString* path, encoding::type const& encoding, NSString* errorMessage, oak::uuid_t const& filterUUID)) : _document(document), _window(window), _block(block)
			{
			}

			void select_path (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
			{
				if(!_window)
					return;

				[OakSavePanel showWithPath:[_document displayNameWithExtension:YES] directory:_document.directory fowWindow:_window encoding:encoding::type(to_s(_document.diskNewlines), to_s(_document.diskEncoding)) completionHandler:^(NSString* path, encoding::type const& encoding){
					if(path)
					{
						_document.path         = path;
						_document.diskEncoding = to_ns(encoding.charset());
						_document.diskNewlines = to_ns(encoding.newlines());
						context->set_path(to_s(path));
					}
					else
					{
						_cancel = true;
					}
				}];
			}

			void select_make_writable (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
			{
				if(!_window)
					return;

				[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillShowAlertNotification object:_document];
				NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"The file “%@” is locked.", _document.displayName] informativeText:@"Do you want to overwrite it anyway?" buttons:@"Overwrite", @"Cancel", nil];
				OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
					if(returnCode == NSAlertFirstButtonReturn)
							context->set_make_writable(true);
					else	_cancel = true;
				});
			}

			void select_create_parent (std::string const& path, io::bytes_ptr content, file::save_context_ptr context)
			{
				if(!_window)
					return;

				[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillShowAlertNotification object:_document];
				NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"No parent folder for “%@”.", _document.displayName] informativeText:[NSString stringWithFormat:@"Do you wish to create a folder at “%@”?", [NSString stringWithCxxString:path::with_tilde(path::parent(path))]] buttons:@"Create Folder", @"Cancel", nil];
				OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
					if(returnCode == NSAlertFirstButtonReturn)
							context->set_create_parent(true);
					else	_cancel = true;
				});
			}

			void obtain_authorization (std::string const& path, io::bytes_ptr content, osx::authorization_t auth, file::save_context_ptr context)
			{
				if(!_window)
					return;

				if(auth.obtain_right(kAuthRightName))
						context->set_authorization(auth);
				else	_cancel = true;
			}

			void select_charset (std::string const& path, io::bytes_ptr content, std::string const& charset, file::save_context_ptr context)
			{
				if(charset != kCharsetNoEncoding)
				{
					if(!_window)
						return;

					[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillShowAlertNotification object:_document];
					NSAlert* alert = [NSAlert tmAlertWithMessageText:[NSString stringWithFormat:@"Unable to save “%@” using “%@” as encoding.", _document.displayName, to_ns(charset)] informativeText:@"Please choose another encoding:" buttons:@"Save", @"Cancel", nil];
					OakEncodingPopUpButton* encodingPopUp = [OakEncodingPopUpButton new];
					[alert setAccessoryView:encodingPopUp];
					OakShowAlertForWindow(alert, _window, ^(NSInteger returnCode){
						if(returnCode == NSAlertFirstButtonReturn)
								context->set_charset(to_s(encodingPopUp.encoding));
						else	_cancel = true;
					});
					[[alert window] recalculateKeyViewLoop];
				}
				else
				{
					context->set_charset(kCharsetUTF8);
				}
			}

			void did_save (std::string const& path, io::bytes_ptr content, encoding::type const& encoding, bool success, std::string const& message, oak::uuid_t const& filter)
			{
				_block(success ? OakDocumentIOResultSuccess : (_cancel ? OakDocumentIOResultCancel : OakDocumentIOResultFailure), to_ns(path), encoding, to_ns(message), filter);
			}

		private:
			OakDocument* _document;
			NSWindow* _window;
			bool _cancel = false;
			void(^_block)(OakDocumentIOResult result, NSString* path, encoding::type const& encoding, NSString* errorMessage, oak::uuid_t const& filterUUID);
		};

		BOOL closeDocument = self.isOpen;
		if(closeDocument)
			++self.openCount;

		auto cb = std::make_shared<callback_t>(self, aWindow, ^(OakDocumentIOResult result, NSString* path, encoding::type const& encoding, NSString* errorMessage, oak::uuid_t const& filterUUID){
			if(closeDocument)
				[self didSaveAtPath:path withEncoding:encoding success:result == OakDocumentIOResultSuccess];
			if(block)
				block(result, errorMessage, filterUUID);
			if(closeDocument)
				[self close];
		});

		[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillSaveNotification object:self];

		io::bytes_ptr content = std::make_shared<io::bytes_t>(_buffer->size());
		_buffer->visit_data([&](char const* bytes, size_t offset, size_t len, bool*){
			memcpy(content->get() + offset, bytes, len);
		});

		file::save(to_s(_path), cb, _authorization, content, attributes, encoding, std::vector<oak::uuid_t>() /* binary import filters */, std::vector<oak::uuid_t>() /* text import filters */);
	}
	else
	{
		NSString* errorMessage = [NSString stringWithFormat:@"Cannot save ‘%@’: no content. Has backup %s, is edited %s.", self.displayName, BSTR(_backupPath), BSTR(self.isDocumentEdited)];
		block(OakDocumentIOResultFailure, errorMessage, oak::uuid_t());
	}
}

// =================

- (void)close
{
	if(--self.openCount != 0)
		return;

	[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillCloseNotification object:self];

	if(_path && _buffer)
	{
		document::marks.copy_from_buffer(to_s(_path), *_buffer);
		if(_onDisk && !self.isDocumentEdited && volume::settings(to_s(_path)).extended_attributes())
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

		void will_replace (size_t from, size_t to, char const* buf, size_t len)
		{
			ng::buffer_t const& buffer = [_self buffer];
			_should_sniff_file_type = from == oak::cap(buffer.begin(0), from, buffer.eol(0)) && _self.shouldSniffFileType;
			_file_type = _should_sniff_file_type ? file_type(buffer) : NULL_STR;
		}

		void did_replace (size_t from, size_t to, char const* buf, size_t len)
		{
			_size += len - (to - from);
			_self.bufferEmpty = _size == 0;

			if(_should_sniff_file_type)
			{
				std::string const newFileType = file_type([_self buffer]);
				if(newFileType != NULL_STR && newFileType != _file_type)
					_self.fileType = to_ns(newFileType);
			}

			[_self scheduleBackup];
		}

	private:
		static std::string file_type (ng::buffer_t const& buffer)
		{
			return file::type_from_bytes(std::make_shared<io::bytes_t>(buffer.substr(buffer.begin(0), std::min<size_t>(buffer.eol(0), 2048))));
		}

		size_t _size = 0;
		OakDocument* _self;

		bool _should_sniff_file_type;
		std::string _file_type;
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
- (BOOL)shouldSniffFileType                           { return settings_for_path(to_s(_virtualPath ?: _path), scope::scope_t(), to_s(_directory ?: [_path stringByDeletingLastPathComponent])).get(kSettingsFileTypeKey, NULL_STR) == NULL_STR; }

- (BOOL)canUndo                                       { return _undoManager && _undoManager->can_undo(); }
- (BOOL)canRedo                                       { return _undoManager && _undoManager->can_redo(); }

- (void)beginUndoGrouping
{
	ASSERT(_buffer);

	if(OakDocumentEditor* editor = self.documentEditors.firstObject)
			_undoManager->begin_undo_group(editor.selection);
	else	_undoManager->begin_undo_group(ng::convert(*_buffer, to_s(_selection)));
}

- (void)endUndoGrouping
{
	ASSERT(_buffer);

	if(OakDocumentEditor* editor = self.documentEditors.firstObject)
			_undoManager->end_undo_group(editor.selection);
	else	_undoManager->end_undo_group(ng::convert(*_buffer, to_s(_selection)));
	self.revision = _buffer->revision();
}

- (void)undo
{
	ng::ranges_t sel = _undoManager->undo();
	for(OakDocumentEditor* editor in self.documentEditors)
		editor.selection = sel;
}

- (void)redo
{
	ng::ranges_t sel = _undoManager->redo();
	for(OakDocumentEditor* editor in self.documentEditors)
		editor.selection = sel;
}

// We currently store these in buffer_t
- (BOOL)isContinuousSpellCheckingEnabled              { return _buffer && _buffer->live_spelling(); }
- (NSString*)spellingLanguage                         { return _buffer ? to_ns(_buffer->spelling_language()) : nil; }
- (NSUInteger)tabSize                                 { return _buffer ? _buffer->indent().tab_size() : 4; }
- (BOOL)softTabs                                      { return _buffer && _buffer->indent().soft_tabs(); }

- (void)setContinuousSpellCheckingEnabled:(BOOL)value { if(_buffer) _buffer->set_live_spelling(value); }
- (void)setSpellingLanguage:(NSString*)value          { if(_buffer) _buffer->set_spelling_language(to_s(value)); }
- (void)setTabSize:(NSUInteger)value                  { if(_buffer) _buffer->indent().set_tab_size(value); }
- (void)setSoftTabs:(BOOL)value                       { if(_buffer) _buffer->indent().set_soft_tabs(value); }

- (ng::buffer_t&)buffer                               { ASSERT(_buffer); return *_buffer; }
- (ng::undo_manager_t&)undoManager                    { ASSERT(_undoManager); return *_undoManager; }

- (NSArray<BundleGrammar*>*)proposedGrammars
{
	NSMutableArray* res = [NSMutableArray array];

	std::string const firstLine = _buffer ? _buffer->substr(_buffer->begin(0), std::min<size_t>(_buffer->eol(0), 2048)) : NULL_STR;
	std::string const path      = to_s(_virtualPath ?: _path);

	for(Bundle* bundle in [BundlesManager sharedInstance].bundles)
	{
		for(BundleGrammar* grammar in bundle.grammars)
		{
			if(firstLine != NULL_STR && grammar.firstLineMatch && regexp::search(to_s(grammar.firstLineMatch), firstLine))
			{
				[res addObject:grammar];
				break;
			}
			else if(path != NULL_STR)
			{
				for(NSString* ext in grammar.filePatterns)
				{
					if(path::rank(path, to_s(ext)))
					{
						[res addObject:grammar];
						break;
					}
				}
			}
		}
	}
	return res;
}

- (void)enumerateSymbolsUsingBlock:(void(^)(text::pos_t const& pos, NSString* symbol))block
{
	if(self.isOpen && _buffer)
	{
		_buffer->wait_for_repair();
		for(auto const& pair : _buffer->symbols())
			block(_buffer->convert(pair.first), to_ns(pair.second));
	}
}

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

// ===============================
// = DocumentEditor Registration =
// ===============================

- (void)registerDocumentEditor:(OakDocumentEditor*)anEditor
{
	[_documentEditors addObject:anEditor];
}

- (void)unregisterDocumentEditor:(OakDocumentEditor*)anEditor
{
	[_documentEditors removeObject:anEditor];
}

- (NSArray<OakDocumentEditor*>*)documentEditors
{
	NSMutableArray* res = [NSMutableArray array];
	for(OakDocumentEditor* editor in _documentEditors)
	{
		if(editor)
			[res addObject:editor];
	}
	return res;
}

- (BOOL)handleOutput:(std::string const&)string placement:(output::type)place format:(output_format::type)format caret:(output_caret::type)caret inputRanges:(ng::ranges_t const&)ranges environment:(std::map<std::string, std::string> const&)environment
{
	OakDocumentEditor* documentEditor = self.documentEditors.firstObject;
	return [documentEditor handleOutput:string placement:place format:format caret:caret inputRanges:ranges environment:environment];
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
		self.path = newPath;
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

		void show_error (std::string const& path, std::string const& message, oak::uuid_t const& filter)
		{
			fprintf(stderr, "%s: %s\n", path.c_str(), message.c_str());
		}

		void show_content (std::string const& path, io::bytes_ptr content, std::map<std::string, std::string> const& attributes, encoding::type const& encoding, std::vector<oak::uuid_t> const& binaryImportFilters, std::vector<oak::uuid_t> const& textImportFilters)
		{
			if(!_self.isOpen)
				return;

			ng::buffer_t& buffer = [_self buffer];

			std::string const yours = std::string(content->begin(), content->end());
			std::string const mine  = buffer.substr(0, buffer.size());

			if(yours == mine)
			{
				_self.savedRevision = _self.revision;
			}
			else if(!_self.isDocumentEdited)
			{
				[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillReloadNotification object:_self];
				[_self beginUndoGrouping];
				buffer.replace(0, buffer.size(), yours);
				[_self endUndoGrouping];

				_self.savedRevision = _self.revision;
				[_self snapshot];
				[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentDidReloadNotification object:_self];
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
					[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentWillReloadNotification object:_self];
					[_self beginUndoGrouping];
					buffer.replace(0, buffer.size(), merged);
					[_self endUndoGrouping];

					_self.savedRevision = merged == yours ? _self.revision : -1;
					[[NSNotificationCenter defaultCenter] postNotificationName:OakDocumentDidReloadNotification object:_self];
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
	if(self.isOpen)
	{
		OakDocumentEditor* documentEditor = self.documentEditors.firstObject ?: [OakDocumentEditor documentEditorWithDocument:self fontScaleFactor:1];
		[documentEditor performReplacements:someReplacements];
		return YES;
	}

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

- (void)runPrintOperationModalForWindow:(NSWindow*)aWindow fontName:(NSString*)aFontName
{
	NSPrintOperation* printer = [NSPrintOperation printOperationWithView:[[OakDocumentPrintableView alloc] initWithDocument:self fontName:aFontName]];

	NSMutableDictionary* info = [[printer printInfo] dictionary];
	info[@"OakPrintThemeUUID"]   = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintThemeUUID"];
	info[@"OakPrintFontSize"]    = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintFontSize"];
	info[NSPrintHeaderAndFooter] = [[NSUserDefaults standardUserDefaults] objectForKey:@"OakPrintHeaderAndFooter"];

	[[printer printInfo] setVerticallyCentered:NO];
	[[printer printPanel] setOptions:[[printer printPanel] options] | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation];
	[[printer printPanel] addAccessoryController:[OakDocumentPrintOptionsViewController new]];

	[printer runOperationModalForWindow:aWindow delegate:nil didRunSelector:NULL contextInfo:nil];
}
@end
