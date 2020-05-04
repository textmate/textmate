#import "OakPasteboard.h"
#import "OakPasteboardSelector.h"
#import <crash/info.h>
#import <ns/ns.h>
#import <oak/oak.h>
#import <oak/debug.h>
#import <sqlite3.h>

static os_log_t const kLogSQLite     = os_log_create("Pasteboard", "sqlite");
static os_log_t const kLogPasteboard = os_log_create("Pasteboard", "history");

NSNotificationName const OakPasteboardDidChangeNotification = @"OakClipboardDidChangeNotification";

NSString* const OakReplacePboard                   = @"OakReplacePboard";
NSString* const OakPasteboardOptionsPboardType     = @"OakPasteboardOptionsPboardType";

NSString* const kUserDefaultsFindWrapAround        = @"findWrapAround";
NSString* const kUserDefaultsFindIgnoreCase        = @"findIgnoreCase";

NSString* const OakFindIgnoreWhitespaceOption      = @"ignoreWhitespace";
NSString* const OakFindFullWordsOption             = @"fullWordMatch";
NSString* const OakFindRegularExpressionOption     = @"regularExpression";

NSString* const kUserDefaultsDisablePersistentClipboardHistory = @"disablePersistentClipboardHistory";
NSString* const kUserDefaultsClipboardHistoryKeepAtLeast       = @"clipboardHistoryKeepAtLeast";
NSString* const kUserDefaultsClipboardHistoryKeepAtMost        = @"clipboardHistoryKeepAtMost";
NSString* const kUserDefaultsClipboardHistoryDaysToKeep        = @"clipboardHistoryDaysToKeep";

// ===========
// = SQLite3 =
// ===========

@interface OakPasteboard ()
+ (sqlite3*)SQLDatabase;
@end

static NSDictionary* ColumnsAsDictionary (sqlite3_stmt* stmt)
{
	NSMutableDictionary* item = [NSMutableDictionary dictionary];
	for(int i = 0; i < sqlite3_data_count(stmt); ++i)
	{
		id value;
		switch(sqlite3_column_type(stmt, i))
		{
			case SQLITE_INTEGER: value = [NSNumber numberWithInt:sqlite3_column_int(stmt, i)]; break;
			case SQLITE_FLOAT:   value = [NSNumber numberWithDouble:sqlite3_column_double(stmt, i)]; break;
			case SQLITE_TEXT:    value = [NSString stringWithUTF8String:(char const*)sqlite3_column_text(stmt, i)]; break;
			case SQLITE_BLOB:    value = [NSData dataWithBytes:sqlite3_column_blob(stmt, i) length:sqlite3_column_bytes(stmt, i)]; break;
			case SQLITE_NULL:    value = nil; break;
		}

		if(value)
			item[[NSString stringWithUTF8String:sqlite3_column_name(stmt, i)]] = value;
	}
	return item;
}

static NSArray* RunSQLStatement (sqlite3* db, char const* query, std::map<std::string, id> const& variables = { })
{
	NSMutableArray<NSMutableArray<NSDictionary*>*>* resultSet = [NSMutableArray array];

	BOOL res = YES;
	while(*query && res)
	{
		sqlite3_stmt* stmt = nullptr;
		char const* nextQuery = nullptr;
		if(sqlite3_prepare_v2(db, query, -1, &stmt, &nextQuery) == SQLITE_OK)
		{
			NSMutableArray<NSDictionary*>* rows;

			for(int i = 0; i < sqlite3_bind_parameter_count(stmt); ++i)
			{
				auto it = variables.find(sqlite3_bind_parameter_name(stmt, i+1));
				if(it != variables.end())
				{
					id value = it->second;
					if(!value || [value isKindOfClass:[NSNull class]])
					{
						sqlite3_bind_null(stmt, i+1);
					}
					else if([value isKindOfClass:[NSString class]])
					{
						sqlite3_bind_text(stmt, i+1, [value UTF8String], -1, SQLITE_STATIC);
					}
					else if([value isKindOfClass:[NSData class]])
					{
						sqlite3_bind_blob(stmt, i+1, [value bytes], [value length], SQLITE_STATIC);
					}
					else if([value isKindOfClass:[NSNumber class]])
					{
						static std::map<std::string, std::function<int(sqlite3_stmt*, int, id)>> const typeMapping = {
							{ @encode(BOOL),               [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.boolValue ? 1 : 0);       } },
							{ @encode(char),               [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.charValue);               } },
							{ @encode(unsigned char),      [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.unsignedCharValue);       } },
							{ @encode(short),              [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.shortValue);              } },
							{ @encode(unsigned short),     [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.unsignedShortValue);      } },
							{ @encode(int),                [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.intValue);              } },
							{ @encode(unsigned int),       [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.unsignedIntValue);      } },
							{ @encode(long),               [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.longValue);             } },
							{ @encode(unsigned long),      [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.unsignedLongValue);     } },
							{ @encode(long long),          [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.longLongValue);         } },
							{ @encode(unsigned long long), [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int64(stmt, i, value.unsignedLongLongValue); } },
							{ @encode(float),              [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_double(stmt, i, value.floatValue);           } },
							{ @encode(double),             [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_double(stmt, i, value.doubleValue);          } },
						};

						auto pair = [value objCType] ? typeMapping.find([value objCType]) : typeMapping.end();
						if(pair != typeMapping.end())
								pair->second(stmt, i+1, value);
						else	sqlite3_bind_text(stmt, i+1, [[value stringValue] UTF8String], -1, nullptr);
					}
				}
				else
				{
					os_log_error(kLogSQLite, "sqlite3: no variable for binding: ‘%{public}s’", sqlite3_bind_parameter_name(stmt, i+1));
				}
			}

			int status;
			while((status = sqlite3_step(stmt)) == SQLITE_ROW)
			{
				if(!rows)
					rows = [NSMutableArray array];
				[rows addObject:ColumnsAsDictionary(stmt)];
			}

			if(status != SQLITE_DONE)
			{
				os_log_error(kLogSQLite, "sqlite3_step: %{public}s executing %{public}s", sqlite3_errmsg(db), query);
				res = NO;
			}

			if(sqlite3_finalize(stmt) != SQLITE_OK)
			{
				os_log_error(kLogSQLite, "sqlite3_finalize: %{public}s", sqlite3_errmsg(db));
				res = NO;
			}

			if(res && rows)
				[resultSet addObject:rows];
		}
		else
		{
			os_log_error(kLogSQLite, "sqlite3_prepare_v2(%{public}s): %{public}s", query, sqlite3_errmsg(db));
			res = NO;
		}
		query = nextQuery;

		if(!res)
			resultSet = nil;
	}

	if(resultSet.count > 1)
			return resultSet;
	else	return resultSet.lastObject;
}

// ======================
// = OakPasteboardEntry =
// ======================

@implementation OakPasteboardEntry
- (id)initWithStrings:(NSArray<NSString*>*)strings options:(NSDictionary*)options flagged:(BOOL)flagged
{
	if(self = [self init])
	{
		_strings = strings;
		_options = options;
		_flagged = flagged;
	}
	return self;
}

- (NSString*)string
{
	return [_strings componentsJoinedByString:@"\n"];
}

- (NSUInteger)historyId
{
	return [_options[@"historyId"] integerValue];
}

- (BOOL)isEqual:(id)otherEntry
{
	if([otherEntry isKindOfClass:[OakPasteboardEntry class]])
		return self.historyId == ((OakPasteboardEntry*)otherEntry).historyId;
	return NO;
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@ [%@]>", [self class], [_strings componentsJoinedByString:@"|"], [_options.allKeys componentsJoinedByString:@"|"]];
}

- (void)setFlagged:(BOOL)newFlagged
{
	_flagged = newFlagged;
	if(NSUInteger historyId = self.historyId)
	{
		char const* query = nullptr;
		if(_flagged)
				query = "INSERT INTO flags (id) VALUES (:historyId);";
		else	query = "DELETE FROM flags WHERE id = :historyId;";
		RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":historyId", @(historyId) } });
	}
}

- (BOOL)fullWordMatch       { return [self.options[OakFindFullWordsOption] boolValue]; };
- (BOOL)ignoreWhitespace    { return [self.options[OakFindIgnoreWhitespaceOption] boolValue]; };
- (BOOL)regularExpression   { return [self.options[OakFindRegularExpressionOption] boolValue]; };

- (find::options_t)findOptions
{
	return find::options_t(
		([self fullWordMatch]       ? find::full_words         : find::none) |
		([NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindIgnoreCase] ? find::ignore_case : find::none) |
		([NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFindWrapAround] ? find::wrap_around : find::none) |
		([self ignoreWhitespace]    ? find::ignore_whitespace  : find::none) |
		([self regularExpression]   ? find::regular_expression : find::none));
}
@end

@interface OakPasteboard ()
@property (nonatomic) NSInteger changeCount;
@property (nonatomic, readonly) NSPasteboard* pasteboard;
@property (nonatomic, readonly) BOOL avoidsDuplicates;
- (void)checkForExternalPasteboardChanges;
@end

// ============================
// = Event Loop Idle Callback =
// ============================

namespace
{
	struct event_loop_idle_callback_t;
	static event_loop_idle_callback_t& idle_callback ();

	struct event_loop_idle_callback_t
	{
		event_loop_idle_callback_t () : _running(false) { _observer = CFRunLoopObserverCreate(kCFAllocatorDefault, kCFRunLoopBeforeWaiting, true, 100, &callback, NULL); start(); }
		~event_loop_idle_callback_t ()                  { stop(); CFRelease(_observer); }

		void start ()
		{
			if(_running)
				return;
			_running = true;
			CFRunLoopAddObserver(CFRunLoopGetCurrent(), _observer, kCFRunLoopCommonModes);
		}

		void stop ()
		{
			if(!_running)
				return;
			_running = false;
			CFRunLoopRemoveObserver(CFRunLoopGetCurrent(), _observer, kCFRunLoopCommonModes);
		}

		void add (OakPasteboard* aPasteboard)    { _pasteboards.insert(aPasteboard); }
		void remove (OakPasteboard* aPasteboard) { ASSERT(_pasteboards.find(aPasteboard) != _pasteboards.end()); _pasteboards.erase(aPasteboard); }

	private:
		static void callback (CFRunLoopObserverRef observer, CFRunLoopActivity activity, void* info)
		{
			for(auto const& it : idle_callback()._pasteboards)
				[it checkForExternalPasteboardChanges];
		}

		bool _running;
		CFRunLoopObserverRef _observer;
		std::set<OakPasteboard*> _pasteboards;
	};

	static event_loop_idle_callback_t& idle_callback ()
	{
		static event_loop_idle_callback_t res;
		return res;
	}
}

@implementation OakPasteboard
+ (void)initialize
{
	static dispatch_once_t onceToken = 0;
	dispatch_once(&onceToken, ^{
		[NSUserDefaults.standardUserDefaults registerDefaults:@{
			kUserDefaultsClipboardHistoryKeepAtLeast:  @25,
			kUserDefaultsClipboardHistoryKeepAtMost:  @500,
			kUserDefaultsClipboardHistoryDaysToKeep:   @30,
		}];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidBecomeActiveNotification:) name:NSApplicationDidBecomeActiveNotification object:NSApp];
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationDidResignActiveNotification:) name:NSApplicationDidResignActiveNotification object:NSApp];
	});
}

+ (void)applicationDidBecomeActiveNotification:(id)sender
{
	idle_callback().start();
}

+ (void)applicationDidResignActiveNotification:(id)sender
{
	idle_callback().stop();
}

+ (NSURL*)databaseURL
{
	NSError* error;
	if(NSURL* appSupport = [[NSFileManager.defaultManager URLForDirectory:NSApplicationSupportDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:&error] URLByAppendingPathComponent:@"TextMate"])
	{
		if([NSFileManager.defaultManager createDirectoryAtURL:appSupport withIntermediateDirectories:YES attributes:nil error:&error])
			return [appSupport URLByAppendingPathComponent:@"PasteboardHistory.db"];
	}
	[NSApp presentError:error];
	return nil;
}

+ (sqlite3*)SQLDatabase
{
	static sqlite3* db = nullptr;
	if(!db)
	{
		// =========================
		// = Delete CoreData files =
		// =========================

		for(NSURL* appSupport in [NSFileManager.defaultManager URLsForDirectory:NSApplicationSupportDirectory inDomains:NSUserDomainMask])
		{
			for(NSString* file in @[ @"ClipboardHistory.db", @"ClipboardHistory.db-shm", @"ClipboardHistory.db-wal" ])
			{
				NSURL* url = [[appSupport URLByAppendingPathComponent:@"TextMate"] URLByAppendingPathComponent:file];
				if([NSFileManager.defaultManager fileExistsAtPath:url.path])
				{
					NSURL* res;
					if([NSFileManager.defaultManager trashItemAtURL:url resultingItemURL:&res error:nil])
						os_log_info(kLogPasteboard, "Moved CoreData file to trash: %{public}@ → %{public}@", url.path.stringByAbbreviatingWithTildeInPath, res.path.stringByAbbreviatingWithTildeInPath);
				}
			}
		}

		// =========================

		BOOL memoryDatabase = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsDisablePersistentClipboardHistory];
		if(sqlite3_open(memoryDatabase ? ":memory:" : self.databaseURL.fileSystemRepresentation, &db) == SQLITE_OK)
		{
			if(os_log_info_enabled(kLogSQLite))
				os_log_info(kLogSQLite, "Opening sqlite3 database: %{public}@", memoryDatabase ? @":memory:" : self.databaseURL.path.stringByAbbreviatingWithTildeInPath);

			[NSNotificationCenter.defaultCenter addObserverForName:NSApplicationWillTerminateNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
				if(!memoryDatabase)
				{
					char const* query =
						"SELECT COUNT(*) AS count FROM strings LEFT JOIN groups ON string_id = strings.id WHERE string_id IS NULL;"
						"DELETE FROM strings WHERE id IN (SELECT strings.id FROM strings LEFT JOIN groups ON string_id = strings.id WHERE string_id IS NULL);";

					if(NSDictionary* row = RunSQLStatement(db, query).firstObject)
					{
						if(NSUInteger count = [row[@"count"] integerValue])
							os_log_info(kLogSQLite, "Garbage collected %lu string(s) from database", count);
					}
				}

				os_log_info(kLogSQLite, "Closing sqlite3 database");
				if(sqlite3_close(db) != SQLITE_OK)
					os_log_error(kLogSQLite, "sqlite3_close: %{public}s", sqlite3_errmsg(db));
			}];

			char const* query =
				"PRAGMA foreign_keys = on;"
				"CREATE TABLE IF NOT EXISTS 'clipboards' ("
				"   'id'               INTEGER PRIMARY KEY,"
				"   'name'             TEXT NOT NULL,"
				"   UNIQUE (name) ON CONFLICT IGNORE"
				");"
				"CREATE TABLE IF NOT EXISTS 'strings' ("
				"   'id'               INTEGER PRIMARY KEY,"
				"   'string'           TEXT NOT NULL,"
				"   UNIQUE (string) ON CONFLICT IGNORE"
				");"
				"CREATE TABLE IF NOT EXISTS 'history' ("
				"   'id'               INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,"
				"   'clipboard_id'     INTEGER NOT NULL,"
				"   'options'          BLOB DEFAULT NULL,"
				"   'date'             TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
				"   CONSTRAINT fk_clipboard FOREIGN KEY (clipboard_id) REFERENCES clipboards (id) ON DELETE CASCADE"
				");"
				"CREATE TABLE IF NOT EXISTS 'flags' ("
				"   'id'               INTEGER NOT NULL,"
				"   CONSTRAINT fk_id FOREIGN KEY (id) REFERENCES history (id) ON DELETE CASCADE"
				");"
				"CREATE TABLE IF NOT EXISTS 'groups' ("
				"   'id'               INTEGER NOT NULL PRIMARY KEY,"
				"   'history_id'       INTEGER NOT NULL,"
				"   'string_id'        INTEGER NOT NULL,"
				"   CONSTRAINT fk_history FOREIGN KEY (history_id) REFERENCES history (id) ON DELETE CASCADE,"
				"   CONSTRAINT fk_string  FOREIGN KEY (string_id)  REFERENCES strings (id) ON DELETE CASCADE"
				");"
				"CREATE TABLE IF NOT EXISTS 'meta' ("
				"   'key'              TEXT NOT NULL,"
				"   'value'            TEXT NOT NULL,"
				"   UNIQUE (key)"
				");"
				"INSERT OR IGNORE INTO meta ('key', 'value') VALUES ('version', '1'),('uuid', :uuid)";

			// Remove superfluous whitespace to improve output of sqlite3’s ‘.schema’ command
			NSMutableString* pretty = [NSMutableString stringWithUTF8String:query];
			NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"(\\(| ) +" options:0 error:nil];
			[regex replaceMatchesInString:pretty options:0 range:NSMakeRange(0, pretty.length) withTemplate:@"$1"];

			RunSQLStatement(OakPasteboard.SQLDatabase, pretty.UTF8String, { { ":uuid", [NSUUID UUID].UUIDString } });
		}
	}
	return db;
}

+ (OakPasteboard*)pasteboardWithName:(NSString*)aName systemPasteboard:(NSPasteboard*)pboard avoidsDuplicates:(BOOL)flag
{
	static NSMutableDictionary<NSString*, OakPasteboard*>* sharedInstances = [NSMutableDictionary dictionary];
	if(!sharedInstances[aName])
	{
		OakPasteboard* res = [[OakPasteboard alloc] initWithName:aName systemPasteboard:pboard avoidsDuplicates:flag];
		sharedInstances[aName] = res;
		idle_callback().add(res);
	}
	return sharedInstances[aName];
}

+ (OakPasteboard*)generalPasteboard  { return [OakPasteboard pasteboardWithName:@"General" systemPasteboard:[NSPasteboard pasteboardWithName:NSGeneralPboard]  avoidsDuplicates:NO];  }
+ (OakPasteboard*)findPasteboard     { return [OakPasteboard pasteboardWithName:@"Find"    systemPasteboard:[NSPasteboard pasteboardWithName:NSFindPboard]     avoidsDuplicates:YES]; }
+ (OakPasteboard*)replacePasteboard  { return [OakPasteboard pasteboardWithName:@"Replace" systemPasteboard:[NSPasteboard pasteboardWithName:OakReplacePboard] avoidsDuplicates:YES]; }

- (instancetype)initWithName:(NSString*)aName systemPasteboard:(NSPasteboard*)pboard avoidsDuplicates:(BOOL)flag
{
	if(self = [self init])
	{
		_name             = aName;
		_pasteboard       = pboard;
		_avoidsDuplicates = flag;
	}
	return self;
}

- (void)ensurePasteboardItemIsInDatabase
{
	if([self.pasteboard availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]])
	{
		// Already in database, but check that historyId is valid
		NSDictionary* options = [self.pasteboard propertyListForType:OakPasteboardOptionsPboardType];
		if(NSNumber* historyId = options[@"historyId"])
		{
			char const* query = "SELECT id FROM history WHERE id = :history_id";
			if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":history_id", historyId } }).firstObject)
				return;
		}
	}

	// Do not add these types to database, see http://nspasteboard.org
	if([self.pasteboard availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
		return;

	NSArray<NSString*>* strings = [self.pasteboard readObjectsForClasses:@[ [NSString class] ] options:nil];
	if(strings.count == 0)
	{
		os_log_info(kLogPasteboard, "No strings on %{public}@ pasteboard. Available types: %{public}@", _name, self.pasteboard.types);
		return;
	}

	OakPasteboardEntry* lastEntry = self.lastEntry;
	if(lastEntry && [lastEntry.strings isEqual:strings])
		return;

	if(self.changeCount == self.pasteboard.changeCount)
		os_log_error(kLogPasteboard, "New content on %{public}@ pasteboard with stale change count (%lu): %{public}@", _name, self.pasteboard.changeCount, [strings componentsJoinedByString:@""]);

	[self addEntryWithStrings:strings options:nil];
}

- (void)updatePasteboardWithEntry:(OakPasteboardEntry*)pasteboardEntry
{
	self.changeCount = [self.pasteboard clearContents];

	if(pasteboardEntry)
	{
		[self.pasteboard writeObjects:pasteboardEntry.strings];
		[self.pasteboard setPropertyList:pasteboardEntry.options forType:OakPasteboardOptionsPboardType];
	}

	[NSNotificationCenter.defaultCenter postNotificationName:OakPasteboardDidChangeNotification object:self];
}

- (void)updatePasteboardWithEntries:(NSArray<OakPasteboardEntry*>*)pasteboardEntries
{
	NSArray<NSString*>* historyIds = [pasteboardEntries valueForKeyPath:@"historyId"];
	NSMutableDictionary* options = [NSMutableDictionary dictionary];
	options[@"historyIds"] = historyIds;

	NSArray<NSString*>* strings = [pasteboardEntries valueForKeyPath:@"@unionOfArrays.strings"];
	NSString* string;
	if([self isEqual:OakPasteboard.findPasteboard])
	{
		string = [strings componentsJoinedByString:@"|"];
		options[OakFindRegularExpressionOption] = @YES;
	}
	else
	{
		string = [strings componentsJoinedByString:@"\n"];
	}

	[self.pasteboard declareTypes:@[ NSPasteboardTypeString, OakPasteboardOptionsPboardType, @"org.nspasteboard.AutoGeneratedType" ] owner:nil];
	[self.pasteboard setString:string forType:NSPasteboardTypeString];
	[self.pasteboard setPropertyList:options forType:OakPasteboardOptionsPboardType];
	[self.pasteboard setPropertyList:@YES forType:@"org.nspasteboard.AutoGeneratedType"];

	self.changeCount = self.pasteboard.changeCount;

	[NSNotificationCenter.defaultCenter postNotificationName:OakPasteboardDidChangeNotification object:self];
}

- (OakPasteboardEntry*)currentEntry
{
	[self ensurePasteboardItemIsInDatabase];

	NSArray<NSString*>* strings = [self.pasteboard readObjectsForClasses:@[ [NSString class] ] options:nil];
	NSDictionary* options = [self.pasteboard propertyListForType:OakPasteboardOptionsPboardType];
	if(!options && [self.lastEntry.strings isEqual:strings])
		options = self.lastEntry.options;
	return strings.count ? [[OakPasteboardEntry alloc] initWithStrings:strings options:options flagged:NO] : nil;
}

- (OakPasteboardEntry*)fetchEntryWithHistoryId:(NSUInteger)historyId
{
	if(!historyId)
		return nil;

	char const* query = "SELECT options, flags.id AS flagged, string FROM history LEFT JOIN flags USING (id) LEFT JOIN groups ON history.id = history_id LEFT JOIN strings ON strings.id = string_id WHERE history.id = :history_id AND string IS NOT NULL;";
	NSArray* rows = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":history_id", @(historyId) } });
	NSArray* strings = [rows valueForKeyPath:@"string"];

	NSMutableDictionary* options = [NSMutableDictionary dictionaryWithObject:@(historyId) forKey:@"historyId"];
	if(NSData* optionsData = rows.firstObject[@"options"])
		[options addEntriesFromDictionary:[NSPropertyListSerialization propertyListWithData:optionsData options:NSPropertyListImmutable format:nil error:nil]];

	return [[OakPasteboardEntry alloc] initWithStrings:strings options:options flagged:rows.firstObject[@"flagged"] ? YES : NO];
}

- (NSArray<OakPasteboardEntry*>*)entries
{
	NSMutableArray<OakPasteboardEntry*>* res = [NSMutableArray array];

	NSNumber* lastHistoryId;
	NSMutableArray* strings;

	char const* query = "SELECT history.id AS history_id, options, flags.id AS flagged, string FROM history LEFT JOIN clipboards ON clipboards.id = clipboard_id LEFT JOIN flags USING (id) LEFT JOIN groups ON history.id = history_id LEFT JOIN strings ON strings.id = string_id WHERE name = :name ORDER BY history.id DESC;";
	for(NSDictionary* row in RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name } }))
	{
		NSNumber* historyId = row[@"history_id"];
		if([lastHistoryId isEqual:historyId])
		{
			[strings addObject:row[@"string"]];
		}
		else
		{
			NSMutableDictionary* options = [NSMutableDictionary dictionaryWithObject:historyId forKey:@"historyId"];
			if(NSData* optionsData = row[@"options"])
				[options addEntriesFromDictionary:[NSPropertyListSerialization propertyListWithData:optionsData options:NSPropertyListImmutable format:nil error:nil]];
			strings = [NSMutableArray arrayWithObject:row[@"string"]];
			[res addObject:[[OakPasteboardEntry alloc] initWithStrings:strings options:options flagged:row[@"flagged"] ? YES : NO]];
		}
		lastHistoryId = historyId;
	}

	return res;
}

- (OakPasteboardEntry*)firstEntry
{
	char const* query = "SELECT MIN(history.id) AS history_id FROM history LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE name = :name;";
	if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name } }).firstObject)
		return [self fetchEntryWithHistoryId:[row[@"history_id"] integerValue]];
	return nil;
}

- (OakPasteboardEntry*)lastEntry
{
	char const* query = "SELECT MAX(history.id) AS history_id FROM history LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE name = :name;";
	if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name } }).firstObject)
		return [self fetchEntryWithHistoryId:[row[@"history_id"] integerValue]];
	return nil;
}

- (OakPasteboardEntry*)entryBefore:(OakPasteboardEntry*)laterEntry
{
	char const* query = "SELECT MAX(history.id) AS history_id FROM history LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE history.id < :history_id AND name = :name;";
	if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name }, { ":history_id", @(laterEntry.historyId) } }).firstObject)
		return [self fetchEntryWithHistoryId:[row[@"history_id"] integerValue]];
	return nil;
}

- (OakPasteboardEntry*)entryAfter:(OakPasteboardEntry*)earlierEntry
{
	char const* query = "SELECT MIN(history.id) AS history_id FROM history LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE history.id > :history_id AND name = :name;";
	if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name }, { ":history_id", @(earlierEntry.historyId) } }).firstObject)
		return [self fetchEntryWithHistoryId:[row[@"history_id"] integerValue]];
	return nil;
}

- (OakPasteboardEntry*)addEntryWithStrings:(NSArray<NSString*>*)someStrings options:(NSDictionary*)someOptions
{
	if(someStrings.count == 0)
	{
		os_log_error(kLogPasteboard, "Adding empty array in [%{public}@ addEntryWithStrings:options:updatePasteboard:]", [self class]);
		return nil;
	}

	NSArray* keys = [someOptions keysOfEntriesPassingTest:^(id key, id obj, BOOL* stop){ return BOOL(![obj isKindOfClass:[NSNumber class]] || [obj boolValue]); }].allObjects;
	NSMutableDictionary* options = keys.count ? [NSMutableDictionary dictionaryWithObjects:[someOptions objectsForKeys:keys notFoundMarker:@NO] forKeys:keys] : [NSMutableDictionary dictionary];

	NSMutableArray* stringIds = [NSMutableArray array];
	NSMutableArray* values = [NSMutableArray array];
	for(NSString* str in someStrings)
	{
		char const* query = "INSERT INTO strings ('string') VALUES (:string); SELECT id FROM strings WHERE string = :string;";
		if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":string", str } }).firstObject)
		{
			[stringIds addObject:row[@"id"]];
			[values addObject:[NSString stringWithFormat:@"((SELECT seq FROM sqlite_sequence WHERE name = 'history'), %@)", row[@"id"]]];
		}
	}

	std::map<std::string, id> variables = {
		{ ":name",    _name },
		{ ":options", options.count ? [NSPropertyListSerialization dataWithPropertyList:options format:NSPropertyListBinaryFormat_v1_0 options:0 error:nil] : nil },
	};

	BOOL isFlagged = NO;
	if(self.avoidsDuplicates)
	{
		NSString* query = [NSString stringWithFormat:@"SELECT COUNT(*) AS flagCount FROM (SELECT history_id, COUNT(*) AS count FROM history LEFT JOIN flags USING (id) LEFT JOIN groups ON history_id = history.id LEFT JOIN clipboards ON clipboard_id = clipboards.id LEFT JOIN strings ON strings.id = string_id WHERE name = :name AND string_id IN (%@) AND flags.id IS NOT NULL GROUP BY history_id HAVING count = %lu);", [stringIds componentsJoinedByString:@", "], stringIds.count];
		if(NSDictionary* res = RunSQLStatement(OakPasteboard.SQLDatabase, query.UTF8String, variables).firstObject)
			isFlagged = [res[@"flagCount"] integerValue] ? YES : NO;
	}

	char const* queryFormat =
		"BEGIN TRANSACTION;"
		"INSERT INTO clipboards ('name') VALUES (:name);"
		"%s"
		"INSERT INTO history ('options', 'clipboard_id') SELECT :options, id FROM clipboards WHERE name = :name;"
		"SELECT LAST_INSERT_ROWID() AS history_id;"
		"%s"
		"INSERT INTO groups ('history_id', 'string_id') VALUES %s;"
		"END TRANSACTION;"
		;

	NSString* deleteQuery = self.avoidsDuplicates ? [NSString stringWithFormat:@"DELETE FROM history WHERE id IN (SELECT history_id FROM (SELECT history_id, COUNT(*) AS count FROM groups LEFT JOIN history ON history_id = history.id LEFT JOIN clipboards ON clipboard_id = clipboards.id LEFT JOIN strings ON string_id = strings.id WHERE name = :name AND string_id IN (%@) GROUP BY history_id HAVING count = %lu));", [stringIds componentsJoinedByString:@", "], stringIds.count] : @"";
	NSString* flagQuery = isFlagged ? @"INSERT INTO flags (id) VALUES (LAST_INSERT_ROWID());" : @"";
	std::string query = text::format(queryFormat, deleteQuery.UTF8String, flagQuery.UTF8String, [values componentsJoinedByString:@","].UTF8String);

	if(NSDictionary* res = RunSQLStatement(OakPasteboard.SQLDatabase, query.c_str(), variables).firstObject)
		options[@"historyId"] = res[@"history_id"];

	[self pruneHistory:self];

	return [[OakPasteboardEntry alloc] initWithStrings:someStrings options:options flagged:NO];
}

- (void)addEntryWithString:(NSString*)aString options:(NSDictionary*)someOptions
{
	if(OakPasteboardEntry* entry = [self addEntryWithStrings:@[ aString ] options:someOptions])
		[self updatePasteboardWithEntry:entry];
}

- (void)addEntryWithString:(NSString*)aString
{
	[self addEntryWithString:aString options:nil];
}

// ====================
// = Removing Entries =
// ====================

- (void)removeEntries:(NSArray<OakPasteboardEntry*>*)pasteboardEntries
{
	if(pasteboardEntries.count)
	{
		NSArray* historyIds = [pasteboardEntries valueForKeyPath:@"historyId"];
		NSString* query = [NSString stringWithFormat:@"DELETE FROM history WHERE id IN (%@);", [historyIds componentsJoinedByString:@", "]];
		RunSQLStatement(OakPasteboard.SQLDatabase, query.UTF8String);

		if([self.pasteboard availableTypeFromArray:@[ OakPasteboardOptionsPboardType ]])
		{
			NSDictionary* options = [self.pasteboard propertyListForType:OakPasteboardOptionsPboardType];
			if(NSNumber* historyId = options[@"historyId"])
			{
				if([historyIds containsObject:historyId])
				{
					NSArray<NSString*>* strings = [self.pasteboard readObjectsForClasses:@[ [NSString class] ] options:nil];
					NSMutableDictionary* mutableOptions = [options mutableCopy];
					[mutableOptions removeObjectForKey:@"historyId"];

					self.changeCount = [self.pasteboard clearContents];
					[self.pasteboard writeObjects:strings];
					[self.pasteboard setPropertyList:mutableOptions forType:OakPasteboardOptionsPboardType];
				}
			}
		}
	}
}

- (void)removeAllEntries
{
	char const* query = "DELETE FROM history WHERE clipboard_id = (SELECT id FROM clipboards WHERE name = :name) AND id NOT IN (SELECT id FROM flags);";
	RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name } });
}

- (void)checkForExternalPasteboardChanges
{
	// Do not touch clipboard unless we are active as CFPasteboardCopyData can stall
	// See https://lists.macromates.com/textmate/2019-August/041109.html
	if(!NSApp.isActive)
		return;

	if(self.changeCount != self.pasteboard.changeCount)
	{
		[self ensurePasteboardItemIsInDatabase];
		self.changeCount = self.pasteboard.changeCount;
		[NSNotificationCenter.defaultCenter postNotificationName:OakPasteboardDidChangeNotification object:self];
	}
}

- (void)pruneHistory:(id)sender
{
	NSInteger keepAtLeast = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsClipboardHistoryKeepAtLeast];
	NSInteger keepAtMost  = [NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsClipboardHistoryKeepAtMost];
	CGFloat daysToKeep    = [NSUserDefaults.standardUserDefaults floatForKey:kUserDefaultsClipboardHistoryDaysToKeep];

	if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, "SELECT COUNT(*) AS count FROM history LEFT JOIN flags USING (id) LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE flags.id IS NULL AND name = :name", { { ":name", _name }}).firstObject)
	{
		NSUInteger count = [row[@"count"] integerValue];

		NSDateFormatter* dateFormatter = [[NSDateFormatter alloc] init];
		dateFormatter.dateFormat = @"YYYY-MM-dd HH:mm:ss";
		dateFormatter.timeZone   = [NSTimeZone timeZoneForSecondsFromGMT:0];
		NSString* keepUntil = [NSString stringWithFormat:@"\"%@\"", [dateFormatter stringFromDate:[NSDate dateWithTimeIntervalSinceNow:-daysToKeep*24*60*60]]];

		if(keepAtLeast && keepAtLeast <= count)
		{
			char const* query = "SELECT date FROM history LEFT JOIN flags USING (id) LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE flags.id IS NULL AND name = :name ORDER BY history.id LIMIT :offset, 1;";
			if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name }, { ":offset", @(count - keepAtLeast) } }).firstObject)
				keepUntil = [NSString stringWithFormat:@"MIN(\"%@\", %@)", row[@"date"], keepUntil];
		}

		if(keepAtMost && keepAtMost <= count)
		{
			char const* query = "SELECT date FROM history LEFT JOIN flags USING (id) LEFT JOIN clipboards ON clipboards.id = clipboard_id WHERE flags.id IS NULL AND name = :name ORDER BY history.id LIMIT :offset, 1;";
			if(NSDictionary* row = RunSQLStatement(OakPasteboard.SQLDatabase, query, { { ":name", _name }, { ":offset", @(count - keepAtMost) } }).firstObject)
				keepUntil = [NSString stringWithFormat:@"MAX(\"%@\", %@)", row[@"date"], keepUntil];
		}

		char const* queryFormat =
			"SELECT COUNT(*) AS count FROM history LEFT JOIN flags USING (id) WHERE date < %1$s AND flags.id IS NULL AND clipboard_id = (SELECT id FROM clipboards WHERE name = :name);"
			"DELETE FROM history WHERE id IN (SELECT history.id FROM history LEFT JOIN flags USING (id) WHERE date < %1$s AND flags.id IS NULL AND clipboard_id = (SELECT id FROM clipboards WHERE name = :name));"
			"SELECT COUNT(*) AS count FROM strings LEFT JOIN groups ON string_id = strings.id WHERE string_id IS NULL;"
			"DELETE FROM strings WHERE id IN (SELECT strings.id FROM strings LEFT JOIN groups ON string_id = strings.id WHERE string_id IS NULL);"
			;

		std::string query = text::format(queryFormat, keepUntil.UTF8String);
		if(NSArray* rows = RunSQLStatement(OakPasteboard.SQLDatabase, query.c_str(), { { ":name", _name } }))
		{
			if(os_log_info_enabled(kLogSQLite))
			{
				NSNumber* deletedItemsCount   = rows[0][0][@"count"];
				NSNumber* deletedStringsCount = rows[1][0][@"count"];
				if(deletedItemsCount.integerValue || deletedStringsCount.integerValue)
				{
					os_log_info(kLogSQLite, "There are a total of %lu %{public}@ pasteboard items, we must keep at least/most %lu/%lu items", count, _name, keepAtLeast, keepAtMost);
					os_log_info(kLogSQLite, "Deleted %{public}@ %{public}@ pasteboard item(s) and garbage collected %{public}@ string(s)", deletedItemsCount, _name, deletedStringsCount);
				}
			}
		}
	}
}

- (OakPasteboardEntry*)previous
{
	OakPasteboardEntry* entry = [self entryBefore:self.currentEntry] ?: self.firstEntry ?: self.currentEntry;
	[self updatePasteboardWithEntry:entry];
	return entry;
}

- (OakPasteboardEntry*)current
{
	return self.currentEntry;
}

- (OakPasteboardEntry*)next
{
	OakPasteboardEntry* entry = [self entryAfter:self.currentEntry] ?: self.lastEntry ?: self.currentEntry;
	[self updatePasteboardWithEntry:entry];
	return entry;
}

- (void)selectItemAtPosition:(NSPoint)location withWidth:(CGFloat)width respondToSingleClick:(BOOL)singleClick
{
	NSArray<OakPasteboardEntry*>* entries = self.entries;

	NSUInteger selectedRow = self.currentEntry ? [entries indexOfObject:self.currentEntry] : 0;
	OakPasteboardSelector* pasteboardSelector = OakPasteboardSelector.sharedInstance;
	[pasteboardSelector setEntries:entries];
	[pasteboardSelector setIndex:selectedRow == NSNotFound ? 0 : selectedRow];
	if(width)
		[pasteboardSelector setWidth:width];
	if(singleClick)
		[pasteboardSelector setPerformsActionOnSingleClick];

	NSInteger newSelection = [pasteboardSelector showAtLocation:location];
	NSArray* newEntries = [pasteboardSelector entries];

	NSMutableArray* remove = [NSMutableArray array];
	NSSet* keep = [NSSet setWithArray:newEntries];
	for(OakPasteboardEntry* entry in entries)
	{
		if(![keep containsObject:entry])
			[remove addObject:entry];
	}
	[self removeEntries:remove];

	if(newSelection != -1)
		[self updatePasteboardWithEntry:[newEntries objectAtIndex:newSelection]];
}

- (void)selectItemForControl:(NSView*)controlView
{
	NSPoint origin = [controlView.window convertRectToScreen:[controlView convertRect:controlView.bounds toView:nil]].origin;
	[self selectItemAtPosition:origin withWidth:NSWidth(controlView.frame) respondToSingleClick:YES];
}
@end
