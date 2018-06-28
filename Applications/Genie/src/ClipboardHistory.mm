#import <sqlite3.h> 
#import <os/log.h>
#import <ns/ns.h>
#import <oak/debug.h>
#import "ClipboardHistory.h"
#import <GenieManager/GenieUserDefaults.h>

static BOOL RunSQLStatement (NSString* dbPath, char const* query, std::map<std::string, id> const& variables)
{
	BOOL res = NO;

	sqlite3* db = nullptr;
	if(sqlite3_open([[dbPath stringByExpandingTildeInPath] fileSystemRepresentation], &db) == SQLITE_OK)
	{
		sqlite3_busy_timeout(db, 250);

		res = YES;
		while(*query && res)
		{
			sqlite3_stmt* stmt = nullptr;
			if(sqlite3_prepare_v2(db, query, -1, &stmt, &query) == SQLITE_OK)
			{
				for(int i = 0; i < sqlite3_bind_parameter_count(stmt); ++i)
				{
					auto it = variables.find(sqlite3_bind_parameter_name(stmt, i+1));
					if(it != variables.end())
					{
						id value = it->second;
						if([value isKindOfClass:[NSString class]])
						{
							sqlite3_bind_text(stmt, i+1, [value UTF8String], -1, nullptr);
						}
						else if([value isKindOfClass:[NSNull class]])
						{
							sqlite3_bind_null(stmt, i+1);
						}
						else if([value isKindOfClass:[NSNumber class]])
						{
							static std::map<std::string, std::function<int(sqlite3_stmt*, int, id)>> const typeMapping = {
								{ @encode(BOOL),               [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.boolValue ? 1 : 0);       } },
								{ @encode(char),               [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.charValue);               } },
								{ @encode(unsigned char),      [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.unsignedCharValue);       } },
								{ @encode(short),              [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.shortValue);              } },
								{ @encode(unsigned short),     [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.unsignedShortValue);      } },
								{ @encode(int),                [](sqlite3_stmt* stmt, int i, NSNumber* value) -> int { return sqlite3_bind_int(stmt, i, value.intValue);                } },
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
						os_log_error(OS_LOG_DEFAULT, "sqlite3: no variable for binding: ‘%{public}s’", sqlite3_bind_parameter_name(stmt, i+1));
					}
				}

				if(sqlite3_step(stmt) != SQLITE_DONE || sqlite3_finalize(stmt) != SQLITE_OK)
					res = NO;
			}
			else
			{
				res = NO;
			}
		}
	}

	sqlite3_close(db);

	if(res == NO)
		os_log_error(OS_LOG_DEFAULT, "sqlite3: error running query: %{public}s", sqlite3_errmsg(db));

	return res;
}

@interface ClipboardHistory ()
{
	id _eventMonitor;
	NSUInteger _clipboardChangeCount;
}
@end

@implementation ClipboardHistory
+ (instancetype)sharedInstance
{
	static ClipboardHistory* sharedInstance = [self new];
	return sharedInstance;
}

- (BOOL)potentialClipboardChangeEvent:(NSEvent*)anEvent
{
	if(anEvent.type == NSEventTypeKeyDown)
	{
		std::string eventString = to_s(anEvent);
		return eventString == "@x" || eventString == "@c" || eventString == "@v";
	}
	return anEvent.type == NSEventTypeLeftMouseUp || anEvent.type == NSEventTypeRightMouseUp || anEvent.type == NSEventTypeOtherMouseUp;
}

- (BOOL)trySetEnabled:(BOOL)flag
{
	if(flag == YES)
	{
		NSDictionary* opts = @{ (__bridge id)kAXTrustedCheckOptionPrompt: @YES };
		bool runningInTerminal = isatty(STDOUT_FILENO) && !AXIsProcessTrusted();
		if(runningInTerminal || !AXIsProcessTrustedWithOptions((__bridge CFDictionaryRef)opts))
			return NO;
	}
	self.enabled = flag;
	return YES;
}

- (void)setEnabled:(BOOL)flag
{
	if(_enabled == flag)
		return;

	_enabled = flag;
	if(flag == NO)
	{
		if(_eventMonitor)
			[NSEvent removeMonitor:_eventMonitor];
		_eventMonitor = nil;
	}
	else
	{
		_clipboardChangeCount = [NSPasteboard generalPasteboard].changeCount;
		_eventMonitor = [NSEvent addGlobalMonitorForEventsMatchingMask:NSEventMaskKeyDown|NSEventMaskLeftMouseUp|NSEventMaskRightMouseUp|NSEventMaskOtherMouseUp handler:^(NSEvent* event){
			if([self potentialClipboardChangeEvent:event])
			{
				NSRunningApplication* currentApp = [[NSWorkspace sharedWorkspace] frontmostApplication];
				dispatch_after(dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_SEC/2), dispatch_get_main_queue(), ^{
					NSPasteboard* pasteboard = [NSPasteboard generalPasteboard];
					NSInteger changeCount = pasteboard.changeCount;
					if(_clipboardChangeCount != changeCount)
					{
						os_log_info(OS_LOG_DEFAULT, "new clipboard change count: %ld → %ld: %{public}@", _clipboardChangeCount, changeCount, currentApp.bundleIdentifier);
						_clipboardChangeCount = changeCount;

						NSArray* ignoredApps = [[NSUserDefaults standardUserDefaults] arrayForKey:kClipboardHistoryIgnoreAppsSettingsKey];
						ignoredApps = [ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES && bundleIdentifier == %@", currentApp.bundleIdentifier]];
						if(ignoredApps.count)
						{
							os_log_info(OS_LOG_DEFAULT, "ignore change from %{public}@", currentApp.bundleIdentifier);
							return;
						}

						if(![pasteboard availableTypeFromArray:@[ @"org.nspasteboard.TransientType", @"org.nspasteboard.ConcealedType", @"org.nspasteboard.AutoGeneratedType" ]])
						{
							if(NSString* textClipping = [pasteboard availableTypeFromArray:@[ NSStringPboardType ]] ? [pasteboard stringForType:NSStringPboardType] : nil)
							{
								static NSRegularExpression* const regex = [NSRegularExpression regularExpressionWithPattern:@"\\A\\d+ (year|month|day|hour|minute|second)s?\\z" options:NSRegularExpressionCaseInsensitive error:nil];

								NSString* expireAfter = [[NSUserDefaults standardUserDefaults] stringForKey:kClipboardHistoryExpireAfterSettingsKey];
								if(!expireAfter || NSNotFound == [regex rangeOfFirstMatchInString:expireAfter options:0 range:NSMakeRange(0, expireAfter.length)].location)
									expireAfter = @"24 hours";
								expireAfter = [@"-" stringByAppendingString:expireAfter];
								os_log_info(OS_LOG_DEFAULT, "expire clipboard history after %{public}@", expireAfter);

								char const* query =
									"CREATE TABLE IF NOT EXISTS 'applications' ("
									"   'id'               INTEGER PRIMARY KEY AUTOINCREMENT,"
									"   'name'             TEXT,"
									"   'identifier'       TEXT NOT NULL,"
									"   'exclude'          INTEGER NOT NULL DEFAULT 0,"
									"   UNIQUE (identifier) ON CONFLICT IGNORE"
									");"
									"CREATE TABLE IF NOT EXISTS 'clippings' ("
									"   'id'               INTEGER PRIMARY KEY AUTOINCREMENT,"
									"   'clipping'         TEXT NOT NULL,"
									"   UNIQUE (clipping) ON CONFLICT IGNORE"
									");"
									"CREATE TABLE IF NOT EXISTS 'history' ("
									"   'id'               INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,"
									"   'clipping_id'      INTEGER NOT NULL REFERENCES clippings (id),"
									"   'application_id'   INTEGER NOT NULL REFERENCES applications (id),"
									"   'date'             TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP"
									");"
									"BEGIN TRANSACTION;"
									"DELETE FROM history WHERE date < DATETIME('now', :expireAfter);"
									"DELETE FROM clippings WHERE id IN (SELECT clippings.id FROM clippings LEFT JOIN history ON clipping_id = clippings.id WHERE clipping_id IS NULL);"
									"INSERT INTO applications ('name', 'identifier') VALUES (:name, :identifier);"
									"INSERT INTO clippings ('clipping') VALUES (:clipping);"
									"INSERT INTO history ('clipping_id', 'application_id')"
									" SELECT clippings.id, applications.id"
									" FROM 'clippings' JOIN 'applications'"
									" WHERE clipping = :clipping AND identifier = :identifier;"
									"END TRANSACTION;";

								std::map<std::string, id> variables = {
									{ ":name",        currentApp.localizedName },
									{ ":identifier",  currentApp.bundleIdentifier },
									{ ":clipping",    textClipping },
									{ ":expireAfter", expireAfter },
								};

								RunSQLStatement(GenieClipboardHistoryPath, query, variables);
							}
						}
					}
				});
			}
		}];
	}
}
@end