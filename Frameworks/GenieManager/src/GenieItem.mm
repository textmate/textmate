#import "GenieItem.h"
#import "GenieManager.h"
#import "GenieLRUDatabase.h"
#import "Utilities.h"
#import "StringImage.h"
#import "GenieTask.h"
#import "hash.h"
#import <OakFoundation/OakFoundation.h>
#import <ns/ns.h>
#import <plist/plist.h>
#import <plist/ascii.h>
#import <regexp/format_string.h>
#import <os/log.h>
#import <sqlite3.h> 

// ================
// = Genie Filter =
// ================

@interface GenieFilter ()
@property (nonatomic, readwrite) NSString* string;
@property (nonatomic, readwrite) NSString* normalizedString;
@property (nonatomic, readwrite) NSString* filterString;
@property (nonatomic, readwrite) NSString* queryString;
@end

@implementation GenieFilter
+ (GenieFilter*)filterWithString:(NSString*)aString
{
	return [[self alloc] initWithString:aString];
}

- (instancetype)initWithString:(NSString*)aString
{
	if(self = [super init])
	{
		_string           = aString;
		_normalizedString = [[aString lowercaseString] decomposedStringWithCanonicalMapping];

		NSRange spaceRange = [_string rangeOfString:@" " options:NSLiteralSearch];
		if(spaceRange.location == NSNotFound)
		{
			_filterString = _normalizedString;
			_queryString  = nil;
		}
		else
		{
			_filterString = [[[_string substringToIndex:spaceRange.location] lowercaseString] decomposedStringWithCanonicalMapping];
			_queryString  = [_string substringFromIndex:NSMaxRange(spaceRange)];
		}

		_string           = OakIsEmptyString(_string)           ? nil : _string;
		_normalizedString = OakIsEmptyString(_normalizedString) ? nil : _normalizedString;
		_filterString     = OakIsEmptyString(_filterString)     ? nil : _filterString;
		_queryString      = OakIsEmptyString(_queryString)      ? nil : _queryString;
	}
	return self;
}
@end

// ====================================
// = Wrapper for use with NSPredicate =
// ====================================

@interface GenieWrapper : NSObject
@property (nonatomic) id values;
- (instancetype)initWithValues:(id)someValues;
@end

@implementation GenieWrapper
- (instancetype)initWithValues:(id)someValues;
{
	if(self = [super init])
		_values = someValues;
	return self;
}

- (id)valueForKey:(NSString*)aKey
{
	id res = [_values performSelector:@selector(staticValueForKey:) withObject:aKey];
	return res ?: [super valueForKey:aKey];
}

- (id)valueForUndefinedKey:(NSString*)aKey
{
	if([aKey isEqualToString:NSMetadataItemContentTypeKey])
	{
		BOOL isURL = [self valueForKey:@"url"] != nil;
		if(isURL)
			return (NSString*)kUTTypeURL;
	}
	return nil;
}
@end

// =====================
// = Data Source Cache =
// =====================

static NSTimeInterval ParseDuration (id value)
{
	NSTimeInterval res = [value doubleValue];
	if([value isKindOfClass:[NSString class]] && [value length] > 1)
	{
		NSString* str = value;
		NSTimeInterval multiply = 1;
		switch([str characterAtIndex:str.length-1])
		{
			case 's': multiply = 1;          break;
			case 'm': multiply = 60;         break;
			case 'h': multiply = 60*60;      break;
			case 'd': multiply = 24*60*60;   break;
			case 'w': multiply = 7*24*60*60; break;
		}
		res = res * multiply;
	}
	return res;
}

NSString* GenieDataSourceCacheRecordDidExpireNotification = @"GenieDataSourceCacheRecordDidExpireNotification";
void* kRunningApplicationsBindings = &kRunningApplicationsBindings;

@interface GenieDataSourceCacheRecord : NSObject
{
	NSTimer* _expirationTimer;
	dispatch_source_t _dependsOnDispatchSource;
	NSTimer* _dependsOnTimer;
}
@property (nonatomic) NSArray* items;
@property (nonatomic) NSDate* creationDate;
@property (nonatomic) NSDate* expirationDate;
@property (nonatomic) NSString* dependsOnPath;
@property (nonatomic) NSString* digest;
@property (nonatomic) NSString* title;
@property (nonatomic) BOOL forceExpired;
@property (nonatomic) BOOL disablePersistence;
@property (nonatomic, getter = isExpired) BOOL expired;
@property (nonatomic) BOOL watchDependencies;
- (void)checkExpired;
@end

@implementation GenieDataSourceCacheRecord
- (instancetype)initWithDigest:(NSString*)aDigest
{
	if(self = [super init])
	{
		_creationDate = [NSDate date];
		_digest       = aDigest;
	}
	return self;
}

- (instancetype)initWithDictionary:(NSDictionary*)plist
{
	if(self = [self init])
	{
		_items          = plist[@"items"];
		_creationDate   = plist[@"creationDate"];
		_expirationDate = plist[@"expirationDate"];
		_dependsOnPath  = plist[@"dependsOnPath"];
		_digest         = plist[@"digest"];
		_title          = plist[@"description"];
		_expired        = [plist[@"expired"] boolValue];

		if(!_expired)
			[self checkExpired];
	}
	return self;
}

- (void)dealloc
{
	self.watchDependencies = NO;
}

- (BOOL)writeToFile:(NSString*)aPath
{
	NSMutableDictionary* plist = [NSMutableDictionary dictionary];
	plist[@"items"]          = _items;
	plist[@"creationDate"]   = _creationDate;
	plist[@"expirationDate"] = _expirationDate;
	plist[@"dependsOnPath"]  = _dependsOnPath;
	plist[@"digest"]         = _digest;
	plist[@"description"]    = _title;
	plist[@"expired"]        = _expired ? @YES : nil;

	if(NSData* data = [NSPropertyListSerialization dataWithPropertyList:plist format:NSPropertyListBinaryFormat_v1_0 options:0 error:nil])
		return [data writeToFile:aPath atomically:YES];
	return NO;
}

- (void)setExpired:(BOOL)flag
{
	if(_expired == flag)
		return;

	if(_expired = flag)
		[[NSNotificationCenter defaultCenter] postNotificationName:GenieDataSourceCacheRecordDidExpireNotification object:self];
}

- (void)setDigest:(NSString*)newDigest
{
	if(_digest == newDigest || [_digest isEqualToString:newDigest])
		return;
	_digest = newDigest;
	self.expired = YES;
}

- (void)checkExpired
{
	BOOL res = NO;
	if(_dependsOnPath)
	{
		if(NSDictionary* attrs = [NSFileManager.defaultManager attributesOfItemAtPath:_dependsOnPath error:nullptr])
		{
			if(NSDate* modificationDate = [attrs fileModificationDate])
				res = [_creationDate timeIntervalSinceDate:modificationDate] < 0;
		}
	}

	if(_expirationDate && [_expirationDate timeIntervalSinceNow] < 0)
		res = YES;

	if(res && !_expired)
		self.expired = YES;
}

- (void)cacheRecordDidExpire:(id)sender
{
	self.watchDependencies = NO;
	self.expired = YES;
}

- (void)setDependsOnDidChange:(BOOL)flag
{
	if(_dependsOnTimer)
		[_dependsOnTimer invalidate];
	_dependsOnTimer = flag ? [NSTimer scheduledTimerWithTimeInterval:0.1 target:self selector:@selector(dependsOnTimerDidFire:) userInfo:nil repeats:NO] : nil;
}

- (void)dependsOnTimerDidFire:(NSTimer*)aTimer
{
	[self cacheRecordDidExpire:nil];
}

- (void)setWatchDependencies:(BOOL)flag
{
	if(_watchDependencies == flag)
		return;

	_watchDependencies = flag;
	if(_watchDependencies && !_expired)
	{
		if(_expirationDate)
		{
			NSTimeInterval delay = MAX(0.1, [_expirationDate timeIntervalSinceNow]);
			_expirationTimer = [NSTimer scheduledTimerWithTimeInterval:delay target:self selector:@selector(cacheRecordDidExpire:) userInfo:nil repeats:NO];
		}

		if(_dependsOnPath)
		{
			int fd = open(_dependsOnPath.fileSystemRepresentation, O_EVTONLY|O_CLOEXEC);
			if(fd != -1)
			{
				_dependsOnDispatchSource = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, fd, DISPATCH_VNODE_DELETE|DISPATCH_VNODE_WRITE|DISPATCH_VNODE_EXTEND|DISPATCH_VNODE_RENAME|DISPATCH_VNODE_ATTRIB|DISPATCH_VNODE_REVOKE, dispatch_get_main_queue());
				dispatch_source_set_cancel_handler(_dependsOnDispatchSource, ^{
					close(fd);
				});

				__weak GenieDataSourceCacheRecord* _self = self;
				dispatch_source_set_event_handler(_dependsOnDispatchSource, ^{
					[_self setDependsOnDidChange:YES];
				});

				dispatch_resume(_dependsOnDispatchSource);
			}
			else
			{
				os_log_error(OS_LOG_DEFAULT, "Failed to observe %{public}@: %{errno}d", _dependsOnPath, errno);
			}
		}
	}
	else
	{
		[_expirationTimer invalidate];
		_expirationTimer = nil;

		if(_dependsOnDispatchSource)
			dispatch_source_cancel(_dependsOnDispatchSource);
		_dependsOnDispatchSource = nullptr;

		if(_dependsOnTimer)
			[_dependsOnTimer invalidate];
		_dependsOnTimer = nil;
	}
}
@end

@interface GenieDataSourceCache : NSObject
{
	NSString* _path;
	NSMutableDictionary<NSString*, GenieDataSourceCacheRecord*>* _contents;
	NSMutableSet* _dirty;

	BOOL _needsSaveCache;
	NSTimer* _saveCacheTimer;
}
+ (instancetype)sharedInstance;
- (GenieDataSourceCacheRecord*)resultForKey:(NSString*)key;
- (void)setResult:(GenieDataSourceCacheRecord*)aRecord forKey:(NSString*)key;
@end

@implementation GenieDataSourceCache
+ (instancetype)sharedInstance
{
	static GenieDataSourceCache* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_path     = [GenieManager.sharedInstance cacheFolderByAppendingPathComponent:@"DataSource"];
		_contents = [NSMutableDictionary dictionary];
		_dirty    = [NSMutableSet set];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:NSApp];
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)aNotification
{
	[self synchronize];
}

- (void)synchronize
{
	if(_needsSaveCache)
	{
		for(NSString* key in _dirty)
			[_contents[key] writeToFile:[[_path stringByAppendingPathComponent:hash(key)] stringByAppendingPathExtension:@"plist"]];
		_dirty = [NSMutableSet set];
	}
	self.needsSaveCache = NO;
}

- (void)setResult:(GenieDataSourceCacheRecord*)aResult forKey:(NSString*)key
{
	if(aResult)
	{
		_contents[key] = aResult;
		if(aResult.disablePersistence == NO)
		{
			[_dirty addObject:key];
			self.needsSaveCache = YES;
		}
	}
	else
	{
		[_contents removeObjectForKey:key];
		NSString* path = [[_path stringByAppendingPathComponent:hash(key)] stringByAppendingPathExtension:@"plist"];
		[NSFileManager.defaultManager removeItemAtPath:path error:nil];
	}
}

- (GenieDataSourceCacheRecord*)resultForKey:(NSString*)key
{
	if(key && !_contents[key])
	{
		NSString* path = [[_path stringByAppendingPathComponent:hash(key)] stringByAppendingPathExtension:@"plist"];
		if(NSDictionary* plist = [NSDictionary dictionaryWithContentsOfFile:path])
			_contents[key] = [[GenieDataSourceCacheRecord alloc] initWithDictionary:plist];
	}
	return key ? _contents[key] : nil;
}

- (void)setNeedsSaveCache:(BOOL)flag
{
	if(_saveCacheTimer)
	{
		[_saveCacheTimer invalidate];
		_saveCacheTimer = nil;
	}

	if(_needsSaveCache = flag)
		_saveCacheTimer = [NSTimer scheduledTimerWithTimeInterval:5 target:self selector:@selector(saveCacheTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)saveCacheTimerDidFire:(NSTimer*)aTimer
{
	[self synchronize];
}
@end

// =====================

@interface GenieItemMetadata : GenieItem
{
	__weak GenieItem* _dataSource;
	GenieItem* _templateItem;
}
@property (nonatomic, readonly) NSMetadataItem* metadataItem;
@property (nonatomic) NSString* metadataItemDisplayName;
@property (nonatomic, readonly) NSString* nameWithVersion;
@property (nonatomic, readonly) NSString* nameWithoutVersion;
@end

@interface GenieItemDictionary : GenieItem
{
	__weak GenieItem* _dataSource;
	GenieItem* _templateItem;
}
@end

@interface GenieItem ()
{
	NSMutableDictionary* _values;
	NSString* _match;

	NSMutableDictionary<NSString*, id>*            _cachedValues;
	NSMutableDictionary<NSString*, NSMutableSet*>* _valuesDependingOnKeys;

	BOOL _updatingHTML;
	BOOL _pendingUpdateHTML;

	NSMetadataQuery* _metadataQuery;

	// =============================
	// = Used by Genie Preferences =
	// =============================

	NSMutableArray* _mdScope;
	NSMutableArray* _mutableScriptArguments;
	NSMutableArray* _mutableSqlBindings;
}
@property (nonatomic) GenieDataSourceCacheRecord* dataSourceCacheRecord;
@property (nonatomic) NSArray<__kindof GenieItem*>* dataSourceResults;
@property (nonatomic) BOOL updating;
@property (nonatomic) BOOL dependsOnRunningApplications;
@property (nonatomic, readwrite) BOOL disableLRUOrdering;
- (instancetype)initWithIdentifier:(NSString*)anIdentifier parentItem:(GenieItem*)parentItem directory:(NSString*)directory;
- (void)updateMetadataDisplayNameForItem:(GenieItemMetadata*)genieItem;
@end

@implementation GenieItemMetadata
- (instancetype)initWithValues:(NSMetadataItem*)metadataItem dataSource:(GenieItem*)dataSource templateItem:(GenieItem*)templateItem parentItem:(GenieItem*)parentItem directory:(NSString*)directory
{
	NSString* identifier = [metadataItem valueForAttribute:NSMetadataItemPathKey] ?: [[NSUUID UUID] UUIDString];
	_templateItem = templateItem;

	if(self = [self initWithIdentifier:identifier parentItem:parentItem directory:directory])
	{
		_metadataItem = metadataItem;
		_dataSource   = dataSource;
	}
	return self;
}

- (NSString*)identifierWithContext
{
	NSString* dataSource = _dataSource.identifier ?: _dataSource.title;
	NSString* identifier = self.identifier ?: self.title;
	return [NSString stringWithFormat:@"%@\034%@", dataSource, identifier];
}

- (NSArray*)allKeys
{
	return _metadataItem.attributes;
}

- (id)staticValueForKey:(NSString*)aKey
{
	if([aKey isEqualToString:NSMetadataItemDisplayNameKey])
	{
		if(!_metadataItemDisplayName)
			[_dataSource updateMetadataDisplayNameForItem:self];
		return _metadataItemDisplayName;
	}

	id value = [_metadataItem valueForAttribute:aKey];
	if(!value && ![@[ @"uid", @"isTemplate" ] containsObject:aKey])
		value = [_templateItem staticValueForKey:aKey];
	return value;
}

- (NSString*)nameWithVersion
{
	NSString* name    = [_metadataItem valueForAttribute:NSMetadataItemDisplayNameKey];
	NSString* version = [_metadataItem valueForAttribute:NSMetadataItemVersionKey];
	return name && version ? [NSString stringWithFormat:@"%@ (%@)", name, version] : nil;
}

- (NSString*)nameWithoutVersion
{
	return [_metadataItem valueForAttribute:NSMetadataItemDisplayNameKey] ?: @"(untitled)";
}
@end

@implementation GenieItemDictionary
- (instancetype)initWithValues:(NSDictionary*)someValues dataSource:(GenieItem*)dataSource templateItem:(GenieItem*)templateItem parentItem:(GenieItem*)parentItem directory:(NSString*)directory
{
	_templateItem = templateItem;
	if(self = [super initWithValues:someValues parentItem:parentItem directory:directory])
	{
		_dataSource = dataSource;
	}
	return self;
}

- (NSString*)identifierWithContext
{
	NSString* dataSource = _dataSource.identifier ?: _dataSource.title;
	NSString* identifier = self.identifier ?: self.title;
	return [NSString stringWithFormat:@"%@\034%@", dataSource, identifier];
}

- (id)staticValueForKey:(NSString*)aKey
{
	id value = [super staticValueForKey:aKey];
	if(!value && ![@[ @"uid", @"isTemplate" ] containsObject:aKey])
		value = [_templateItem staticValueForKey:aKey];
	return value;
}
@end

// =================
// = GenieHTMLItem =
// =================

@implementation GenieHTMLItem
- (id)initWithOriginalItem:(GenieItem*)originalItem
{
	if(self = [super init])
	{
		_originalItem = originalItem;
		_readOnly = YES;
	}
	return self;
}
@end

// =============
// = GenieItem =
// =============

static std::map<GenieItemKind, NSString*> KindMapping = {
	{ kGenieItemKindWebAddress,      @"web"             },
	{ kGenieItemKindRunScript,       @"script"          },
	{ kGenieItemKindOpenFile,        @"file"            },
	{ kGenieItemKindSpotlight,       @"spotlight"       },
	{ kGenieItemKindSqlite,          @"sqlite"          },
	{ kGenieItemKindCommandResult,   @"exec"            },
	{ kGenieItemKindRecentDocuments, @"recentDocuments" },
	{ kGenieItemKindPredicateGroup,  @"predicateGroup"  },
};

@implementation GenieItem
+ (NSSet*)keyPathsForValuesAffectingBusy
{
	return [NSSet setWithArray:@[ @"updating" ]];
}

+ (NSSet*)keyPathsForValuesAffectingReplacementItems
{
	return [NSSet setWithArray:@[ @"dataSourceResults" ]];
}

+ (void)expireItemsForIdentifier:(NSString*)identifier
{
	if(GenieDataSourceCacheRecord* record = [GenieDataSourceCache.sharedInstance resultForKey:identifier])
		record.expired = YES;
}

+ (NSString*)stringFromKind:(GenieItemKind)aKind
{
	auto it = KindMapping.find(aKind);
	return it != KindMapping.end() ? it->second : nil;
}

+ (GenieItemKind)kindFromString:(NSString*)aString
{
	for(auto const& pair : KindMapping)
	{
		if([pair.second isEqualToString:aString])
			return pair.first;
	}
	return kGenieItemKindUnused;
}

- (instancetype)initWithIdentifier:(NSString*)anIdentifier parentItem:(GenieItem*)parentItem directory:(NSString*)directory
{
	if(self = [super init])
	{
		_cachedValues = [NSMutableDictionary dictionary];

		_identifier = anIdentifier;
		_parentItem = parentItem;
		_directory  = directory;

		_disabled = [[self staticValueForKey:@"disabled"] boolValue];
		_readOnly = [[self staticValueForKey:@"readOnly"] boolValue];
		_fallback = [[self staticValueForKey:@"match"] isEqualToString:@"*"];

		if(_hasHTMLOutput = [[self staticValueForKey:@"output"] isEqualToString:@"html"])
		{
			_htmlOutputItem = [[GenieHTMLItem alloc] initWithOriginalItem:self];
			_htmlOutputItem.queryString = self.queryString;
		}

		BOOL acceptQuery = _fallback;
		for(NSString* key in @[ @"title", @"subtitle", @"file", @"url", @"value", @"scriptArguments", @"standardInputString" ])
		{
			if(acceptQuery)
				break;

			id value = [self staticValueForKey:key];
			if([value isKindOfClass:[NSString class]])
			{
				acceptQuery = [value containsString:@"${query"];
			}
			else if([value isKindOfClass:[NSArray class]])
			{
				for(id str in value)
					acceptQuery = [str containsString:@"${query"];
			}
		}
		_acceptsQuery = acceptQuery;

		_disableLRUOrdering   = [[self staticValueForKey:@"disableLRUOrdering"] boolValue];
		_disableLearning      = [[self staticValueForKey:@"disableLearning"] boolValue];
		_disableFuzzyMatching = [[self staticValueForKey:@"disableFuzzyMatching"] boolValue];
		_disableRankOrdering  = [[self staticValueForKey:@"disableRankOrdering"] boolValue];

		_kind = [GenieItem kindFromString:[self staticValueForKey:@"kind"]];
		if(_kind == kGenieItemKindUnused)
		{
			if([self staticValueForKey:@"script"] || [self staticValueForKey:@"exec"])
				_kind = kGenieItemKindRunScript;
			else if([self staticValueForKey:@"url"])
				_kind = kGenieItemKindWebAddress;
			else if([self staticValueForKey:@"file"])
				_kind = kGenieItemKindOpenFile;
			else if([self staticValueForKey:@"predicate"])
				_kind = kGenieItemKindPredicateGroup;
			else
				_kind = kGenieItemKindGroup;
		}

		NSMutableArray* children = [NSMutableArray array];
		for(NSDictionary* childValues in [self staticValueForKey:@"children"])
		{
			if(GenieItem* item = [[GenieItem alloc] initWithValues:childValues parentItem:self directory:directory])
			{
				if(_kind == kGenieItemKindPredicateGroup && !childValues[@"disableLRUOrdering"])
					item.disableLRUOrdering = YES;
				[children addObject:item];
			}
		}
		_children = children.count ? children : nil;
	}
	return self;
}

- (instancetype)initWithValues:(NSDictionary*)someValues parentItem:(GenieItem*)parentItem directory:(NSString*)directory
{
	_values = [someValues mutableCopy];
	NSString* identifier = someValues[@"uid"] ?: someValues[@"title"] ?: [[NSUUID UUID] UUIDString];
	return [self initWithIdentifier:identifier parentItem:parentItem directory:directory];
}

- (id)copyWithNewParent:(GenieItem*)parentItem
{
	GenieItem* newItem = [[GenieItem alloc] initWithValues:_values parentItem:parentItem directory:self.directory];
	newItem.disableLRUOrdering = self.disableLRUOrdering;
	return newItem;
}

- (void)dealloc
{
	self.live = NO;
	self.dataSourceCacheRecord = nil;
	self.dependsOnRunningApplications = NO;

	if(_metadataQuery)
	{
		[[NSNotificationCenter defaultCenter] removeObserver:self name:NSMetadataQueryDidUpdateNotification object:_metadataQuery];
		[_metadataQuery stopQuery];
		_metadataQuery = nil;
	}
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"<%@: %@>", [self class], self.title];
}

- (NSString*)identifierWithContext
{
	NSString* parent     = self.parentItem.identifierWithContext;
	NSString* identifier = self.identifier ?: self.title;
	return parent ? [parent stringByAppendingFormat:@"\034%@", identifier] : identifier;
}

- (NSArray*)allKeys
{
	return _values.allKeys;
}

- (id)staticValueForKey:(NSString*)aKey
{
	return _values[aKey];
}

+ (id)jsonifyedValue:(id)value
{
	if([value isKindOfClass:[NSDate class]])
	{
		NSDateFormatter* formatter = [[NSDateFormatter alloc] init];
		formatter.dateFormat = @"yyyy-MM-dd HH:mm:ss";
		formatter.timeZone   = [NSTimeZone timeZoneForSecondsFromGMT:0];
		value = [formatter stringFromDate:value];
	}
	else if([value isKindOfClass:[NSURL class]])
	{
		value = [(NSURL*)value absoluteString];
	}
	else if([value isKindOfClass:[NSArray class]])
	{
		NSMutableArray* array = [NSMutableArray array];
		for(id item in value)
		{
			if(id jsonFriendly = [GenieItem jsonifyedValue:item])
				[array addObject:jsonFriendly];
		}
		value = array.count ? array : nil;
	}
	else if([value isKindOfClass:[NSDictionary class]])
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionary];
		for(NSString* key in value)
		{
			if(id jsonFriendly = [GenieItem jsonifyedValue:value[key]])
				dict[key] = jsonFriendly;
		}
		value = dict.count ? dict : nil;
	}
	else if(value && ![value isKindOfClass:[NSString class]] && ![value isKindOfClass:[NSNumber class]])
	{
		NSLog(@"[%@ asJSONObject] *** discard value: %@", [self class], [value class]);
		value = nil;
	}
	return value;
}

- (NSDictionary*)asJSONObject
{
	NSMutableDictionary* res = [NSMutableDictionary dictionary];
	for(NSString* key in self.allKeys)
		res[key] = [GenieItem jsonifyedValue:[self staticValueForKey:key]];

	if(NSArray<GenieItem*>* children = self.children)
	{
		NSMutableArray* array = [NSMutableArray array];
		for(GenieItem* child in children)
			[array addObject:child.asJSONObject];
		res[@"children"] = array;
	}
	return res;
}

- (NSURL*)previewItemURL
{
	NSURL* url;
	if(NSString* urlString = self.url)
		url = [NSURL URLWithString:urlString];
	else if(NSString* file = self.file)
		url = [NSURL fileURLWithPath:file];
	return url.filePathURL;
}

- (void)updateHTML
{
	if(_hasHTMLOutput && !_htmlOutputItem.htmlString)
	{
		if(_updatingHTML)
		{
			_pendingUpdateHTML = YES;
			return;
		}
		_updatingHTML = YES;

		if(NSArray* program = self.scriptWithArguments)
		{
			NSMutableDictionary* environment = [self.environment mutableCopy];
			environment[@"GENIE_HTML_OUTPUT"] = @"1";

			GenieTask* task = [[GenieTask alloc] initWithCommand:self.scriptWithArguments directory:self.directory];
			task.environment = environment;
			task.timeOut     = 15;

			[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
				NSString* stdoutStr = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
				NSString* stderrStr = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];
				_htmlOutputItem.htmlString = rc == 0 && OakNotEmptyString(stdoutStr) ? stdoutStr : stderrStr;

				_updatingHTML = NO;
				if(_pendingUpdateHTML)
				{
					_pendingUpdateHTML = NO;
					[self updateHTML];
				}
			}];
		}
	}
}

- (BOOL)matchesPredicate:(NSPredicate*)aPredicate
{
	BOOL res = NO;
	@try {
		res = [aPredicate evaluateWithObject:[[GenieWrapper alloc] initWithValues:self]];
	}
	@catch (NSException* e) {
		NSLog(@"[%@ matchesPredicate:%@] %@", [self class], aPredicate, e);
	}
	return res;
}

- (BOOL)isTemplate
{
	return [[self staticValueForKey:@"isTemplate"] boolValue];
}

- (BOOL)isPlaceholder
{
	return [[self staticValueForKey:@"isPlaceholder"] boolValue];
}

- (NSString*)queryString
{
	BOOL fullString = self.isFallback || self.kind == kGenieItemKindCommandResult;
	return fullString ? _filter.string : _filter.queryString;
}

- (void)setFilter:(GenieFilter*)newFilter
{
	NSString* oldQuery = self.queryString;

	_filter = newFilter;
	[_dataSourceResults makeObjectsPerformSelector:_cmd withObject:newFilter];

	if(![oldQuery isEqualToString:self.queryString])
	{
		_htmlOutputItem.queryString = self.queryString;
		[self updateValuesForKeysDependingOnVariable:@"query"];
	}
}

// ===========================
// = Format String Expansion =
// ===========================

- (NSDictionary*)environment
{
	if(!_environment)
		_environment = [GenieManager.sharedInstance.environment copy];
	return _environment;
}

- (void)setValueForKey:(NSString*)aKey dependsOnVariable:(NSString*)aVariable
{
	if(aKey)
	{
		if(!_valuesDependingOnKeys)
			_valuesDependingOnKeys = [NSMutableDictionary dictionary];

		NSMutableSet* keys = _valuesDependingOnKeys[aVariable] ?: (_valuesDependingOnKeys[aVariable] = [NSMutableSet set]);
		[keys addObject:aKey];
	}
}

- (void)updateValuesForKeysDependingOnVariable:(NSString*)aVariable
{
	if(NSSet* keys = _valuesDependingOnKeys[aVariable])
	{
		[_valuesDependingOnKeys removeObjectForKey:aVariable];
		for(NSString* key in keys)
		{
			[self willChangeValueForKey:key];
			[_cachedValues removeObjectForKey:key];
			[self didChangeValueForKey:key];

			if([key isEqualToString:@"scriptWithArguments"])
			{
				_dataSourceCacheRecord.digest = [self.scriptWithArguments componentsJoinedByString:@"\034"];
				if(_dataSourceCacheRecord.expired)
					self.dataSourceNeedsUpdate = YES;
			}
		}
	}
}

- (NSString*)stringForKey:(NSString*)aKey whileExpanding:(NSString*)expandingKey
{
	static NSString* const kParentPrefix = @"parent.";

	id value;
	if([aKey isEqualToString:@"query"])
	{
		value = self.queryString;
		[self setValueForKey:expandingKey dependsOnVariable:aKey];
	}
	else if([aKey isEqualToString:@"last_query"])
	{
		return [GenieLRUDatabase.sharedInstance lastQueryStringForItem:self];
	}
	else if([aKey isEqualToString:@"clipboard"])
	{
		value = [[NSPasteboard generalPasteboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[NSPasteboard generalPasteboard] stringForType:NSStringPboardType] : nil;
	}
	else if([aKey isEqualToString:@"find"])
	{
		value = [[NSPasteboard pasteboardWithName:NSFindPboard] availableTypeFromArray:@[ NSStringPboardType ]] ? [[NSPasteboard pasteboardWithName:NSFindPboard] stringForType:NSStringPboardType] : nil;
	}
	else if([aKey hasPrefix:kParentPrefix])
	{
		NSString* parentKey = aKey;
		GenieItem* parentItem = self;
		while([parentKey hasPrefix:kParentPrefix])
		{
			parentKey  = [parentKey substringFromIndex:kParentPrefix.length];
			parentItem = parentItem.parentItem;
		}

		static NSSet* const dynamicKeys = [NSSet setWithArray:@[ @"title", @"subtitle", @"file", @"url", @"value", @"uiTitle", @"invalidate", @"bundleIdentifier", @"sqlDatabase", @"mdApplicationCanOpen" ]];
		if([dynamicKeys containsObject:parentKey])
				value = [parentItem valueForKey:parentKey];
		else	value = [parentItem stringForKey:parentKey whileExpanding:nil];
	}
	else
	{
		value = [self staticValueForKey:aKey] ?: self.environment[aKey];
		if(!value && [aKey isEqualToString:NSMetadataItemDurationSecondsKey])
		{
			if(NSTimeInterval duration = [GenieMediaDuration.sharedInstance durationForPath:self.file])
			{
				value = @(duration);
			}
			else
			{
				[self setValueForKey:expandingKey dependsOnVariable:NSMetadataItemDurationSecondsKey];
				[GenieMediaDuration.sharedInstance obtainDurationForPath:self.file andCallback:^(NSTimeInterval duration){
					[self updateValuesForKeysDependingOnVariable:NSMetadataItemDurationSecondsKey];
				}];
			}
		}
	}

	if([value isKindOfClass:[NSArray class]] && [value count] <= 1)
		value = [value firstObject];

	if([value isKindOfClass:[NSDate class]])
		value = [value descriptionWithLocale:nil];
	else if(![value isKindOfClass:[NSString class]])
		value = [value respondsToSelector:@selector(stringValue)] ? [value stringValue] : [value description];

	return value;
}

- (NSString*)expandedStringFromFormat:(NSString*)formatString whileExpanding:(NSString*)expandingKey abbreviatePath:(BOOL)tildePath urlEncodeVariables:(BOOL)urlEscape
{
	auto const getVariable = [&](std::string const& varName, std::string const& fallback) -> std::string {
		if(NSString* res = [self stringForKey:to_ns(varName) whileExpanding:expandingKey])
		{
			if(urlEscape)
			{
				res = [res stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.alphanumericCharacterSet];
			}
			else if(tildePath)
			{
				NSString* variable = to_ns(varName);
				for(NSString* fileKey in @[ @"file", NSMetadataItemPathKey ])
				{
					if([variable isEqualToString:fileKey] || [variable hasSuffix:[@"parent." stringByAppendingString:fileKey]])
						res = [res stringByAbbreviatingWithTildeInPath];
				}
			}
			return to_s(res);
		}
		return fallback;
	};
	return [formatString isKindOfClass:[NSString class]] ? to_ns(format_string::expand(to_s(formatString), getVariable)) : nil;
}

- (NSString*)expandedStringForKey:(NSString*)aKey abbreviatePath:(BOOL)tildePath urlEncodeVariables:(BOOL)urlEscape fallback:(NSString*)fallback
{
	id value = _cachedValues[aKey];
	if(!value)
	{
		NSString* format = [self staticValueForKey:aKey] ?: fallback;
		value = [self expandedStringFromFormat:format whileExpanding:aKey abbreviatePath:tildePath urlEncodeVariables:urlEscape];
		_cachedValues[aKey] = value ?: [NSNull null];
	}
	return [value isKindOfClass:[NSString class]] ? value : nil;
}

- (NSString*)absolutePathForPath:(NSString*)aPath
{
	if(!aPath || [aPath isEqualToString:@""])
		aPath = nil;
	else if([aPath hasPrefix:@"~/"])
		aPath = [aPath stringByExpandingTildeInPath];
	else if(![aPath hasPrefix:@"/"])
		aPath = [_directory stringByAppendingPathComponent:aPath];
	return aPath;
}

- (NSString*)match
{
	if(!_match)
		_match = [[([self staticValueForKey:@"match"] ?: self.title) lowercaseString] decomposedStringWithCanonicalMapping];
	return _match;
}

- (NSString*)title                { return [self expandedStringForKey:@"title"                                  abbreviatePath:YES urlEncodeVariables:NO  fallback:@"${kMDItemDisplayName}"]; }
- (NSString*)subtitle             { return [self expandedStringForKey:@"subtitle"                               abbreviatePath:YES urlEncodeVariables:NO  fallback:@"${kMDItemPath}"]; }
- (NSString*)file                 { return [self absolutePathForPath:[self expandedStringForKey:@"file"         abbreviatePath:NO  urlEncodeVariables:NO  fallback:@"${kMDItemPath}"]]; }

- (NSString*)url
{
	NSString* urlString = [self expandedStringForKey:@"url" abbreviatePath:NO urlEncodeVariables:YES fallback:nil];
	return [urlString stringByAddingPercentEncodingWithAllowedCharacters:[NSCharacterSet characterSetWithRange:NSMakeRange(0, 128)]];
}

- (NSString*)value                { return [self expandedStringForKey:@"value"                                  abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]; }
- (NSString*)uiTitle              { return [self expandedStringForKey:@"uiTitle"                                abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]; }
- (NSString*)invalidate           { return [self expandedStringForKey:@"invalidate"                             abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]; }
- (NSString*)bundleIdentifier     { return [self expandedStringForKey:@"bundleIdentifier"                       abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]; }
- (NSString*)sqlDatabase          { return [self absolutePathForPath:[self expandedStringForKey:@"sqlDatabase"  abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]]; }
- (NSString*)mdApplicationCanOpen { return [self expandedStringForKey:@"mdApplicationCanOpen"                   abbreviatePath:NO  urlEncodeVariables:NO  fallback:nil]; }

- (NSImage*)iconImage
{
	NSImage* res = _cachedValues[@"iconImage"];
	if(res)
		return res;

	NSSize iconSize = NSMakeSize(32, 32);

	NSDictionary* iconInfo = [self staticValueForKey:@"icon"];
	if(iconInfo.count)
	{
		NSString* pathFromURL;
		if(NSString* urlString = iconInfo[@"url"])
			pathFromURL = [NSURL URLWithString:urlString].filePathURL.path;

		if(NSString* name = iconInfo[@"name"])
			res = [NSImage imageNamed:name];
		else if(NSString* file = [self absolutePathForPath:iconInfo[@"file"]] ?: pathFromURL)
			res = [[NSWorkspace sharedWorkspace] iconForFile:file];
		else if(NSString* fileType = iconInfo[@"fileType"])
			res = [[NSWorkspace sharedWorkspace] iconForFileType:fileType];
		else if(NSString* imagePath = [self absolutePathForPath:iconInfo[@"image"]])
			res = [[NSImage alloc] initByReferencingFile:imagePath];
		else if(NSString* text = [self expandedStringFromFormat:iconInfo[@"text"] whileExpanding:@"iconImage" abbreviatePath:NO urlEncodeVariables:NO])
			res = [StringImage imageWithString:text size:NSMakeSize(64, 64)];
		else if(NSString* bundleIdentifier = iconInfo[@"application"])
		{
			if(NSString* path = [[NSWorkspace sharedWorkspace] absolutePathForAppBundleWithIdentifier:bundleIdentifier])
				res = [[NSWorkspace sharedWorkspace] iconForFile:path];
		}
		else if(NSString* urlString = iconInfo[@"url"])
		{
			if(NSURL* url = [NSURL URLWithString:urlString])
			{
				res = [GenieFavoriteIcon.sharedInstance favoriteIconForURL:url];
				if(!res)
				{
					res = [[NSWorkspace sharedWorkspace] iconForFileType:@"com.apple.web-internet-location"];

					[GenieFavoriteIcon.sharedInstance obtainFavoriteIconForURL:url andCallback:^(NSImage* image){
						[self willChangeValueForKey:@"iconImage"];
						image.size = iconSize;
						_cachedValues[@"iconImage"] = image;
						[self didChangeValueForKey:@"iconImage"];
					}];
				}
			}
		}
	}

	if(!res)
	{
		if(NSString* file = self.file)
		{
			res = [[NSWorkspace sharedWorkspace] iconForFile:file];
		}
		else if(NSString* urlString = self.url)
		{
			if(NSURL* url = [NSURL URLWithString:urlString])
			{
				res = [GenieFavoriteIcon.sharedInstance favoriteIconForURL:url];
				if(!res)
				{
					res = [[NSWorkspace sharedWorkspace] iconForFileType:@"com.apple.web-internet-location"];

					[GenieFavoriteIcon.sharedInstance obtainFavoriteIconForURL:url andCallback:^(NSImage* image){
						[self willChangeValueForKey:@"iconImage"];
						image.size = iconSize;
						_cachedValues[@"iconImage"] = image;
						[self didChangeValueForKey:@"iconImage"];
					}];
				}
			}
		}
		else
		{
			res = self.parentItem.iconImage;
		}
	}

	if(!res)
		res = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];

	double aspect = res.size.width / res.size.height;
	if(aspect > 1)
			res.size = NSMakeSize(iconSize.width, iconSize.height/aspect);
	else	res.size = NSMakeSize(iconSize.width/aspect, iconSize.height);

	_cachedValues[@"iconImage"] = res;
	return res;
}

- (NSArray*)scriptWithArguments
{
	NSArray* res = _cachedValues[@"scriptWithArguments"];
	if(!res)
	{
		if(NSString* script = [self staticValueForKey:@"script"])
		{
			if([script hasPrefix:@"#!"])
			{
				NSString* cacheDir   = [GenieManager.sharedInstance cacheFolderByAppendingPathComponent:@"Scripts"];
				NSString* scriptPath = [cacheDir stringByAppendingPathComponent:hash(script)];

				if(![NSFileManager.defaultManager isExecutableFileAtPath:scriptPath])
				{
					[script writeToFile:scriptPath atomically:YES encoding:NSUTF8StringEncoding error:nil];
					[NSFileManager.defaultManager setAttributes:@{ NSFilePosixPermissions: @(S_IRWXU) } ofItemAtPath:scriptPath error:nil];
				}
				res = @[ scriptPath ];
			}
			else
			{
				res = @[ @"/bin/sh", @"-c", script ];
			}
		}
		else if(NSArray* execArray = [self staticValueForKey:@"exec"])
		{
			if([execArray isKindOfClass:[NSArray class]])
			{
				NSMutableArray* array = [NSMutableArray array];
				for(NSString* str in execArray)
					[array addObject:[self expandedStringFromFormat:str whileExpanding:@"scriptWithArguments" abbreviatePath:NO urlEncodeVariables:NO] ?: @""];
				res = array;
			}
		}

		if(NSArray* scriptArguments = [_mutableScriptArguments valueForKey:@"value"] ?: [self staticValueForKey:@"scriptArguments"])
		{
			NSMutableArray* array = [NSMutableArray array];
			for(NSString* argument in scriptArguments)
				[array addObject:[self expandedStringFromFormat:argument whileExpanding:@"scriptWithArguments" abbreviatePath:NO urlEncodeVariables:NO] ?: @""];
			res = res ? [res arrayByAddingObjectsFromArray:array] : array;
		}

		if(NSString* executablePath = res.firstObject)
		{
			if(![executablePath hasPrefix:@"/"])
			{
				if(self.directory && [executablePath containsString:@"/"])
				{
					executablePath = [[self.directory stringByAppendingPathComponent:executablePath] stringByStandardizingPath];
				}
				else
				{
					NSDictionary* environment = self.environment ?: [[NSProcessInfo processInfo] environment];
					for(NSString* path in [environment[@"PATH"] componentsSeparatedByString:@":"])
					{
						NSString* candidate = [path stringByAppendingPathComponent:executablePath];
						if(![NSFileManager.defaultManager isExecutableFileAtPath:candidate])
							continue;

						executablePath = candidate;
						break;
					}
				}

				NSMutableArray* tmp = [NSMutableArray arrayWithArray:res];
				tmp[0] = executablePath;
				res = tmp;

				if(![NSFileManager.defaultManager isExecutableFileAtPath:res.firstObject])
					os_log_error(OS_LOG_DEFAULT, "no executable at path: %{public}@", res.firstObject);
			}
		}

		// Do not cache script arguments if edited via Genie Prefs
		if(_mutableScriptArguments == nil)
			_cachedValues[@"scriptWithArguments"] = res ?: [NSNull null];
	}
	return [res isKindOfClass:[NSArray class]] ? res : nil;
}

- (NSArray*)clipboardRepresentations
{
	if(NSString* value = self.value)
		return @[ value ];
	else if(NSString* file = self.file)
		return @[ [NSURL fileURLWithPath:file], file ];
	else if(NSString* urlString = self.url)
		return @[ [NSURL URLWithString:urlString], urlString ];
	else if(NSString* title = self.title)
		return @[ title ];
	return nil;
}

// ===============
// = Data Source =
// ===============

- (void)flushCachedValues
{
	for(GenieItem* item in _dataSourceResults)
		[item flushCachedValues];

	// Incase any of these fields show relative time
	_cachedValues[@"title"]    = nil;
	_cachedValues[@"subtitle"] = nil;
}

- (BOOL)hasReplacementItems
{
	switch(self.kind)
	{
		case kGenieItemKindSpotlight:
		case kGenieItemKindSqlite:
		case kGenieItemKindCommandResult:
		case kGenieItemKindRecentDocuments:
			return YES;
		break;
	}
	return NO;
}

- (NSArray<GenieItem*>*)replacementItems
{
	if(self.hasReplacementItems)
		return _dataSourceResults ?: @[ ];

	if(self.kind == kGenieItemKindPredicateGroup)
		return @[ ];

	return @[ self ];
}

- (BOOL)isBusy
{
	return _updating;
}

- (void)setLive:(BOOL)flag
{
	if(_live == flag)
		return;

	if(_live = flag)
	{
		[self refreshDataSourceIfNeeded];
	}
	else
	{
		[self flushCachedValues];
		_htmlOutputItem.htmlString  = nil;
		_htmlOutputItem.queryString = nil;
	}
}

- (void)setDataSourceNeedsUpdate:(BOOL)flag
{
	if(_dataSourceNeedsUpdate == flag)
		return;

	if(_dataSourceNeedsUpdate = flag)
		[self refreshDataSourceIfNeeded];
}

- (void)refreshDataSourceIfNeeded
{
	if(!self.hasReplacementItems)
		return;

	if(_live && !_dataSourceResults)
	{
		if(GenieDataSourceCacheRecord* cacheRecord = [GenieDataSourceCache.sharedInstance resultForKey:self.identifier])
		{
			if(self.kind == kGenieItemKindCommandResult)
				cacheRecord.digest = [self.scriptWithArguments componentsJoinedByString:@"\034"];

			self.dataSourceCacheRecord = cacheRecord;
			self.dataSourceResults = [self createGenieItemsFrom:cacheRecord.items];

			if(cacheRecord.expired)
					_dataSourceNeedsUpdate = YES;
			else	[cacheRecord checkExpired];
		}

		if(!_dataSourceResults)
		{
			NSArray* placeholders = [_children filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"placeholder = YES"]];
			[placeholders makeObjectsPerformSelector:@selector(setFilter:) withObject:_filter];
			self.dataSourceResults = placeholders ?: @[ ];
			_dataSourceNeedsUpdate = YES;
		}
	}

	if(_live && _dataSourceNeedsUpdate)
		[self refreshDataSource];
}

- (void)refreshDataSource
{
	if(_updating)
	{
		_dataSourceNeedsUpdate = YES;
		return;
	}
	self.updating = YES;
	self.dataSourceNeedsUpdate = NO;

	auto callback = ^(GenieDataSourceCacheRecord* res, NSString* error, NSData* stdout, NSData* stderr){
		if(NSArray* items = res.items)
		{
			if(![_dataSourceCacheRecord.items isEqualToArray:items])
				self.dataSourceResults = [self createGenieItemsFrom:items];

			if(self.kind == kGenieItemKindCommandResult && self.identifier)
				[GenieDataSourceCache.sharedInstance setResult:res forKey:self.identifier];
		}

		self.dataSourceCacheRecord = res;
		self.updating = NO;

		if(_dataSourceNeedsUpdate)
			[self refreshDataSource];
	};

	switch(self.kind)
	{
		case kGenieItemKindSpotlight:       [self runSpotlightQueryAndCallback:callback];            break;
		case kGenieItemKindSqlite:          [self runSqliteDataSourceAndCallback:callback];          break;
		case kGenieItemKindCommandResult:   [self runScriptDataSourceAndCallback:callback];          break;
		case kGenieItemKindRecentDocuments: [self runRecentDocumentsDataSourceAndCallback:callback]; break;
	}
}

- (NSArray<GenieItem*>*)createGenieItemsFrom:(NSArray*)items
{
	GenieItem* templateItem = [_children filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"template = YES"]].firstObject;

	NSMutableArray<GenieItem*>* genieItems = [NSMutableArray array];
	for(id item in items)
	{
		GenieItem* genieItem;
		if([item isKindOfClass:[NSDictionary class]])
			genieItem = [[GenieItemDictionary alloc] initWithValues:item dataSource:self templateItem:templateItem parentItem:self.parentItem directory:self.directory];
		else if([item isKindOfClass:[NSMetadataItem class]])
			genieItem = [[GenieItemMetadata alloc] initWithValues:item dataSource:self templateItem:templateItem parentItem:self.parentItem directory:self.directory];
		else
			continue;

		genieItem.filter = _filter;
		genieItem.live   = _live;
		[genieItems addObject:genieItem];
	}
	return genieItems;
}

- (void)setDataSourceCacheRecord:(GenieDataSourceCacheRecord*)newDataSourceCacheRecord
{
	if(_dataSourceCacheRecord)
	{
		_dataSourceCacheRecord.watchDependencies = NO;
		[[NSNotificationCenter defaultCenter] removeObserver:self name:GenieDataSourceCacheRecordDidExpireNotification object:_dataSourceCacheRecord];
	}

	if(_dataSourceCacheRecord = newDataSourceCacheRecord)
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(cacheRecordDidExpire:) name:GenieDataSourceCacheRecordDidExpireNotification object:_dataSourceCacheRecord];
		_dataSourceCacheRecord.watchDependencies = YES;
	}
}

- (void)cacheRecordDidExpire:(NSNotification*)aNotification
{
	self.dataSourceNeedsUpdate = YES;
}

// ======================
// = Script Data Source =
// ======================

- (void)runScriptDataSourceAndCallback:(void(^)(GenieDataSourceCacheRecord*, NSString*, NSData*, NSData*))callback
{
	NSArray* program = self.scriptWithArguments;

	GenieTask* task = [[GenieTask alloc] initWithCommand:program directory:self.directory];
	task.environment = self.environment;

	id timeOutValue = [self staticValueForKey:@"timeOut"];
	task.timeOut = timeOutValue ? [timeOutValue intValue] : 30; // 30s

	// self.error = nil;
	// self.standardOutput = nil;
	// self.standardError  = nil;

	[task launch:^(int rc, NSData* stdoutData, NSData* stderrData){
		GenieDataSourceCacheRecord* res;
		NSString* error;
		if(rc == 0)
		{
			@try {
				NSDictionary* json = [NSJSONSerialization JSONObjectWithData:stdoutData options:0 error:nullptr];
				if(json && [json isKindOfClass:[NSDictionary class]])
				{
					res = [[GenieDataSourceCacheRecord alloc] initWithDigest:[program componentsJoinedByString:@"\034"]];
					res.title = self.title;

					if(json[@"items"] && [json[@"items"] isKindOfClass:[NSArray class]])
						res.items = json[@"items"];

					if(NSTimeInterval expiresIn = ParseDuration(json[@"expires_in"]))
						res.expirationDate = [NSDate dateWithTimeIntervalSinceNow:expiresIn];

					if(NSString* dependsOn = json[@"depends_on"])
						res.dependsOnPath = dependsOn;

					res.disablePersistence = self.isFallback || !(res.expirationDate || res.dependsOnPath);
				}
				else
				{
					error = @"command did not produce expected JSON output";
				}
			}
			@catch (NSException* e) {
				error = [NSString stringWithFormat:@"exception parsing JSON output: %@", e];
			}
		}
		else
		{
			error = [NSString stringWithFormat:@"command terminated with code %d", rc];
		}
		callback(res, error, stdoutData, stderrData);
	}];
}

// ======================
// = Sqlite Data Source =
// ======================

- (void)runSqliteDataSourceAndCallback:(void(^)(GenieDataSourceCacheRecord*, NSString*, NSData*, NSData*))callback
{
	NSString* path  = self.sqlDatabase;
	NSString* query = [self staticValueForKey:@"sqlQuery"];
	if(path && query)
	{
		NSMutableArray* queryArguments = [NSMutableArray array];
		for(NSString* argument in [_mutableSqlBindings valueForKey:@"value"] ?: [self staticValueForKey:@"sqlBindings"])
			[queryArguments addObject:[self expandedStringFromFormat:argument whileExpanding:@"scriptArguments" abbreviatePath:NO urlEncodeVariables:NO] ?: @""];

		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			NSString* error;

			sqlite3* db = nullptr;
			if(sqlite3_open_v2(path.fileSystemRepresentation, &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK)
			{
				sqlite3_busy_timeout(db, 500);

				sqlite3_stmt* stmt = nullptr;
				if(sqlite3_prepare_v2(db, [query UTF8String], -1, &stmt, nullptr) == SQLITE_OK)
				{
					for(NSUInteger i = 0; i < queryArguments.count; ++i)
						sqlite3_bind_text(stmt, i+1, [queryArguments[i] UTF8String], -1, nullptr);

					NSMutableArray<NSDictionary*>* items = [NSMutableArray array];

					int rc = SQLITE_ROW;
					while(true)
					{
						rc = sqlite3_step(stmt);
						if(rc != SQLITE_ROW)
							break;

						NSMutableDictionary* item = [NSMutableDictionary dictionary];
						for(int i = 0; i < sqlite3_data_count(stmt); ++i)
						{
							NSString* key = [NSString stringWithUTF8String:sqlite3_column_name(stmt, i)];									
							if(unsigned char const* str = sqlite3_column_text(stmt, i))
								item[key] = [NSString stringWithUTF8String:(char const*)str];
						}
						[items addObject:item];
					}

					if(sqlite3_finalize(stmt) == SQLITE_OK)
					{
						dispatch_async(dispatch_get_main_queue(), ^{
							GenieDataSourceCacheRecord* res = [[GenieDataSourceCacheRecord alloc] initWithDigest:query];
							res.disablePersistence = YES;
							res.dependsOnPath = path;
							res.items = items;
							callback(res, error, nil, nil);
						});
					}
					else
					{
						error = [NSString stringWithFormat:@"error: %@: %@", [NSString stringWithUTF8String:sqlite3_errmsg(db)], query];
					}
				}
				else
				{
					error = [NSString stringWithFormat:@"error: %@: %@", [NSString stringWithUTF8String:sqlite3_errmsg(db)], query];
				}
			}
			else
			{
				error = [NSString stringWithFormat:@"error: %@: %@", [NSString stringWithUTF8String:sqlite3_errmsg(db)], path];
			}

			sqlite3_close(db);

			if(error)
			{
				dispatch_async(dispatch_get_main_queue(), ^{
					callback(nil, error, nil, nil);
				});
			}
		});
	}
	else
	{
		if(!path)
			os_log_error(OS_LOG_DEFAULT, "missing database in sqlite3 item: %{public}@", self);
		if(!query)
			os_log_error(OS_LOG_DEFAULT, "missing query in sqlite3 item: %{public}@", self);
	}
}

// ================================
// = Recent Documents Data Source =
// ================================

- (void)runRecentDocumentsDataSourceAndCallback:(void(^)(GenieDataSourceCacheRecord*, NSString*, NSData*, NSData*))callback
{
	NSString* bundleIdentifier = self.bundleIdentifier;
	NSMutableArray<NSURL*>* urls = [NSMutableArray array];
	NSString* dependsOnPath;
	NSString* error;

	@try {
		// modern = 10.13 (High Sierra)
		NSString* modernPath = [NSHomeDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"Library/Application Support/com.apple.sharedfilelist/com.apple.LSSharedFileList.ApplicationRecentDocuments/%@.sfl2", bundleIdentifier]];
		NSString* legacyPath = [NSHomeDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"Library/Application Support/com.apple.sharedfilelist/com.apple.LSSharedFileList.ApplicationRecentDocuments/%@.sfl", bundleIdentifier]];

		if([NSFileManager.defaultManager fileExistsAtPath:modernPath isDirectory:nil])
		{
			NSDictionary* dict = [NSKeyedUnarchiver unarchiveObjectWithFile:modernPath];
			for(id item in dict[@"items"])
			{
				if(NSURL* url = [[NSURL URLByResolvingBookmarkData:item[@"Bookmark"] options:NSURLBookmarkResolutionWithoutUI|NSURLBookmarkResolutionWithoutMounting relativeToURL:nil bookmarkDataIsStale:nullptr error:nullptr] filePathURL])
					[urls addObject:url];
			}
			dependsOnPath = modernPath;
		}
		else if([NSFileManager.defaultManager fileExistsAtPath:legacyPath isDirectory:nil])
		{
			std::multimap<double, NSURL*> sorted;

			NSDictionary* dict = [NSKeyedUnarchiver unarchiveObjectWithFile:legacyPath];
			for(id item in dict[@"items"])
			{
				if([item respondsToSelector:@selector(URL)] && [item respondsToSelector:@selector(order)])
				{
					if(NSURL* url = [[item performSelector:@selector(URL)] filePathURL])
					{
						double(*f)(id, SEL) = (double(*)(id, SEL))[item methodForSelector:@selector(order)];
						sorted.emplace(f(item, @selector(order)), url);
					}
				}
			}

			for(auto const& pair : sorted)
				[urls addObject:pair.second];

			dependsOnPath = legacyPath;
		}
		else
		{
			error = [NSString stringWithFormat:@"unable to locate recent documents for bundle identifier ‘%@’", bundleIdentifier];
		}
	}
	@catch (NSException* e) {
		os_log_error(OS_LOG_DEFAULT, "exception reading recent documents for ‘%{public}@’: %{public}@", bundleIdentifier, e);
		error = [NSString stringWithFormat:@"exception reading recent documents for bundle identifier ‘%@’: %@", bundleIdentifier, e];
	}

	NSMutableArray<NSMetadataItem*>* items = [NSMutableArray array];
	for(NSURL* url in urls)
	{
		if(NSMetadataItem* searchItem = [[NSMetadataItem alloc] initWithURL:url])
			[items addObject:searchItem];
	}

	GenieDataSourceCacheRecord* res = [[GenieDataSourceCacheRecord alloc] initWithDigest:bundleIdentifier];
	res.disablePersistence = YES;
	res.dependsOnPath = dependsOnPath;
	res.items = items;
	callback(res, error, nil, nil);
}

// ===================
// = Spotlight Query =
// ===================

- (void)setDependsOnRunningApplications:(BOOL)flag
{
	if(_dependsOnRunningApplications == flag)
		return;

	if(_dependsOnRunningApplications = flag)
			[NSWorkspace.sharedWorkspace addObserver:self forKeyPath:@"runningApplications" options:0 context:kRunningApplicationsBindings];
	else	[NSWorkspace.sharedWorkspace removeObserver:self forKeyPath:@"runningApplications" context:kRunningApplicationsBindings];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)someObject change:(NSDictionary*)someChanges context:(void*)context
{
	if(context == kRunningApplicationsBindings)
	{
		if(_metadataQuery)
		{
			[[NSNotificationCenter defaultCenter] removeObserver:self name:NSMetadataQueryDidUpdateNotification object:_metadataQuery];
			[_metadataQuery stopQuery];
			_metadataQuery = nil;
		}

		self.dependsOnRunningApplications = NO;
		self.dataSourceNeedsUpdate = YES;
	}
	else
	{
		[super observeValueForKeyPath:keyPath ofObject:someObject change:someChanges context:context];
	}
}

- (void)metadataDidUpdateNotification:(NSNotification*)aNotification
{
	self.dataSourceNeedsUpdate = YES;
}

- (NSArray<NSMetadataItem*>*)itemsFromQuery:(NSMetadataQuery*)query
{
	NSMutableArray<NSMetadataItem*>* items = [NSMutableArray array];
	[query disableUpdates];
	for(NSUInteger i = 0; i < query.resultCount; ++i)
		[items addObject:[query resultAtIndex:i]];
	[query enableUpdates];
	return items;
}

- (void)runSpotlightQueryAndCallback:(void(^)(GenieDataSourceCacheRecord*, NSString*, NSData*, NSData*))callback
{
	if(_metadataQuery)
	{
		GenieDataSourceCacheRecord* res = [[GenieDataSourceCacheRecord alloc] initWithDigest:nil];
		res.disablePersistence = YES;
		res.items = [self itemsFromQuery:_metadataQuery];
		callback(res, nil, nil, nil);
	}
	else if(NSMetadataQuery* query = [self createMetadataQuery])
	{
		BOOL dependsOnRunningApplications = [[self staticValueForKey:@"mdApplicationIsRunning"] boolValue];
		__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSMetadataQueryDidFinishGatheringNotification object:query queue:nil usingBlock:^(NSNotification*){
			[[NSNotificationCenter defaultCenter] removeObserver:observerId];

			GenieDataSourceCacheRecord* res = [[GenieDataSourceCacheRecord alloc] initWithDigest:nil];
			res.disablePersistence = YES;
			res.items = [self itemsFromQuery:query];
			callback(res, nil, nil, nil);

			if(self.dependsOnRunningApplications = dependsOnRunningApplications)
				[query stopQuery];
		}];

		if(!dependsOnRunningApplications)
		{
			_metadataQuery = query;
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(metadataDidUpdateNotification:) name:NSMetadataQueryDidUpdateNotification object:query];
		}

		[query startQuery];
	}
	else
	{
		GenieDataSourceCacheRecord* res = [[GenieDataSourceCacheRecord alloc] initWithDigest:nil];
		res.disablePersistence = YES;
		res.items = @[ ];
		callback(res, nil, nil, nil);
	}
}

- (void)updateMetadataDisplayNameForItem:(GenieItemMetadata*)genieItem
{
	if(![genieItem.metadataItem valueForAttribute:NSMetadataItemVersionKey])
	{
		genieItem.metadataItemDisplayName = genieItem.nameWithoutVersion;
		return;
	}

	NSMutableDictionary* counts = [NSMutableDictionary dictionary];
	for(GenieItemMetadata* item in _dataSourceResults)
	{
		if(NSString* nameWithVersion = item.nameWithVersion)
		{
			NSString* name = item.nameWithoutVersion;
			counts[name]            = @([counts[name] intValue]+1);
			counts[nameWithVersion] = @([counts[nameWithVersion] intValue]+1);
		}
	}

	for(GenieItemMetadata* item in _dataSourceResults)
	{
		NSString* name = item.nameWithoutVersion;
		if(NSString* nameWithVersion = item.nameWithVersion)
		{
			NSInteger countOfDuplicateNames    = [counts[name] intValue];
			NSInteger countOfDuplicateVersions = [counts[nameWithVersion] intValue];

			if(countOfDuplicateNames > countOfDuplicateVersions)
				name = nameWithVersion;
		}
		item.metadataItemDisplayName = name;
	}
}

- (NSArray*)searchItemsInScope:(NSArray*)searchScopes
{
	NSMutableArray<NSURL*>* searchItems;
	if([[self staticValueForKey:@"mdApplicationIsRunning"] boolValue])
	{
		searchItems = [NSMutableArray array];
		for(NSRunningApplication* app in [[NSWorkspace sharedWorkspace] runningApplications])
		{
			if(NSURL* url = app.bundleURL.filePathURL ?: app.executableURL.filePathURL)
				[searchItems addObject:url];
		}
	}
	else if(NSString* someURL = self.mdApplicationCanOpen)
	{
		searchItems = [NSMutableArray array];
		if(NSURL* canOpenURL = [someURL hasPrefix:@"/"] ? [NSURL fileURLWithPath:someURL] : [NSURL URLWithString:someURL])
		{
			if(CFArrayRef urls = LSCopyApplicationURLsForURL((__bridge CFURLRef)canOpenURL, kLSRolesAll))
				searchItems = (NSMutableArray*)CFBridgingRelease(urls);
		}
	}

	if(!searchItems)
		return nil;

	if(searchScopes)
	{
		NSPredicate* predicate = [NSPredicate predicateWithBlock:^BOOL(NSURL* url, NSDictionary* bindings){
			NSString* itemPath = url.filePathURL.path.stringByStandardizingPath;
			for(NSString* path in searchScopes)
			{
				if([itemPath hasPrefix:[path stringByAppendingString:@"/"]])
					return YES;
			}
			return NO;
		}];
		[searchItems setArray:[searchItems filteredArrayUsingPredicate:predicate]];
	}

	NSMutableArray<NSMetadataItem*>* metadataItems = [NSMutableArray array];
	for(NSURL* url in searchItems)
	{
		if(NSMetadataItem* searchItem = [[NSMetadataItem alloc] initWithURL:url])
				[metadataItems addObject:searchItem];
		else	NSLog(@"%s *** failed to create meta data item for %@", sel_getName(_cmd), url.filePathURL.path);
	}
	return metadataItems;
}

- (NSMetadataQuery*)createMetadataQuery
{
	NSMetadataQuery* query = [[NSMetadataQuery alloc] init];
	query.predicate = [NSPredicate predicateFromMetadataQueryString:[self staticValueForKey:@"mdQuery"] ?: @"TRUE"];

	NSArray* searchScopes;
	if(NSArray* scopes = [self staticValueForKey:@"mdScope"])
	{
		scopes = [scopes filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES"]];
		scopes = [scopes valueForKeyPath:@"path.stringByStandardizingPath"];
		if(scopes.count)
			query.searchScopes = searchScopes = scopes;
	}

	static NSSet* const kStringKeys = [NSSet setWithArray:@[ NSMetadataItemDisplayNameKey ]];

	NSString* sortKey = [self staticValueForKey:@"sortBy"] ?: NSMetadataItemDisplayNameKey;
	BOOL descending   = [[self staticValueForKey:@"descending"] boolValue];
	SEL method        = [kStringKeys containsObject:sortKey] ? @selector(localizedCaseInsensitiveCompare:) : @selector(compare:);
	query.sortDescriptors = @[
		[NSSortDescriptor sortDescriptorWithKey:sortKey ascending:!descending selector:method],
		[NSSortDescriptor sortDescriptorWithKey:NSMetadataItemDisplayNameKey ascending:YES selector:@selector(localizedCaseInsensitiveCompare:)],
		[NSSortDescriptor sortDescriptorWithKey:NSMetadataItemVersionKey ascending:NO selector:@selector(compare:)],
	];

	if(NSArray* searchItems = [self searchItemsInScope:searchScopes])
	{
		if(searchItems.count == 0)
			return nil;
		query.searchItems = searchItems;
	}

	return query;
}

// =============================
// = Used by Genie Preferences =
// =============================

- (void)flushIconImageAndKey:(NSString*)aKey
{
	[self willChangeValueForKey:@"iconImage"];
	if(aKey)
		[_cachedValues removeObjectForKey:aKey];
	[_cachedValues removeObjectForKey:@"iconImage"];
	[self didChangeValueForKey:@"iconImage"];
}

- (void)flushScriptWithArguments
{
	[self willChangeValueForKey:@"scriptWithArguments"];
	[_cachedValues removeObjectForKey:@"scriptWithArguments"];
	[self didChangeValueForKey:@"scriptWithArguments"];
}

- (NSString*)primitiveTitle                                 { return _values[@"title"]; }
- (NSString*)primitiveSubtitle                              { return _values[@"subtitle"]; }
- (NSString*)primitiveMatch                                 { return _values[@"match"]; }
- (NSString*)primitiveFile                                  { return _values[@"file"]; }
- (NSString*)primitiveUrl                                   { return _values[@"url"]; }

- (void)setPrimitiveTitle:(NSString*)newString              { _values[@"title"]    = newString; }
- (void)setPrimitiveSubtitle:(NSString*)newString           { _values[@"subtitle"] = newString; }
- (void)setPrimitiveMatch:(NSString*)newString              { _values[@"match"]    = newString; }
- (void)setPrimitiveFile:(NSString*)newString               { _values[@"file"]     = newString; [self flushIconImageAndKey:@"file"];  }
- (void)setPrimitiveUrl:(NSString*)newString                { _values[@"url"]      = newString; [self flushIconImageAndKey:@"url"]; }

- (NSDictionary*)iconDescription                            { return _values[@"icon"]; }
- (void)setIconDescription:(NSDictionary*)newDictionary     { _values[@"icon"] = newDictionary; [self flushIconImageAndKey:nil]; }

- (NSString*)predicate                                      { return _values[@"predicate"]; }
- (void)setPredicate:(NSString*)newString                   { _values[@"predicate"] = newString; }

- (NSString*)primitiveBundleIdentifier                      { return _values[@"bundleIdentifier"]; }
- (void)setPrimitiveBundleIdentifier:(NSString*)newString   { _values[@"bundleIdentifier"] = newString; }

// Spotlight

- (NSString*)mdQuery                                        { return _values[@"mdQuery"]; }
- (void)setMdQuery:(NSString*)newString                     { _values[@"mdQuery"] = newString; }

- (NSMutableArray*)mdScope
{
	if(!_mdScope)
	{
		NSMutableArray<NSMutableDictionary*>* array = [NSMutableArray array];
		for(NSDictionary* scope in _values[@"mdScope"])
			[array addObject:[scope mutableCopy]];
		_mdScope = array;
	}
	return _mdScope;
}

- (NSString*)sortBy                                         { return _values[@"sortBy"]; }
- (void)setSortBy:(NSString*)newString                      { _values[@"sortBy"] = newString; }

- (BOOL)descending                                          { return [_values[@"descending"] boolValue]; }
- (void)setDescending:(BOOL)flag                            { _values[@"descending"] = flag ? @YES : nil; }

// Run Script

- (NSString*)script                                         { return _values[@"script"]; }
- (void)setScript:(NSString*)newString                      { _values[@"script"] = newString; [self flushScriptWithArguments]; }

- (NSMutableArray*)mutableScriptArguments
{
	if(!_mutableScriptArguments)
	{
		_mutableScriptArguments = [NSMutableArray array];
		for(NSString* value in _values[@"scriptArguments"])
			[_mutableScriptArguments addObject:[@{ @"value": value } mutableCopy]];
	}
	return _mutableScriptArguments;
}

// Sqlite

- (NSString*)primitiveSqlDatabase                           { return _values[@"sqlDatabase"]; }
- (void)setPrimitiveSqlDatabase:(NSString*)newString        { _values[@"sqlDatabase"] = newString; }

- (NSString*)sqlQuery                                       { return _values[@"sqlQuery"]; }
- (void)setSqlQuery:(NSString*)newString                    { _values[@"sqlQuery"] = newString; }

- (NSMutableArray*)mutableSqlBindings
{
	if(!_mutableSqlBindings)
	{
		_mutableSqlBindings = [NSMutableArray array];
		for(NSString* value in _values[@"sqlBindings"])
			[_mutableSqlBindings addObject:[@{ @"value": value } mutableCopy]];
	}
	return _mutableSqlBindings;
}

// Other

- (BOOL)canEditDisplayName                      { return !(self.isTemplate || self.isPlaceholder); }
- (NSString*)displayName                        { return self.isPlaceholder ? @"PLACEHOLDER" : (self.isTemplate ? @"TEMPLATE" : self.primitiveTitle); }
- (void)setDisplayName:(NSString*)newString     { self.primitiveTitle = newString; }

static NSSet* const kFilteredKeys = [NSSet setWithObjects:
	@"uid", @"disabled", @"kind", @"title", @"subtitle", @"match", @"icon", @"children", @"file", @"url", @"script", @"scriptArguments", @"predicate", @"bundleIdentifier", @"sqlDatabase", @"sqlQuery", @"sqlBindings", @"mdQuery", @"mdScope", @"sortBy", @"descending",
	nil
];

- (NSUInteger)countOfAdvancedKeys
{
	NSUInteger res = 0;
	for(NSString* key in [_values allKeys])
	{
		if(![kFilteredKeys containsObject:key])
			++res;
	}
	return res;
}

- (NSString*)plistDump
{
	NSMutableDictionary* temp = [NSMutableDictionary dictionary];
	for(NSString* key in _values)
	{
		if(![kFilteredKeys containsObject:key])
			temp[key] = _values[key];
	}

	plist::dictionary_t plist = plist::convert((__bridge CFPropertyListRef)temp);
	return to_ns(to_s(plist));
}

- (void)setPlistDump:(NSString*)aString
{
	if(CFPropertyListRef cfPlist = plist::create_cf_property_list(plist::parse_ascii(to_s(aString))))
	{
		NSMutableDictionary* oldValues = [_values copy];

		NSDictionary* dict = CFBridgingRelease(cfPlist);
		for(NSString* key in dict)
		{
			if(![kFilteredKeys containsObject:key])
				_values[key] = dict[key];
		}

		for(NSString* key in oldValues)
		{
			if(![dict objectForKey:key] && ![kFilteredKeys containsObject:key])
				_values[key] = nil;
		}
	}
}

// ============================
// = Used when saving changes =
// ============================

- (NSDictionary*)rawValues
{
	NSMutableDictionary* res = [NSMutableDictionary dictionaryWithDictionary:_values];
	res[@"uid"]             = _identifier ?: [[NSUUID UUID] UUIDString];
	res[@"kind"]            = [GenieItem stringFromKind:_kind];
	res[@"disabled"]        = _disabled ? @YES : nil;
	res[@"children"]        = _children.count ? [_children valueForKey:@"rawValues"] : nil;

	NSArray* scriptArguments = [self.mutableScriptArguments valueForKey:@"value"];
	res[@"scriptArguments"] = scriptArguments.count ? scriptArguments : nil;

	NSMutableArray* scopes = [NSMutableArray array];
	for(NSDictionary* scope in self.mdScope)
	{
		if(!scope[@"disabled"] || [scope[@"disabled"] boolValue])
		{
			[scopes addObject:scope];
		}
		else
		{
			NSMutableDictionary* dict = [scope mutableCopy];
			dict[@"disabled"] = nil;
			[scopes addObject:dict];
		}
	}
	res[@"mdScope"] = scopes.count ? scopes : nil;

	return res;
}
@end
