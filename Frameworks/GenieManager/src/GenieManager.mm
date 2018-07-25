#import "GenieItem.h"
#import "GenieUserDefaults.h"
#import <os/log.h>
#import <regexp/format_string.h>
#import <plist/plist.h>
#import <plist/ascii.h>
#import <ns/ns.h>
#import "GenieManager.h"
#import "StringImage.h"
#import "Utilities.h"
#import "GenieTask.h"
#import "hash.h"

static NSString* const kVariablesSettingsKey            = @"variables";
static NSString* const kGenieItemsDidChangeNotification = @"GenieItemsDidChangeNotification";

@interface GenieItemsDocument : NSObject
{
	NSString* _itemsPath;
	NSString* _customItemsPath;
	NSString* _defaultItemsPath;

	std::vector<dispatch_source_t> _dispatchSources;

	BOOL _needsReloadItems;
	NSTimer* _reloadItemsTimer;
}
@property (nonatomic) NSArray* observedPaths;
@property (nonatomic, readonly) NSMutableArray<GenieItem*>* items;
@end

static NSDictionary* ConvertItem (NSMutableDictionary* items, NSString* identifier)
{
	NSMutableDictionary* newValues = [items[identifier] mutableCopy];
	if(!newValues)
		return nil;

	items[identifier] = nil;

	newValues[@"uid"] = identifier;
	if(NSArray* children = newValues[@"children"])
	{
		NSMutableArray* newChildren = [NSMutableArray array];
		for(id childIdentifier in children)
		{
			if([childIdentifier isKindOfClass:[NSString class]])
			{
				if(NSDictionary* newChildItem = ConvertItem(items, childIdentifier))
					[newChildren addObject:newChildItem];
			}
			else
			{
				// This real child item has already replaced the identifier
				[newChildren addObject:childIdentifier];
			}
		}
		newValues[@"children"] = newChildren;
	}
	return newValues;
}

static NSArray* FlattenItems (NSArray<NSDictionary*>* items, NSMutableDictionary* itemRepository)
{
	NSMutableArray* res = [NSMutableArray array];
	for(NSDictionary* item in items)
	{
		if(NSString* uid = item[@"uid"])
		{
			[res addObject:uid];

			NSMutableDictionary* mutableItem = [item mutableCopy];
			mutableItem[@"uid"]      = nil;
			mutableItem[@"children"] = FlattenItems(item[@"children"], itemRepository);
			itemRepository[uid] = @{ @"values": mutableItem };
		}
	}
	return res.count ? res : nil;
}

@implementation GenieItemsDocument
- (instancetype)init
{
	if(self = [super init])
	{
		NSString* appSupport  = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES).firstObject;
		NSString* genieFolder = [appSupport stringByAppendingPathComponent:@"Genie"];
		NSString* supportPath = NSBundle.mainBundle.sharedSupportPath;

		_items            = [NSMutableArray array];
		_itemsPath        = [genieFolder stringByAppendingPathComponent:@"Items.plist"];
		_customItemsPath  = [genieFolder stringByAppendingPathComponent:@"Custom.genieItems"];
		_defaultItemsPath = [supportPath stringByAppendingPathComponent:@"Default.genieItems"];
		[self reloadItems:self];

		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(genieItemsDidChange:) name:kGenieItemsDidChangeNotification object:kGeniePrefsBundleIdentifier];
	}
	return self;
}

- (void)reloadItems:(id)sender
{
	self.needsReloadItems = NO;

	NSMutableArray* paths = [NSMutableArray array];
	if(NSDictionary* customRepository = [NSDictionary dictionaryWithContentsOfFile:_customItemsPath])
	{
		[paths addObject:_customItemsPath];

		NSDictionary* defaultItems = [NSDictionary dictionaryWithContentsOfFile:_defaultItemsPath][@"items"];
		NSDictionary* customItems  = customRepository[@"items"];

		NSMutableSet* identifiers = [NSMutableSet setWithArray:customItems.allKeys];
		[identifiers addObjectsFromArray:defaultItems.allKeys];

		NSMutableDictionary<NSString*, NSDictionary*>* values = [NSMutableDictionary dictionary];
		NSMutableDictionary<NSString*, NSNumber*>* weights    = [NSMutableDictionary dictionary];
		for(NSString* identifier in identifiers)
		{
			NSDictionary* defaultValues = defaultItems[identifier];
			NSDictionary* customValues  = customItems[identifier];

			if(NSMutableDictionary* mergedValues = [defaultValues[@"values"] mutableCopy])
			{
				if(NSDictionary* replacementValues = customValues[@"values"])
					[mergedValues addEntriesFromDictionary:replacementValues];
				for(NSString* key in customValues[@"removeValues"])
					[mergedValues removeObjectForKey:key];
				values[identifier] = mergedValues;
			}
			else if(![customValues[@"partial"] boolValue])
			{
				values[identifier] = customValues[@"values"];
			}

			weights[identifier] = customValues[@"weight"] ?: defaultValues[@"weight"];
		}

		NSMutableDictionary* rootValues = [values mutableCopy];
		for(NSString* identifier in rootValues.allKeys)
		{
			if(NSDictionary* newItem = ConvertItem(rootValues, identifier))
				rootValues[identifier] = newItem;
		}

		std::multimap<NSInteger, GenieItem*> weighted;
		for(NSString* identifier in rootValues)
		{
			NSString* path = defaultItems[identifier] ? _defaultItemsPath : _customItemsPath;
			if(GenieItem* item = [[GenieItem alloc] initWithValues:rootValues[identifier] parentItem:nil directory:[path stringByDeletingLastPathComponent]])
				weighted.emplace(weights[item.identifier].intValue, item);
		}

		NSMutableArray* items = [NSMutableArray array];
		for(auto const& pair : weighted)
			[items addObject:pair.second];

		[[self mutableArrayValueForKey:@"items"] setArray:items];
	}
	else if(NSDictionary* rawItems = [NSDictionary dictionaryWithContentsOfFile:_itemsPath])
	{
		NSMutableArray* items = [NSMutableArray array];
		[paths addObject:_itemsPath];

		for(NSDictionary* rawItem in rawItems[@"items"])
		{
			if(GenieItem* item = [[GenieItem alloc] initWithValues:rawItem parentItem:nil directory:[_itemsPath stringByDeletingLastPathComponent]])
				[items addObject:item];
		}
		[[self mutableArrayValueForKey:@"items"] setArray:items];
	}

	self.observedPaths = paths;
}

- (void)genieItemsDidChange:(NSNotification*)aNotification
{
	[self reloadItems:self];
}

- (void)reloadItemsTimerDidFire:(NSTimer*)aTimer
{
	[self reloadItems:self];
}

- (void)setNeedsReloadItems:(BOOL)flag
{
	if(_reloadItemsTimer)
	{
		[_reloadItemsTimer invalidate];
		_reloadItemsTimer = nil;
	}

	if(_needsReloadItems = flag)
		_reloadItemsTimer = [NSTimer scheduledTimerWithTimeInterval:1 target:self selector:@selector(reloadItemsTimerDidFire:) userInfo:nil repeats:NO];
}

- (void)didChangeObservedPath:(NSString*)path
{
	self.needsReloadItems = YES;
}

- (void)setObservedPaths:(NSArray*)newObservedPaths
{
	for(dispatch_source_t source : _dispatchSources)
		dispatch_source_cancel(source);
	_dispatchSources.clear();

	_observedPaths = newObservedPaths;

	for(NSString* path in _observedPaths)
	{
		int fd = open([path fileSystemRepresentation], O_EVTONLY|O_CLOEXEC);
		if(fd == -1)
		{
			os_log_error(OS_LOG_DEFAULT, "Failed to observe %{public}@: %{errno}d", path, errno);
			continue;
		}

		dispatch_source_t source = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, fd, DISPATCH_VNODE_DELETE|DISPATCH_VNODE_WRITE|DISPATCH_VNODE_EXTEND|DISPATCH_VNODE_RENAME|DISPATCH_VNODE_REVOKE, dispatch_get_main_queue());
		dispatch_source_set_cancel_handler(source, ^{
			close(fd);
		});

		__weak GenieItemsDocument* weakSelf = self;
		dispatch_source_set_event_handler(source, ^{
			[weakSelf didChangeObservedPath:path];
		});

		_dispatchSources.emplace_back(source);
		dispatch_resume(source);
	}
}

- (void)saveDocument:(id)sender
{
	BOOL const saveDelta = YES;

	self.observedPaths = nil;
	NSDictionary* const defaultItems = [NSDictionary dictionaryWithContentsOfFile:_defaultItemsPath][@"items"];

	NSMutableDictionary* allItems = [NSMutableDictionary dictionary];
	FlattenItems([_items valueForKey:@"rawValues"], allItems);

	NSMutableDictionary* repository = [NSMutableDictionary dictionary];
	for(NSString* identifier in allItems)
	{
		NSDictionary* customItem = allItems[identifier];
		if(saveDelta)
		{
			if(NSDictionary* defaultItem = defaultItems[identifier])
			{
				if([defaultItem[@"values"] isEqualToDictionary:customItem[@"values"]])
				{
					customItem = nil;
				}
				else
				{
					NSMutableDictionary* newValues = [NSMutableDictionary dictionary];
					NSMutableArray* removeValues   = [NSMutableArray array];

					NSMutableSet* keys = [NSMutableSet setWithArray:[customItem[@"values"] allKeys]];
					[keys addObjectsFromArray:[defaultItem[@"values"] allKeys]];
					for(NSString* key in keys.allObjects)
					{
						id customValue = customItem[@"values"][key];
						id defaultValue = defaultItem[@"values"][key];
						if([customValue isEqual:defaultValue])
							continue;
						else if(customValue)
							newValues[key] = customValue;
						else
							[removeValues addObject:key];
					}

					NSMutableDictionary* deltaItem = [NSMutableDictionary dictionary];
					deltaItem[@"removeValues"] = removeValues.count ? removeValues : nil;
					deltaItem[@"values"]       = newValues.count ? newValues : nil;
					deltaItem[@"partial"]      = @YES;
					customItem = deltaItem;
				}
			}
		}
		repository[identifier] = customItem;
	}

	// ==================
	// = Update Weights =
	// ==================

	NSInteger lastWeight = -1;
	for(GenieItem* item in _items)
	{
		NSNumber* weight = defaultItems[item.identifier][@"weight"];
		BOOL updateWeight = !weight || weight.intValue < lastWeight;
		if(updateWeight || !saveDelta)
		{
			if(updateWeight)
				weight = @(lastWeight + 1);

			if(NSDictionary* existingItem = repository[item.identifier])
			{
				NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:existingItem];
				dict[@"weight"] = weight;
				repository[item.identifier] = dict;
			}
			else
			{
				repository[item.identifier] = @{ @"weight": weight };
			}
		}
		lastWeight = weight.intValue;
	}

	// ==================

	NSDictionary* customItemsPlist = @{
		@"items": repository,
	};

	[NSFileManager.defaultManager createDirectoryAtPath:[_customItemsPath stringByDeletingLastPathComponent] withIntermediateDirectories:YES attributes:nil error:nullptr];
	[customItemsPlist writeToFile:_customItemsPath atomically:YES];

	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:kGenieItemsDidChangeNotification object:kGeniePrefsBundleIdentifier userInfo:nil deliverImmediately:YES];
}
@end

@interface GenieManager ()
{
	GenieItemsDocument* _document;
	NSMutableArray<GenieItem*>* _items;

	NSDictionary* _environment;
}
@end

@implementation GenieManager
+ (instancetype)sharedInstance
{
	static GenieManager* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	static NSString* const kAppleMapsURL = @"http://maps.apple.com/?q=%s";

	[self.userDefaults registerDefaults:@{
		kEnableClipboardHistorySettingsKey:      @NO,
		kDisableLaunchAtLoginSettingsKey:        @NO,
		kActivationKeyEventSettingsKey:          @"@ ",
		kClipboardHistoryExpireAfterSettingsKey: @"24 hours",
		kClipboardHistoryIgnoreAppsSettingsKey:  @[
			@{ @"bundleIdentifier": @"com.apple.keychainaccess", @"localizedName": @"Keychain Access" }
		],
		kVariablesSettingsKey:                   @[
			@{ @"name": @"EDITOR",        @"value": @"mate -w" },
			@{ @"name": @"LC_CTYPE",      @"value": @"en_US.UTF-8"                   },
			@{ @"name": @"LOGIN_SHELL",   @"value": @"${SHELL:-bash} -lc"            },
			@{ @"name": @"MAPS_URL",      @"value": kAppleMapsURL, @"disabled": @YES },
			@{ @"name": @"PATH",          @"value": @"/usr/local/bin:$PATH"          },
		],
	}];
}

+ (NSUserDefaults*)userDefaults
{
	NSString* bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];
	if([bundleIdentifier isEqualToString:kGenieBundleIdentifier])
		return [NSUserDefaults standardUserDefaults];
	return [[NSUserDefaults alloc] initWithSuiteName:kGenieBundleIdentifier];
}

- (void)runInNextEventLoopIteration:(void(^)())callback
{
	callback();
}

- (void)runAsActive:(void(^)())callback
{
	if(NSApp.isActive)
	{
		callback();
	}
	else
	{
		__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidBecomeActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
			[[NSNotificationCenter defaultCenter] removeObserver:observerId];
			[self performSelector:@selector(runInNextEventLoopIteration:) withObject:callback afterDelay:0];
		}];
		[NSApp activateIgnoringOtherApps:YES];
	}
}

- (void)runAsInactive:(void(^)())callback
{
	if(!NSApp.isActive)
	{
		callback();
	}
	else
	{
		__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:NSApplicationDidResignActiveNotification object:NSApp queue:nil usingBlock:^(NSNotification*){
			[[NSNotificationCenter defaultCenter] removeObserver:observerId];
			[self performSelector:@selector(runInNextEventLoopIteration:) withObject:callback afterDelay:0];
		}];
		[NSApp hide:self];
	}
}

- (NSBundle*)mainBundle
{
	NSString* bundleIdentifier = NSBundle.mainBundle.bundleIdentifier;
	if([bundleIdentifier isEqualToString:kGenieBundleIdentifier])
		return NSBundle.mainBundle;

	NSURL* url = [NSWorkspace.sharedWorkspace URLForApplicationWithBundleIdentifier:kGenieBundleIdentifier];
	return url ? [NSBundle bundleWithURL:url] : nil;
}

- (instancetype)init
{
	if(self = [super init])
	{
		self.variables = [GenieManager.userDefaults arrayForKey:kVariablesSettingsKey];

		_document = [[GenieItemsDocument alloc] init];
		_items = _document.items;

		NSString* bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];
		if([bundleIdentifier isEqualToString:kGenieBundleIdentifier])
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];

		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:kGeniePrefsBundleIdentifier];
	}
	return self;
}

- (NSString*)cacheFolderByAppendingPathComponent:(NSString*)aPath
{
	NSString* cacheDir = NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES).firstObject;
	cacheDir = [cacheDir stringByAppendingPathComponent:NSBundle.mainBundle.bundleIdentifier];
	if(aPath)
		cacheDir = [cacheDir stringByAppendingPathComponent:aPath];
	[NSFileManager.defaultManager createDirectoryAtPath:cacheDir withIntermediateDirectories:YES attributes:nil error:nullptr];
	return cacheDir;
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	self.variables = [GenieManager.userDefaults arrayForKey:kVariablesSettingsKey];
}

- (void)setVariables:(NSArray*)newVariables
{
	if([_variables isEqual:newVariables])
		return;

	NSMutableArray* variables = [NSMutableArray array];
	for(NSDictionary* variable in newVariables)
		[variables addObject:[variable mutableCopy]];

	_variables   = variables;
	_environment = nil;
}

- (NSDictionary*)environment
{
	if(!_environment)
	{
		std::map<std::string, std::string> map;

		NSDictionary* env = NSProcessInfo.processInfo.environment;
		for(NSString* key in env)
			map.emplace(to_s(key), to_s(env[key]));

		if(NSString* supportPath = self.mainBundle.sharedSupportPath)
			map["GENIE_SUPPORT_PATH"] = supportPath.fileSystemRepresentation;

		map["GENIE_CACHES_PATH"] = [self cacheFolderByAppendingPathComponent:nil].fileSystemRepresentation;

		for(NSDictionary* variable in [_variables filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES"]])
			map[to_s(variable[@"name"])] = format_string::expand(to_s(variable[@"value"]), map);

		NSMutableDictionary* variables = [NSMutableDictionary dictionary];
		for(auto const& pair : map)
			variables[[NSString stringWithUTF8String:pair.first.c_str()]] = [NSString stringWithUTF8String:pair.second.c_str()];
		_environment = variables;
	}
	return _environment;
}

- (BOOL)synchronize
{
	[_document saveDocument:self];

	[GenieManager.userDefaults setObject:_variables forKey:kVariablesSettingsKey];
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:kGeniePrefsBundleIdentifier userInfo:nil deliverImmediately:NO];

	return YES;
}
@end
