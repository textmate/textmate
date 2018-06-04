#import "GenieItem.h"
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

static NSString* const kGenieIdentifier                 = @"com.macromates.Genie";
static NSString* const kVariablesSettingsKey            = @"variables";
static NSString* const kGenieItemsDidChangeNotification = @"GenieItemsDidChangeNotification";

@interface GenieManager ()
{
	NSString* _itemsPath;
	NSMutableArray<GenieItem*>* _items;

	NSDictionary* _environment;
	std::vector<dispatch_source_t> _dispatchSources;

	BOOL _needsReloadItems;
	NSTimer* _reloadItemsTimer;
}
@property (nonatomic) NSArray* observedPaths;
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
		kVariablesSettingsKey: @[
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
	if([bundleIdentifier isEqualToString:kGenieIdentifier])
		return [NSUserDefaults standardUserDefaults];
	return [[NSUserDefaults alloc] initWithSuiteName:kGenieIdentifier];
}

- (NSBundle*)mainBundle
{
	NSString* bundleIdentifier = NSBundle.mainBundle.bundleIdentifier;
	if([bundleIdentifier isEqualToString:kGenieIdentifier])
		return NSBundle.mainBundle;

	NSURL* url = [NSWorkspace.sharedWorkspace URLForApplicationWithBundleIdentifier:kGenieIdentifier];
	return url ? [NSBundle bundleWithURL:url] : nil;
}

- (instancetype)init
{
	if(self = [super init])
	{
		self.variables = [GenieManager.userDefaults arrayForKey:kVariablesSettingsKey];

		NSString* appSupport  = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES).firstObject;
		NSString* genieFolder = [appSupport stringByAppendingPathComponent:@"Genie"];

		_items     = [NSMutableArray array];
		_itemsPath = [genieFolder stringByAppendingPathComponent:@"Items.plist"];
		[self reloadItems:self];

		NSString* bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];
		if([bundleIdentifier isEqualToString:kGenieIdentifier])
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];

		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:@"com.macromates.GeniePrefs"];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(genieItemsDidChange:) name:kGenieItemsDidChangeNotification object:@"com.macromates.GeniePrefs"];
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

- (void)reloadItems:(id)sender
{
	self.needsReloadItems = NO;

	NSMutableArray* items = [NSMutableArray array];
	NSMutableArray* paths = [NSMutableArray array];

	if(NSDictionary* rawItems = [NSDictionary dictionaryWithContentsOfFile:_itemsPath])
	{
		[paths addObject:_itemsPath];

		for(NSDictionary* rawItem in rawItems[@"items"])
		{
			if(GenieItem* item = [[GenieItem alloc] initWithValues:rawItem parentItem:nil directory:[_itemsPath stringByDeletingLastPathComponent]])
				[items addObject:item];
		}
	}

	[[self mutableArrayValueForKey:@"items"] setArray:items];

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

		dispatch_source_set_event_handler(source, ^{
			[GenieManager.sharedInstance didChangeObservedPath:path];
		});

		_dispatchSources.emplace_back(source);
		dispatch_resume(source);
	}
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
	self.observedPaths = nil;

	// ==================================
	// = Write all items to single file =
	// ==================================

	NSMutableArray* items = [NSMutableArray array];
	for(GenieItem* item in _items)
		[items addObject:item.rawValues];

	NSDictionary* itemsPlist = @{
		@"items": items
	};

	[NSFileManager.defaultManager createDirectoryAtPath:[_itemsPath stringByDeletingLastPathComponent] withIntermediateDirectories:YES attributes:nil error:nullptr];
	[itemsPlist writeToFile:_itemsPath atomically:YES];

	[GenieManager.userDefaults setObject:_variables forKey:kVariablesSettingsKey];
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:@"com.macromates.GeniePrefs" userInfo:nil deliverImmediately:NO];

	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:kGenieItemsDidChangeNotification object:@"com.macromates.GeniePrefs" userInfo:nil deliverImmediately:YES];

	return YES;
}
@end
