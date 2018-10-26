#import "OakOpenWithMenu.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>

@interface OakOpenWithApplicationInfo : NSObject
@property (nonatomic) NSURL* url;
@property (nonatomic) NSString* identifier;
@property (nonatomic) NSString* name;
@property (nonatomic) NSString* version;
@property (nonatomic, getter = isDefaultApplication) BOOL defaultApplication;

@property (nonatomic) NSUInteger countOfDuplicateNames;
@property (nonatomic) NSUInteger countOfDuplicateVersions;

@property (nonatomic, readonly) NSString* nameWithVersion;
@property (nonatomic, readonly) NSString* displayName;
@end

@implementation OakOpenWithApplicationInfo
- (instancetype)initWithBundleURL:(NSURL*)url
{
	if(self = [super init])
	{
		NSBundle* bundle = [NSBundle bundleWithURL:url];
		if(!bundle)
			return nil;

		_identifier = bundle.bundleIdentifier;
		if(!_identifier)
		{
			NSLog(@"warning: missing CFBundleIdentifier: %@", bundle);
			return nil;
		}

		_url     = url;
		_name    = [[NSFileManager defaultManager] displayNameAtPath:url.filePathURL.path];
		_version = [bundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: ([bundle objectForInfoDictionaryKey:@"CFBundleVersion"] ?: @"???");
	}
	return self;
}

- (NSString*)nameWithVersion
{
	return [NSString stringWithFormat:@"%@ (%@)", _name, _version];
}

- (NSString*)displayName
{
	NSString* name = _name;

	if(_countOfDuplicateNames > _countOfDuplicateVersions)
		name = [name stringByAppendingFormat:@" (%@)", _version];
	if(_defaultApplication)
		name = [name stringByAppendingFormat:@" (default)"];
	if(_countOfDuplicateVersions > 1)
		name = [name stringByAppendingFormat:@" — %@", [[_url.filePathURL.path stringByDeletingLastPathComponent] stringByAbbreviatingWithTildeInPath]];

	return name;
}
@end

static NSURL* CanonicalURL (NSURL* url, BOOL isDirectoryFlag = YES)
{
	if(NSString* path = [[url filePathURL] path])
		return [NSURL fileURLWithPath:path isDirectory:isDirectoryFlag];
	return url;
}

static NSArray<OakOpenWithApplicationInfo*>* ApplicationURLsForPaths (NSSet* paths)
{
	NSMutableSet* allAppURLs     = [NSMutableSet set];
	NSMutableSet* defaultAppURLs = [NSMutableSet set];

	bool first = true;
	for(NSString* path in paths)
	{
		NSURL* url = [NSURL fileURLWithPath:path];

		NSMutableSet* appUrls = [NSMutableSet set];
		NSArray* appUrlsArray = (NSArray*)CFBridgingRelease(LSCopyApplicationURLsForURL((__bridge CFURLRef)url, kLSRolesAll));
		for(NSURL* url in appUrlsArray)
			[appUrls addObject:CanonicalURL(url)];

		if(NSURL* defaultAppURL = CanonicalURL([[NSWorkspace sharedWorkspace] URLForApplicationToOpenURL:url]))
		{
			[appUrls addObject:defaultAppURL];
			[defaultAppURLs addObject:defaultAppURL];
		}

		if(std::exchange(first, false))
				[allAppURLs setSet:appUrls];
		else	[allAppURLs intersectSet:appUrls];
	}

	NSMutableArray<OakOpenWithApplicationInfo*>* apps = [NSMutableArray array];
	NSCountedSet* counts = [[NSCountedSet alloc] initWithCapacity:allAppURLs.count * 2];

	for(NSURL* url in allAppURLs)
	{
		if(OakOpenWithApplicationInfo* info = [[OakOpenWithApplicationInfo alloc] initWithBundleURL:url])
		{
			info.defaultApplication = defaultAppURLs.count == 1 && [defaultAppURLs containsObject:url];
			[apps addObject:info];

			[counts addObject:info.name];
			[counts addObject:info.nameWithVersion];
		}
	}

	for(OakOpenWithApplicationInfo* app in apps)
	{
		app.countOfDuplicateNames    = [counts countForObject:app.name];
		app.countOfDuplicateVersions = [counts countForObject:app.nameWithVersion];
	}

	return [apps count] == 0 ? nil : [apps sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"defaultApplication" ascending:NO], [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)], [NSSortDescriptor sortDescriptorWithKey:@"version" ascending:NO] ]];
}

@interface OakOpenWithMenu () <NSMenuDelegate>
@end

@implementation OakOpenWithMenu
+ (instancetype)sharedInstance
{
	static OakOpenWithMenu* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)addOpenWithMenuForPaths:(NSSet*)paths toMenuItem:(NSMenuItem*)item
{
	NSMenu* submenu = [NSMenu new];
	[submenu setDelegate:[OakOpenWithMenu sharedInstance]];

	[item setRepresentedObject:paths];
	[item setSubmenu:submenu];
}

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	NSMenuItem* superItem = [menu parentMenuItem];
	if(menu.numberOfItems > 0 || !superItem)
		return;

	NSArray<OakOpenWithApplicationInfo*>* apps = ApplicationURLsForPaths(superItem.representedObject);
	if(!apps)
	{
		[menu addItemWithTitle:@"No Suitable Applications Found" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	BOOL didInsertDefaultItems = NO;
	for(OakOpenWithApplicationInfo* app : apps)
	{
		if(didInsertDefaultItems && app.isDefaultApplication == NO)
			[menu addItem:[NSMenuItem separatorItem]];
		didInsertDefaultItems = app.isDefaultApplication;

		NSMenuItem* menuItem = [menu addItemWithTitle:app.displayName action:@selector(openWith:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:app.url];
		[menuItem setToolTip:[app.url.filePathURL.path stringByAbbreviatingWithTildeInPath]];

		NSImage* image = nil;
		if([app.url getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL] || (image = [[NSWorkspace sharedWorkspace] iconForFile:app.url.filePathURL.path]))
		{
			image = [image copy];
			image.size = NSMakeSize(16, 16);
			[menuItem setImage:image];
		}
	}
}

- (void)openWith:(id)sender
{
	// Since we can have multiple applications for the same bundle identifier, e.g. Xcode release and beta, we must open by URL.
	// Unfortunately the API that is URL-based does not allow opening multiple documents at once, so we use AppleScript.

	NSURL* applicationURL = [sender representedObject];
	NSSet* filePaths = [[[sender menu] parentMenuItem] representedObject];

	NSAppleEventDescriptor* listDesc = [NSAppleEventDescriptor listDescriptor];
	NSInteger nextIndex = 1;

	for(NSString* filePath in filePaths)
	{
		if(NSURL* url = [NSURL fileURLWithPath:filePath])
		{
			if(NSData* urlData = [url.absoluteString dataUsingEncoding:NSUTF8StringEncoding])
			{
				if(NSAppleEventDescriptor* urlDesc = [NSAppleEventDescriptor descriptorWithDescriptorType:typeFileURL data:urlData])
					[listDesc insertDescriptor:urlDesc atIndex:nextIndex++];
			}
		}
	}

	NSAppleEventDescriptor* odocEvent = [NSAppleEventDescriptor appleEventWithEventClass:kCoreEventClass eventID:kAEOpenDocuments targetDescriptor:nil returnID:kAutoGenerateReturnID transactionID:kAnyTransactionID];
	[odocEvent setParamDescriptor:listDesc forKeyword:keyDirectObject];
	NSDictionary* launchOptions = @{ NSWorkspaceLaunchConfigurationAppleEvent: odocEvent };

	NSError* err = nil;
	if(![[NSWorkspace sharedWorkspace] launchApplicationAtURL:applicationURL options:NSWorkspaceLaunchDefault configuration:launchOptions error:&err])
		NSLog(@"%@: %@", applicationURL, err.localizedDescription);
}
@end
