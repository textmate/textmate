#import "OakOpenWithMenu.h"
#import "NSMenuItem Additions.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>
#import <text/ctype.h>
#import <io/path.h>

static std::vector< std::pair<NSString*, NSURL*> > ApplicationURLsForPaths (NSSet* paths)
{
	std::vector< std::pair<NSString*, NSURL*> > res;
	NSMutableSet* defaultApplicationURLs = [NSMutableSet set];
	NSMutableSet* allApplicationURLs = [NSMutableSet set];

	for(NSString* path in paths)
	{
		NSURL* fileURL = [NSURL fileURLWithPath:path];
		NSArray* applicationURLs = [(NSArray*)LSCopyApplicationURLsForURL((CFURLRef)fileURL, kLSRolesAll) autorelease];
		NSURL* defaultApplicationURL = nil;
		if(noErr == LSGetApplicationForURL((CFURLRef)fileURL, kLSRolesAll, NULL, (CFURLRef*)&defaultApplicationURL))
		{
			if(![applicationURLs containsObject:defaultApplicationURL])
				applicationURLs = [applicationURLs arrayByAddingObject:defaultApplicationURL];
			[defaultApplicationURLs addObject:defaultApplicationURL];
		}
		if(allApplicationURLs.count == 0)
				[allApplicationURLs setSet:[NSSet setWithArray:applicationURLs]];
		else	[allApplicationURLs intersectSet:[NSSet setWithArray:applicationURLs]];
	}

	if(allApplicationURLs.count == 0)
		return res;

	std::map<std::string, std::map<std::string, NSURL*>, text::less_t> apps;
	for(NSURL* appURL in allApplicationURLs)
	{
		std::string const& appName = path::display_name(appURL.path.UTF8String);
		NSBundle* appBundle        = [NSBundle bundleWithPath:appURL.path];
		NSString* appVersion       = [appBundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: ([appBundle objectForInfoDictionaryKey:@"CFBundleVersion"] ?: @"???");
		apps[appName][appVersion.UTF8String] = appURL;
	}

	NSURL* defaultApplicationURL = defaultApplicationURLs.count == 1 ? defaultApplicationURLs.anyObject : nil;
	iterate(appIter, apps)
	{
		riterate(versIter, appIter->second)
		{
			NSString* appName = [NSString stringWithCxxString:appIter->first];
			NSURL* appURL = versIter->second;

			if([defaultApplicationURL isEqual:appURL])
				appName = [NSString stringWithFormat:@"%@ (default)", appName];

			if(appIter->second.size() > 1) // we have more than one version
				appName = [NSString stringWithFormat:@"%@ (%@)", appName, [NSString stringWithCxxString:versIter->first]];

			res.insert(([defaultApplicationURL isEqual:appURL] ? res.begin() : res.end()), std::make_pair(appName, appURL));
		}
	}
	return res;
}

static OakOpenWithMenu* SharedInstance;

@implementation OakOpenWithMenu
+ (id)sharedInstance
{
	return SharedInstance ?: [[self new] autorelease];
}

- (id)init
{
	if(SharedInstance)
			[self release];
	else	self = SharedInstance = [[super init] retain];
	return SharedInstance;
}

+ (void)addOpenWithMenuForPaths:(NSSet*)paths toMenuItem:(NSMenuItem*)item
{
	NSMenu* submenu = [[NSMenu new] autorelease];
	[submenu setAutoenablesItems:NO];
	[submenu setDelegate:[OakOpenWithMenu sharedInstance]];

	[item setRepresentedObject:paths];
	[item setSubmenu:submenu];
}

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	NSMenuItem* superItem = [menu parentMenuItem];
	if(menu.numberOfItems > 0 || !superItem)
		return;

	std::vector< std::pair<NSString*, NSURL*> > appURLs = ApplicationURLsForPaths(superItem.representedObject);

	if(appURLs.empty())
	{
		[[menu addItemWithTitle:@"No Suitable Applications Found" action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];
		return;
	}

	iterate(app, appURLs)
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:app->first action:@selector(openWith:) keyEquivalent:@""];
		[menuItem setIconForFile:[app->second path]];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:app->second];
	}

	if(appURLs.size() > 1)
		[menu insertItem:[NSMenuItem separatorItem] atIndex:1];
}

- (void)openWith:(id)sender
{
	NSURL* applicationURL   = [sender representedObject];
	NSSet* filePaths = [[[sender menu] parentMenuItem] representedObject];

	NSMutableArray* fileURLs = [NSMutableArray arrayWithCapacity:filePaths.count];
	for(NSString* filePath in filePaths)
		[fileURLs addObject:[NSURL fileURLWithPath:filePath]];
	[[NSWorkspace sharedWorkspace] openURLs:fileURLs
                   withAppBundleIdentifier:[[NSBundle bundleWithPath:applicationURL.path] bundleIdentifier]
                                   options:0
            additionalEventParamDescriptor:NULL
                         launchIdentifiers:NULL];
}
@end
