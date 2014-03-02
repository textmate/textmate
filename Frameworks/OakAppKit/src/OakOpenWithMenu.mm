#import "OakOpenWithMenu.h"
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
		CFURLRef fileURL = (CFURLRef)CFBridgingRetain([NSURL fileURLWithPath:path]);
		NSArray* applicationURLs = (NSArray*)CFBridgingRelease(LSCopyApplicationURLsForURL(fileURL, kLSRolesAll));
		CFURLRef defaultApplicationURL = nil;
		if(noErr == LSGetApplicationForURL(fileURL, kLSRolesAll, NULL, &defaultApplicationURL))
		{
			NSURL* defaultAppURL = (NSURL*)CFBridgingRelease(defaultApplicationURL);
			if(![applicationURLs containsObject:defaultAppURL])
				applicationURLs = [applicationURLs arrayByAddingObject:defaultAppURL];
			[defaultApplicationURLs addObject:defaultAppURL];
		}
		if(allApplicationURLs.count == 0)
				[allApplicationURLs setSet:[NSSet setWithArray:applicationURLs]];
		else	[allApplicationURLs intersectSet:[NSSet setWithArray:applicationURLs]];
		CFRelease(fileURL);
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
	for(auto const& appIter : apps)
	{
		riterate(versIter, appIter.second)
		{
			NSString* appName = [NSString stringWithCxxString:appIter.first];
			NSURL* appURL = versIter->second;

			if([defaultApplicationURL isEqual:appURL])
				appName = [NSString stringWithFormat:@"%@ (default)", appName];

			if(appIter.second.size() > 1) // we have more than one version
				appName = [NSString stringWithFormat:@"%@ (%@)", appName, [NSString stringWithCxxString:versIter->first]];

			res.insert(([defaultApplicationURL isEqual:appURL] ? res.begin() : res.end()), std::make_pair(appName, appURL));
		}
	}
	return res;
}

@implementation OakOpenWithMenu
+ (id)sharedInstance
{
	static OakOpenWithMenu* instance = [OakOpenWithMenu new];
	return instance;
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

	std::vector< std::pair<NSString*, NSURL*> > appURLs = ApplicationURLsForPaths(superItem.representedObject);

	if(appURLs.empty())
	{
		[menu addItemWithTitle:@"No Suitable Applications Found" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	for(auto const& app : appURLs)
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:app.first action:@selector(openWith:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:app.second];

		NSImage* image = nil;
		if([app.second getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL] || (image = [[NSWorkspace sharedWorkspace] iconForFile:[app.second path]]))
		{
			image = [image copy];
			image.size = NSMakeSize(16, 16);
			[menuItem setImage:image];
		}
	}

	if(appURLs.size() > 1)
		[menu insertItem:[NSMenuItem separatorItem] atIndex:1];
}

- (void)openWith:(id)sender
{
	NSURL* applicationURL = [sender representedObject];
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
