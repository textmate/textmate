#import "OakOpenWithMenu.h"
#import "NSMenu Additions.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/oak.h>

static NSURL* CanonicalURL (NSURL* url, BOOL isDirectoryFlag = YES)
{
	return [NSURL fileURLWithPath:[[url filePathURL] path] isDirectory:isDirectoryFlag];
}

static NSArray* ApplicationURLsForPaths (NSSet* paths)
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

		CFURLRef defaultAppURL = nil;
		if(noErr == LSGetApplicationForURL((__bridge CFURLRef)url, kLSRolesAll, NULL, &defaultAppURL))
		{
			NSURL* tmp = CanonicalURL((__bridge NSURL*)defaultAppURL);
			[appUrls addObject:tmp];
			[defaultAppURLs addObject:tmp];

			CFRelease(defaultAppURL);
		}

		if(std::exchange(first, false))
				[allAppURLs setSet:appUrls];
		else	[allAppURLs intersectSet:appUrls];
	}

	NSMutableArray* apps        = [NSMutableArray array];
	NSMutableDictionary* counts = [NSMutableDictionary dictionary];

	for(NSURL* url in allAppURLs)
	{
		if(NSBundle* bundle = [NSBundle bundleWithURL:url])
		{
			NSString* identifier = [bundle bundleIdentifier];
			counts[identifier] = @([counts[identifier] intValue] + 1);
			[apps addObject:@{
				@"identifier" : identifier,
				@"name"       : [bundle objectForInfoDictionaryKey:@"CFBundleName"],
				@"version"    : [bundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: ([bundle objectForInfoDictionaryKey:@"CFBundleVersion"] ?: @"???"),
				@"url"        : url,
				@"isDefault"  : @([defaultAppURLs containsObject:url]),
			}];
		}
	}

	NSMutableArray* res = [NSMutableArray array];
	for(NSDictionary* app in [apps sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"isDefault" ascending:NO], [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)], [NSSortDescriptor sortDescriptorWithKey:@"version" ascending:NO] ]])
	{
		NSString* name = app[@"name"];
		if([counts[app[@"identifier"]] intValue] > 1)
			name = [NSString stringWithFormat:@"%@ (%@)", name, app[@"version"]];
		if([app[@"isDefault"] boolValue] && [defaultAppURLs count] == 1)
			name = [NSString stringWithFormat:@"%@ (default)", name];
		[res addObject:@{ @"name" : name, @"url" : app[@"url"], @"isDefault" : app[@"isDefault"] }];
	}
	return [res count] == 0 ? nil : res;
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

	NSArray* apps = ApplicationURLsForPaths(superItem.representedObject);
	if(!apps)
	{
		[menu addItemWithTitle:@"No Suitable Applications Found" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	BOOL didInsertDefaultItems = NO;
	for(NSDictionary* app : apps)
	{
		if(didInsertDefaultItems && ![app[@"isDefault"] boolValue])
			[menu addItem:[NSMenuItem separatorItem]];
		didInsertDefaultItems = [app[@"isDefault"] boolValue];

		NSMenuItem* menuItem = [menu addItemWithTitle:app[@"name"] action:@selector(openWith:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:app[@"url"]];
		[menuItem setToolTip:[[[app[@"url"] filePathURL] path] stringByAbbreviatingWithTildeInPath]];

		NSImage* image = nil;
		if([app[@"url"] getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL] || (image = [[NSWorkspace sharedWorkspace] iconForFile:[app[@"url"] path]]))
		{
			image = [image copy];
			image.size = NSMakeSize(16, 16);
			[menuItem setImage:image];
		}
	}
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
