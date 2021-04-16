#import "OakOpenWithMenu.h"

@interface OakOpenWithApplicationInfo ()
@property (nonatomic, readwrite, getter = isDefaultApplication) BOOL defaultApplication;
@property (nonatomic, readwrite, getter = hasMultipleVersions)  BOOL multipleVersions;
@property (nonatomic, readwrite, getter = hasMultipleCopies)    BOOL multipleCopies;
@end

@implementation OakOpenWithApplicationInfo
- (instancetype)initWithBundleURL:(NSURL*)url
{
	if(self = [super init])
	{
		NSBundle* bundle = [NSBundle bundleWithURL:url];
		if(!bundle)
			return nil;

		_URL              = url;
		_bundleIdentifier = bundle.bundleIdentifier;
		_name             = [NSFileManager.defaultManager displayNameAtPath:url.filePathURL.path];
		_version          = [bundle objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: ([bundle objectForInfoDictionaryKey:@"CFBundleVersion"] ?: @"???");
	}
	return self;
}

- (NSString*)displayName
{
	NSString* name = _name;

	if(_multipleCopies)
		name = [name stringByAppendingFormat:@" (%@)", _version];
	if(_defaultApplication)
		name = [name stringByAppendingFormat:@" (default)"];
	if(_multipleVersions)
		name = [name stringByAppendingFormat:@" — %@", [[_URL.filePathURL.path stringByDeletingLastPathComponent] stringByAbbreviatingWithTildeInPath]];

	return name;
}
@end

@interface OakOpenWithMenuDelegate ()
{
	NSArray<OakOpenWithApplicationInfo*>* _applications;
}
@end

static NSURL* CanonicalURL (NSURL* url, BOOL isDirectoryFlag = YES)
{
	if(NSString* path = url.filePathURL.path)
		return [NSURL fileURLWithPath:path isDirectory:isDirectoryFlag];
	return url;
}

@implementation OakOpenWithMenuDelegate
- (instancetype)initWithDocumentURLs:(NSArray<NSURL*>*)someDocumentURLs
{
	if(self = [super init])
	{
		_documentURLs = someDocumentURLs;
	}
	return self;
}

- (NSArray<OakOpenWithApplicationInfo*>*)applications
{
	if(!_applications)
	{
		NSMutableSet<NSURL*>* allAppURLs;
		NSMutableSet<NSURL*>* defaultAppURLs = [NSMutableSet set];

		for(NSURL* documentURL in _documentURLs)
		{
			NSMutableSet<NSURL*>* appURLs = [NSMutableSet set];
			NSArray<NSURL*>* appURLsArray = (NSArray*)CFBridgingRelease(LSCopyApplicationURLsForURL((__bridge CFURLRef)documentURL, kLSRolesAll));
			for(NSURL* appURL in appURLsArray)
				[appURLs addObject:CanonicalURL(appURL)];

			if(NSURL* defaultAppURL = CanonicalURL([NSWorkspace.sharedWorkspace URLForApplicationToOpenURL:documentURL]))
			{
				[appURLs addObject:defaultAppURL];
				[defaultAppURLs addObject:defaultAppURL];
			}

			if(allAppURLs)
					[allAppURLs intersectSet:appURLs];
			else	allAppURLs = appURLs;
		}

		NSMutableArray<OakOpenWithApplicationInfo*>* apps = [NSMutableArray array];
		NSCountedSet* counts = [[NSCountedSet alloc] initWithCapacity:allAppURLs.count * 2];

		for(NSURL* appURL in allAppURLs)
		{
			if(OakOpenWithApplicationInfo* app = [[OakOpenWithApplicationInfo alloc] initWithBundleURL:appURL])
			{
				app.defaultApplication = defaultAppURLs.count == 1 && [defaultAppURLs containsObject:appURL];
				[apps addObject:app];

				NSString* nameWithVersion = [NSString stringWithFormat:@"%@ (%@)", app.name, app.version];
				[counts addObject:app.name];
				[counts addObject:nameWithVersion];
			}
		}

		for(OakOpenWithApplicationInfo* app in apps)
		{
			NSString* nameWithVersion = [NSString stringWithFormat:@"%@ (%@)", app.name, app.version];
			app.multipleVersions = [counts countForObject:nameWithVersion] > 1;
			app.multipleCopies   = [counts countForObject:app.name] > [counts countForObject:nameWithVersion];
		}

		_applications = [apps sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"defaultApplication" ascending:NO], [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)], [NSSortDescriptor sortDescriptorWithKey:@"version" ascending:NO] ]];
	}
	return _applications;
}

- (void)openDocumentURLs:(NSArray<NSURL*>*)documentURLs withApplicationURL:(NSURL*)applicationURL
{
	// Since we can have multiple applications for the same bundle identifier, e.g. Xcode release and beta, we must open by URL.
	// Unfortunately the API that is URL-based does not allow opening multiple documents at once, so we use AppleScript.

	NSAppleEventDescriptor* listDesc = [NSAppleEventDescriptor listDescriptor];
	NSInteger nextIndex = 1;

	for(NSURL* url in documentURLs)
	{
		if(NSData* urlData = [url.absoluteString dataUsingEncoding:NSUTF8StringEncoding])
		{
			if(NSAppleEventDescriptor* urlDesc = [NSAppleEventDescriptor descriptorWithDescriptorType:typeFileURL data:urlData])
				[listDesc insertDescriptor:urlDesc atIndex:nextIndex++];
		}
	}

	NSAppleEventDescriptor* odocEvent = [NSAppleEventDescriptor appleEventWithEventClass:kCoreEventClass eventID:kAEOpenDocuments targetDescriptor:nil returnID:kAutoGenerateReturnID transactionID:kAnyTransactionID];
	[odocEvent setParamDescriptor:listDesc forKeyword:keyDirectObject];
	NSDictionary* launchOptions = @{ NSWorkspaceLaunchConfigurationAppleEvent: odocEvent };

	NSError* err = nil;
	if(![NSWorkspace.sharedWorkspace launchApplicationAtURL:applicationURL options:NSWorkspaceLaunchDefault configuration:launchOptions error:&err])
		NSLog(@"%@: %@", applicationURL, err.localizedDescription);
}

// ==========================
// = MenuItem Action Method =
// ==========================

- (void)openWith:(id)sender
{
	[self openDocumentURLs:_documentURLs withApplicationURL:[sender representedObject]];
}

// ==========================
// = NSMenuDelegate Methods =
// ==========================

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	[menu removeAllItems];

	NSArray<OakOpenWithApplicationInfo*>* apps = self.applications;
	if(apps.count == 0)
	{
		[menu addItemWithTitle:@"No Suitable Applications Found" action:@selector(nop:) keyEquivalent:@""];
	}
	else
	{
		BOOL didInsertDefaultItems = NO;
		for(OakOpenWithApplicationInfo* app : apps)
		{
			if(didInsertDefaultItems && app.isDefaultApplication == NO)
				[menu addItem:[NSMenuItem separatorItem]];
			didInsertDefaultItems = app.isDefaultApplication;

			NSMenuItem* menuItem = [menu addItemWithTitle:app.displayName action:@selector(openWith:) keyEquivalent:@""];
			menuItem.target            = self;
			menuItem.representedObject = app.URL;
			menuItem.toolTip           = [app.URL.filePathURL.path stringByAbbreviatingWithTildeInPath];

			NSImage* image = nil;
			if([app.URL getResourceValue:&image forKey:NSURLEffectiveIconKey error:NULL] || (image = [NSWorkspace.sharedWorkspace iconForFile:app.URL.filePathURL.path]))
			{
				image = [image copy];
				image.size = NSMakeSize(16, 16);
				menuItem.image = image;
			}
		}
	}
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)theEvent target:(id*)aTarget action:(SEL*)anAction
{
	return NO;
}
@end
