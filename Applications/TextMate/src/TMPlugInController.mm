#import "TMPlugInController.h"
#import <oak/debug.h>

OAK_DEBUG_VAR(PlugInController);

static TMPlugInController* SharedInstance;

@protocol TMWindow
- (BOOL)addStatusBarCell:(NSCell*)aCell;
- (BOOL)removeStatusBarCell:(NSCell*)aCell;
@end

@interface NSObject (TMFileOperationProtocol)
- (void)willOpenURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;
- (void)didOpenURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;

- (void)willSaveToURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;
- (void)didSaveToURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;

- (void)willCloseURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;
- (void)didCloseURL:(NSURL*)anURL inWindow:(id <TMWindow>)aWindow;
@end

@protocol TMPlugInController
- (float)version;
- (void)registerFileOperationObserver:(id)anObserver;
- (void)unregisterFileOperationObserver:(id)anObserver;
@end

@interface NSObject (TMPlugInClass)
- (id)initWithPlugInController:(TMPlugInController*)aController;
@end

@interface TMPlugIn : NSObject
{
	NSBundle* plugInBundle;
	id instance;
}
+ (TMPlugIn*)plugInWithPath:(NSString*)aPath;
@end

@implementation TMPlugIn
- (TMPlugIn*)initWithPath:(NSString*)aPath
{
	D(DBF_PlugInController, bug("%s\n", [aPath UTF8String]););
	if(plugInBundle = [NSBundle bundleWithPath:aPath])
	{
		if(self = [super init])
			[plugInBundle retain];
	}
	else
	{
		NSLog(@"%s couldn't load plugIn %@", sel_getName(_cmd), aPath);
		[self dealloc];
		self = nil;
	}
	return self;
}

+ (TMPlugIn*)plugInWithPath:(NSString*)aPath
{
	return [[[self alloc] initWithPath:aPath] autorelease];
}

- (void)dealloc
{
	D(DBF_PlugInController, bug("\n"););
	[plugInBundle release];
	[super dealloc];
}

- (NSString*)name
{
	return [plugInBundle objectForInfoDictionaryKey:@"CFBundleName"];
}

- (NSString*)bundleIdentifier
{
	return [plugInBundle objectForInfoDictionaryKey:@"CFBundleIdentifier"];
}

- (int)undocumentedRelianceVersion
{
	return [[plugInBundle objectForInfoDictionaryKey:@"ReliesOnClassesFromVersion"] intValue];
}

- (id)instance
{
	if(!instance)
	{
		[plugInBundle load];
		id obj = [[plugInBundle principalClass] alloc];
		if(!obj)
			NSLog(@"%s %@ plug-in has no principal class", sel_getName(_cmd), [self name]);
		else if([obj respondsToSelector:@selector(initWithPlugInController:)])
			instance = [obj initWithPlugInController:[TMPlugInController sharedInstance]];
		else
			NSLog(@"%s %@ plug-in doesn't have proper initializer", sel_getName(_cmd), [self name]);
	}
	D(DBF_PlugInController, bug("%s\n", [[instance description] UTF8String]););
	return instance;
}
@end

@implementation TMPlugInController
+ (TMPlugInController*)sharedInstance
{
	return SharedInstance ?: [[TMPlugInController new] autorelease];
}

- (id)init
{
	if(SharedInstance)
	{
		[self release];
	}
	else if(self = SharedInstance = [[super init] retain])
	{
		D(DBF_PlugInController, bug("\n"););
		loadedPlugIns = [NSMutableArray new];
		plugInBundleIdentifiers = [NSMutableSet new];
	}
	return SharedInstance;
}

- (float)version
{
	return 2.0;
}

- (int)lastMajorInternalChange
{
	return 1700;
}

- (void)loadPlugIn:(NSString*)aPath
{
	if(TMPlugIn* plugIn = [TMPlugIn plugInWithPath:aPath])
	{
		if(![[plugIn bundleIdentifier] hasPrefix:@"com.macromates"])
		{
			NSLog(@"Skip loading plug-in: %@ (%@)", [plugIn bundleIdentifier], aPath);
			return;
		}

		if(![plugIn undocumentedRelianceVersion] || [plugIn undocumentedRelianceVersion] >= [self lastMajorInternalChange])
		{
			NSString* bundleIdentifier = [plugIn bundleIdentifier];
			if(bundleIdentifier && ![plugInBundleIdentifiers containsObject:bundleIdentifier])
			{
				[loadedPlugIns addObject:plugIn];
				[plugInBundleIdentifiers addObject:bundleIdentifier];
				[plugIn instance];
			}
		}
		else
		{
			NSLog(@"%s %@ plug-in was not loaded as it relies on version %d", sel_getName(_cmd), [plugIn name], [plugIn undocumentedRelianceVersion]);
		}
	}
	else
	{
		NSLog(@"%s failed to load %@", sel_getName(_cmd), aPath);
	}
}
#if 0
- (NSString*)installPath
{
	NSArray* libraryPaths = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES);
	return [NSString pathWithComponents:@[ [libraryPaths firstObject], [[NSProcessInfo processInfo] processName], @"PlugIns" ]];
}

- (void)installPlugIn:(NSString*)aPath
{
	NSString* installAs = [[self installPath] stringByAppendingPathComponent:[aPath lastPathComponent]];
	if([aPath isEqualToString:installAs])
		return;

	id plugInName = [[NSBundle bundleWithPath:aPath] objectForInfoDictionaryKey:@"CFBundleName"] ?: [[installAs lastPathComponent] stringByDeletingPathExtension];
	if([installAs existsAsPath])
	{
		id newVersion = [[NSBundle bundleWithPath:aPath] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: [[NSBundle bundleWithPath:aPath] objectForInfoDictionaryKey:@"CFBundleVersion"];
		id oldVersion = [[NSBundle bundleWithPath:installAs] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: [[NSBundle bundleWithPath:installAs] objectForInfoDictionaryKey:@"CFBundleVersion"];
		int choice = NSRunAlertPanel(@"Plug-in Already Installed", @"Version %@ of “%@” is already installed.\nDo you want to replace it with version %@?\n\nUpgrading a plug-in will require TextMate to be relaunched.", @"Replace", @"Cancel", nil, oldVersion ?: @"???", plugInName, newVersion ?: @"???");
		if(choice == NSAlertDefaultReturn) // "Replace"
		{
			if(![installAs moveFileToTrash])
			{
				NSRunAlertPanel(@"Install Failed", @"Couldn't remove old plug-in (“%@”)", @"Continue", nil, nil, [installAs stringByAbbreviatingWithTildeInPath]);
				installAs = nil;
			}
		}
		else if(choice == NSAlertAlternateReturn) // "Cancel"
		{
			installAs = nil;
		}
	}

	if(installAs)
	{
		if([[self installPath] canCreateAsDirectory])
		{
			NSFileManager* fm = [NSFileManager defaultManager];
			BOOL res = [fm isDeletableFileAtPath:aPath] ? [fm movePath:aPath toPath:installAs handler:nil] : [fm copyPath:aPath toPath:installAs handler:nil];
			if(res && didLoadAllPlugIns)
			{
				int choice = NSRunAlertPanel(@"Plug-in Installed", @"To activate “%@” you will need to relaunch TextMate.", @"Relaunch", @"Cancel", nil, plugInName);
				if(choice == NSAlertDefaultReturn) // "Relaunch"
					[OakSelfUpdate restart];
			}
			else if(!res)
			{
				NSRunAlertPanel(@"Install Failed", @"The plug-in has not been installed.", @"Continue", nil, nil);
			}
		}
		else
		{
			NSRunAlertPanel(@"Install Failed", @"It was not possible to create the plug-in folder (“%@”)", @"Continue", nil, nil, [[self installPath] stringByAbbreviatingWithTildeInPath]);
		}
	}
}
#endif
- (void)loadAllPlugIns:(id)sender
{
	NSMutableArray* array = [NSMutableArray array];

	NSArray* appSupportPaths = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSAllDomainsMask, YES);
	NSString* subPath = [NSString pathWithComponents:@[ @"TextMate", @"PlugIns" ]];
	for(NSString* path in appSupportPaths)
		[array addObject:[path stringByAppendingPathComponent:subPath]];
	[array addObject:[[NSBundle mainBundle] builtInPlugInsPath]];

	for(NSString* path in array)
	{
		D(DBF_PlugInController, bug("scan %s\n", [path UTF8String]););
		for(NSString* plugInName in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil])
		{
			if([[[plugInName pathExtension] lowercaseString] isEqualToString:@"tmplugin"])
				[self loadPlugIn:[path stringByAppendingPathComponent:plugInName]];
		}
	}

	didLoadAllPlugIns = YES;
}
@end
