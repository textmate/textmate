#import "TMPlugInController.h"
#import <OakSystem/application.h>
#import <io/path.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(PlugInController);

static TMPlugInController* SharedInstance;
static NSInteger const kPlugInAPIVersion = 2;

@interface TMPlugInController ()
@property (nonatomic) NSMutableDictionary* loadedPlugIns;
@end

static id CreateInstanceOfPlugInClass (Class cl, TMPlugInController* controller)
{
	if(id instance = [cl alloc])
	{
		if([instance respondsToSelector:@selector(initWithPlugInController:)])
				return [instance initWithPlugInController:controller];
		else	return [instance init];
	}
	return nil;
}

@implementation TMPlugInController
+ (TMPlugInController*)sharedInstance
{
	return SharedInstance ?: [TMPlugInController new];
}

- (id)init
{
	if(SharedInstance)
	{
	}
	else if(self = SharedInstance = [super init])
	{
		D(DBF_PlugInController, bug("\n"););
		self.loadedPlugIns = [NSMutableDictionary dictionary];
	}
	return SharedInstance;
}

- (CGFloat)version
{
	return 2.0;
}

- (void)loadPlugInAtPath:(NSString*)aPath
{
	if(NSBundle* bundle = [NSBundle bundleWithPath:aPath])
	{
		NSString* identifier = [bundle objectForInfoDictionaryKey:@"CFBundleIdentifier"];
		NSString* name = [bundle objectForInfoDictionaryKey:@"CFBundleName"];

		if(![self.loadedPlugIns objectForKey:identifier])
		{
			if([[bundle objectForInfoDictionaryKey:@"TMPlugInAPIVersion"] intValue] == kPlugInAPIVersion)
			{
				std::string const crashedDuringPlugInLoad = path::join(path::temp(), "load_" + to_s(identifier));

				if(path::exists(crashedDuringPlugInLoad))
				{
					NSInteger choice = NSRunAlertPanel([NSString stringWithFormat:@"Skip Loading %@ Plug-In?", name ?: identifier], @"Previous attempt of loading the plug-in caused abnormal exit. Would you like to skip it?", @"Skip", @"Load Anyway", nil);
					if(choice == NSAlertDefaultReturn) // "Skip"
						return;
				}

				path::set_content(crashedDuringPlugInLoad, "");

				if([bundle load])
				{
					if(id instance = CreateInstanceOfPlugInClass([bundle principalClass], self))
					{
						self.loadedPlugIns[identifier] = instance;
					}
					else
					{
						NSLog(@"Failed to instantiate plug-in class: %@, path %@", [bundle principalClass], aPath);
					}
				}
				else
				{
					NSLog(@"Failed to load plug-in: %@, path %@", name ?: identifier, aPath);
				}

				unlink(crashedDuringPlugInLoad.c_str());
			}
			else
			{
				NSLog(@"Skip incompatible plug-in: %@, path %@", name ?: identifier, aPath);
			}
		}
		else
		{
			NSLog(@"Skip plug-in at path: %@ (already loaded %@)", identifier, [self.loadedPlugIns[identifier] bundlePath]);
		}
	}
	else
	{
		NSLog(@"Failed to create NSBundle for path: %@", aPath);
	}
}

- (void)loadAllPlugIns:(id)sender
{
	NSMutableArray* paths = [NSMutableArray array];
	for(NSString* path in NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSAllDomainsMask, YES))
		[paths addObject:[NSString pathWithComponents:@[ path, @"TextMate", @"PlugIns" ]]];
	[paths addObject:[[NSBundle mainBundle] builtInPlugInsPath]];

	for(NSString* path in paths)
	{
		D(DBF_PlugInController, bug("scan %s\n", [path UTF8String]););
		for(NSString* plugInName in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:path error:nil])
		{
			if([[[plugInName pathExtension] lowercaseString] isEqualToString:@"tmplugin"])
				[self loadPlugInAtPath:[path stringByAppendingPathComponent:plugInName]];
		}
	}
}

- (void)installPlugInAtPath:(NSString*)src
{
	NSFileManager* fm = [NSFileManager defaultManager];

	NSArray* libraryPaths = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSAllDomainsMask, YES);
	NSString* dst = [NSString pathWithComponents:@[ libraryPaths[0], @"TextMate", @"PlugIns", [src lastPathComponent] ]];
	if([src isEqualToString:dst])
		return;

	NSBundle* plugInBundle = [NSBundle bundleWithPath:src];
	NSString* plugInName   = [plugInBundle objectForInfoDictionaryKey:@"CFBundleName"] ?: [[src lastPathComponent] stringByDeletingPathExtension];

	if([[plugInBundle objectForInfoDictionaryKey:@"TMPlugInAPIVersion"] intValue] != kPlugInAPIVersion)
	{
		NSRunAlertPanel(@"Cannot Install Plug-in", @"The %@ plug-in is not compatible with this version of TextMate.", @"Continue", nil, nil, plugInName);
		return;
	}

	if([fm fileExistsAtPath:dst])
	{
		NSString* newVersion = [[NSBundle bundleWithPath:src] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: [[NSBundle bundleWithPath:src] objectForInfoDictionaryKey:@"CFBundleVersion"];
		NSString* oldVersion = [[NSBundle bundleWithPath:dst] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] ?: [[NSBundle bundleWithPath:dst] objectForInfoDictionaryKey:@"CFBundleVersion"];
		NSInteger choice = NSRunAlertPanel(@"Plug-in Already Installed", @"Version %@ of “%@” is already installed.\nDo you want to replace it with version %@?\n\nUpgrading a plug-in will require TextMate to be relaunched.", @"Replace", @"Cancel", nil, oldVersion ?: @"???", plugInName, newVersion ?: @"???");
		if(choice == NSAlertDefaultReturn) // "Replace"
		{
			if(![fm removeItemAtPath:dst error:NULL])
			{
				NSRunAlertPanel(@"Install Failed", @"Couldn't remove old plug-in (“%@”)", @"Continue", nil, nil, [dst stringByAbbreviatingWithTildeInPath]);
				dst = nil;
			}
		}
		else if(choice == NSAlertAlternateReturn) // "Cancel"
		{
			dst = nil;
		}
	}

	if(!dst)
		return;

	NSString* dstDir = [dst stringByDeletingLastPathComponent];
	if([fm createDirectoryAtPath:dstDir withIntermediateDirectories:YES attributes:nil error:NULL])
	{
		if([fm copyItemAtPath:src toPath:dst error:NULL])
		{
			NSInteger choice = NSRunAlertPanel(@"Plug-in Installed", @"To activate “%@” you will need to relaunch TextMate.", @"Relaunch", @"Cancel", nil, plugInName);
			if(choice == NSAlertDefaultReturn) // "Relaunch"
				oak::application_t::relaunch();
		}
		else
		{
			NSRunAlertPanel(@"Install Failed", @"The plug-in has not been installed.", @"Continue", nil, nil);
		}
	}
	else
	{
		NSRunAlertPanel(@"Install Failed", @"It was not possible to create the plug-in folder (“%@”)", @"Continue", nil, nil, [dstDir stringByAbbreviatingWithTildeInPath]);
	}
}
@end
