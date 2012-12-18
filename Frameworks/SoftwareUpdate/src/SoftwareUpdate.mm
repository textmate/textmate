#import "SoftwareUpdate.h"
#import "DownloadWindowController.h"
#import "sw_update.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <network/network.h>
#import <ns/ns.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(SoftwareUpdate_Check);

NSString* const kUserDefaultsDisableSoftwareUpdatesKey     = @"SoftwareUpdateDisablePolling";
NSString* const kUserDefaultsSoftwareUpdateChannelKey      = @"SoftwareUpdateChannel"; // release (default), beta, nightly
NSString* const kUserDefaultsSubmitUsageInfoKey            = @"SoftwareUpdateSubmitUsageInfo";
NSString* const kUserDefaultsAskBeforeUpdatingKey          = @"SoftwareUpdateAskBeforeUpdating";
NSString* const kUserDefaultsLastSoftwareUpdateCheckKey    = @"SoftwareUpdateLastPoll";
NSString* const kUserDefaultsSoftwareUpdateSuspendUntilKey = @"SoftwareUpdateSuspendUntil";

NSString* const kSoftwareUpdateChannelRelease              = @"release";
NSString* const kSoftwareUpdateChannelBeta                 = @"beta";
NSString* const kSoftwareUpdateChannelNightly              = @"nightly";

@interface SoftwareUpdate ()
@property (nonatomic, retain) NSDate* lastPoll;
@property (nonatomic, assign) BOOL isChecking;
@property (nonatomic, retain) NSString* errorString;
@property (nonatomic, retain) NSTimer* pollTimer;
@property (nonatomic, retain) DownloadWindowController* downloadWindowController;
- (void)scheduleVersionCheck:(id)sender;
@end

static SoftwareUpdate* SharedInstance;

@implementation SoftwareUpdate
{
	key_chain_t keyChain;
	NSTimeInterval pollInterval;
}

+ (SoftwareUpdate*)sharedInstance
{
	return SharedInstance ?: [[self new] autorelease];
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:
		[NSDictionary dictionaryWithObjectsAndKeys:
			kSoftwareUpdateChannelRelease, kUserDefaultsSoftwareUpdateChannelKey,
			nil]];
}

- (id)init
{
	if(SharedInstance)
	{
		[self release];
	}
	else if(self = SharedInstance = [[super init] retain])
	{
		D(DBF_SoftwareUpdate_Check, bug("\n"););
		pollInterval = 60*60;

		[[[NSWorkspace sharedWorkspace] notificationCenter] addObserver:self selector:@selector(scheduleVersionCheck:) name:NSWorkspaceDidWakeNotification object:[NSWorkspace sharedWorkspace]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return SharedInstance;
}

- (void)scheduleVersionCheck:(id)sender
{
	D(DBF_SoftwareUpdate_Check, bug("had pending check: %s\n", BSTR(self.pollTimer)););
	[self.pollTimer invalidate];
	self.pollTimer = nil;

	struct statfs sfsb;
	BOOL readOnlyFileSystem = statfs(oak::application_t::path().c_str(), &sfsb) != 0 || (sfsb.f_flags & MNT_RDONLY);
	BOOL disablePolling = [[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsDisableSoftwareUpdatesKey] boolValue];
	D(DBF_SoftwareUpdate_Check, bug("download visible %s, disable polling %s, read only file system %s → %s\n", BSTR(downloadWindowController.isVisible), BSTR(disablePolling), BSTR(readOnlyFileSystem), BSTR(!downloadWindowController.isVisible && !disablePolling && !readOnlyFileSystem)););
	if(self.downloadWindowController.isVisible || disablePolling || readOnlyFileSystem)
		return;

	NSDate* nextCheck = [(self.lastPoll ?: [NSDate distantPast]) addTimeInterval:pollInterval];
	if(NSDate* suspendUntil = [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsSoftwareUpdateSuspendUntilKey])
		nextCheck = [nextCheck laterDate:suspendUntil];

	D(DBF_SoftwareUpdate_Check, bug("perform next check in %.1f hours\n", std::max<NSTimeInterval>(1, [nextCheck timeIntervalSinceNow])/60/60););
	self.pollTimer = [NSTimer scheduledTimerWithTimeInterval:std::max<NSTimeInterval>(1, [nextCheck timeIntervalSinceNow]) target:self selector:@selector(performVersionCheck:) userInfo:nil repeats:NO];
}

- (void)userDefaultsDidChange:(id)sender
{
	[self scheduleVersionCheck:self];
}

// ========================
// = Performing the check =
// ========================

- (void)performVersionCheck:(NSTimer*)aTimer
{
	D(DBF_SoftwareUpdate_Check, bug("last check was %.1f hours ago\n", -[self.lastPoll timeIntervalSinceNow] / (60*60)););
	if(!self.isChecking)
	{
		self.isChecking = YES;

		NSURL* url = [self.channels objectForKey:[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsSoftwareUpdateChannelKey]];
		[self performSelectorInBackground:@selector(performBackgroundVersionCheck:) withObject:@{ @"infoURL" : url, @"timer" : YES_obj }];
	}
}

- (IBAction)checkForUpdates:(id)sender
{
	D(DBF_SoftwareUpdate_Check, bug("\n"););
	if(!self.isChecking)
	{
		self.isChecking = YES;

		NSURL* url = [self.channels objectForKey:OakIsAlternateKeyOrMouseEvent(NSAlternateKeyMask) ? kSoftwareUpdateChannelNightly : [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsSoftwareUpdateChannelKey]];
		BOOL force = OakIsAlternateKeyOrMouseEvent(NSShiftKeyMask);
		[self performSelectorInBackground:@selector(performBackgroundVersionCheck:) withObject:@{ @"infoURL" : url, @"force" : @(force) }];
	}
}

- (void)performBackgroundVersionCheck:(NSDictionary*)someOptions
{
	@autoreleasepool {
		NSURL* url = [someOptions objectForKey:@"infoURL"];
		std::string error = NULL_STR;
		auto info = sw_update::download_info(to_s([url absoluteString]), &error);

		NSMutableDictionary* res = [[someOptions mutableCopy] autorelease];
		if(error != NULL_STR)
		{
			[res setObject:[NSString stringWithCxxString:error] forKey:@"error"];
		}
		else
		{
			[res setObject:[NSString stringWithCxxString:info.url] forKey:@"url"];
			[res setObject:@(info.version) forKey:@"version"];
		}

		[self performSelectorOnMainThread:@selector(didPerformBackgroundVersionCheck:) withObject:res waitUntilDone:NO];
	}
}

- (void)didPerformBackgroundVersionCheck:(NSDictionary*)info
{
	long version          = [[info objectForKey:@"version"] longValue];
	NSString* downloadURL = [info objectForKey:@"url"];
	NSString* error       = [info objectForKey:@"error"];
	BOOL timer            = [[info objectForKey:@"timer"] boolValue];
	BOOL force            = [[info objectForKey:@"force"] boolValue];
	BOOL ask              = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAskBeforeUpdatingKey];

	if(version && downloadURL)
	{
		NSString* appName     = [NSString stringWithCxxString:oak::application_t::name()];
		NSInteger thisVersion = force ? 0 : strtol(oak::application_t::revision().c_str(), NULL, 10);
		if(version <= thisVersion)
		{
			if(!timer)
				NSRunInformationalAlertPanel(@"Up To Date", @"%@ %ld is the latest version available—you have %ld.", @"Continue", nil, nil, appName, version, thisVersion);
		}
		else
		{
			bool interactive = ask || !timer;
			int choice = interactive ? NSRunInformationalAlertPanel(@"New Version Available", @"%@ %ld is now available—you have %ld. Would you like to download it now?", @"Download & Install", nil, @"Later", appName, version, thisVersion) : NSAlertDefaultReturn;
			if(choice == NSAlertDefaultReturn) // "Download & Install"
			{
				self.downloadWindowController = [[DownloadWindowController alloc] initWithURL:downloadURL displayString:[NSString stringWithFormat:@"Downloading %@ %ld…", appName, version] keyChain:keyChain];
				self.downloadWindowController.versionOfDownload = [NSString stringWithFormat:@"%ld", version];
				if(!interactive && [NSApp isActive])
						[[self.downloadWindowController window] orderFront:self];
				else	[self.downloadWindowController showWindow:self];
			}
			else if(choice == NSAlertOtherReturn) // "Later"
			{
				[[NSUserDefaults standardUserDefaults] setObject:[[NSDate date] addTimeInterval:24*60*60] forKey:kUserDefaultsSoftwareUpdateSuspendUntilKey];
			}
		}
	}
	else if(error && !timer)
	{
		NSRunInformationalAlertPanel(@"Error checking for new version", @"%@", @"Continue", nil, nil, error);
		error = nil;
	}

	self.isChecking  = NO;
	self.lastPoll    = [NSDate date];
	self.errorString = error;
}

// ==============
// = Properties =
// ==============

- (void)setChannels:(NSDictionary*)someChannels
{
	[_channels autorelease];
	_channels = [someChannels retain];
	[self scheduleVersionCheck:nil];
}

- (void)setSignee:(key_chain_t::key_t const&)aSignee
{
	keyChain.add(aSignee);
}

- (NSDate*)lastPoll                  { return [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsLastSoftwareUpdateCheckKey]; }
- (void)setLastPoll:(NSDate*)newDate { [[NSUserDefaults standardUserDefaults] setObject:newDate forKey:kUserDefaultsLastSoftwareUpdateCheckKey]; }
@end
