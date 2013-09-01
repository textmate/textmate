#import "SoftwareUpdate.h"
#import "DownloadWindowController.h"
#import "sw_update.h"
#import "version_compare.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakSound.h>
#import <OakAppKit/NSMenu Additions.h>
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

struct shared_state_t
{
	double progress = 0;
	bool stop = false;
};

typedef std::shared_ptr<shared_state_t> shared_state_ptr;

@interface SoftwareUpdate ()
@property (nonatomic) NSDate* lastPoll;
@property (nonatomic) BOOL isChecking;
@property (nonatomic) NSString* lastVersionDownloaded;
@property (nonatomic) NSString* errorString;
@property (nonatomic) NSTimer* pollTimer;
@property (nonatomic) DownloadWindowController* downloadWindow;
@property (nonatomic) NSString* archive;

- (void)scheduleVersionCheck:(id)sender;
- (void)checkVersionAtURL:(NSURL*)anURL inBackground:(BOOL)backgroundFlag allowRedownload:(BOOL)redownloadFlag;
@end

static SoftwareUpdate* SharedInstance;

@implementation SoftwareUpdate
{
	key_chain_t keyChain;
	NSTimeInterval pollInterval;

	shared_state_ptr sharedState;
	CGFloat secondsLeft;
}

+ (SoftwareUpdate*)sharedInstance
{
	return SharedInstance ?: [self new];
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsSoftwareUpdateChannelKey : kSoftwareUpdateChannelRelease
	}];
}

- (id)init
{
	if(SharedInstance)
	{
	}
	else if(self = SharedInstance = [super init])
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
	D(DBF_SoftwareUpdate_Check, bug("download visible %s, disable polling %s, read only file system %s → %s\n", BSTR(self.downloadWindow), BSTR(disablePolling), BSTR(readOnlyFileSystem), BSTR(!self.downloadWindow && !disablePolling && !readOnlyFileSystem)););
	if(_downloadWindow.isWorking || disablePolling || readOnlyFileSystem)
		return;

	NSDate* nextCheck = [(self.lastPoll ?: [NSDate distantPast]) dateByAddingTimeInterval:pollInterval];
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
	if(_downloadWindow.isWorking)
		return;

	NSURL* url = [self.channels objectForKey:[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsSoftwareUpdateChannelKey]];
	[self checkVersionAtURL:url inBackground:YES allowRedownload:NO];
}

- (IBAction)checkForUpdates:(id)sender
{
	D(DBF_SoftwareUpdate_Check, bug("\n"););

	BOOL isShiftDown = OakIsAlternateKeyOrMouseEvent(NSShiftKeyMask);
	NSURL* url = [self.channels objectForKey:OakIsAlternateKeyOrMouseEvent(NSAlternateKeyMask) ? kSoftwareUpdateChannelNightly : [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsSoftwareUpdateChannelKey]];
	[self checkVersionAtURL:url inBackground:NO allowRedownload:isShiftDown];
}

- (void)checkVersionAtURL:(NSURL*)anURL inBackground:(BOOL)backgroundFlag allowRedownload:(BOOL)redownloadFlag
{
	if(self.isChecking)
		return;
	self.isChecking = YES;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		std::string error = NULL_STR;
		auto info = sw_update::download_info(to_s([anURL absoluteString]), &error);

		dispatch_async(dispatch_get_main_queue(), ^{
			self.errorString = [NSString stringWithCxxString:error];
			self.lastPoll    = [NSDate date];
			self.isChecking  = NO;

			if(self.errorString)
			{
				if(!backgroundFlag)
					NSRunInformationalAlertPanel(@"Error checking for new version", @"%@", @"Continue", nil, nil, self.errorString);
			}
			else if(info.version != NULL_STR && info.url != NULL_STR)
			{
				NSString* version    = [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"];
				NSString* newVersion = [NSString stringWithFormat:@"%@ %@", [[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleName"], [NSString stringWithCxxString:info.version]];

				BOOL downloadAndInstall = NO;

				if(version_equal(info.version, to_s(version)) && !backgroundFlag)
				{
					NSInteger choice = NSRunInformationalAlertPanel(@"Up To Date", @"%@ is the latest version available—you have version %@.", @"Continue", nil, redownloadFlag ? @"Redownload" : nil, newVersion, version);
					if(choice == NSAlertOtherReturn) // “Redownload”
						downloadAndInstall = YES;
				}
				else if(version_less(info.version, to_s(version)) && !backgroundFlag)
				{
					NSInteger choice = NSRunInformationalAlertPanel(@"Up To Date", @"%@ is the latest version available—you have version %@.", @"Continue", nil, @"Downgrade", newVersion, version);
					if(choice == NSAlertOtherReturn) // “Downgrade”
						downloadAndInstall = YES;
				}
				else if(version_less(to_s(version), info.version))
				{
					if(!backgroundFlag || [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAskBeforeUpdatingKey])
					{
						NSInteger choice = NSRunInformationalAlertPanel(@"New Version Available", @"%@ is now available—you have version %@. Would you like to download it now?", @"Download & Install", nil, @"Later", newVersion, version);
						if(choice == NSAlertDefaultReturn) // “Download & Install”
							downloadAndInstall = YES;
						else if(choice == NSAlertOtherReturn) // “Later”
							[[NSUserDefaults standardUserDefaults] setObject:[[NSDate date] dateByAddingTimeInterval:24*60*60] forKey:kUserDefaultsSoftwareUpdateSuspendUntilKey];
					}
					else if(version_less(to_s(self.lastVersionDownloaded), info.version))
					{
						downloadAndInstall = YES;
					}
				}

				if(downloadAndInstall)
				{
					BOOL interactive = !backgroundFlag || [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsAskBeforeUpdatingKey];
					self.lastVersionDownloaded = [NSString stringWithCxxString:info.version];
					[self downloadVersion:newVersion atURL:[NSString stringWithCxxString:info.url] interactively:interactive];
				}
			}
		});
	});
}

// ===================
// = Download Update =
// ===================

- (DownloadWindowController*)downloadWindow
{
	return _downloadWindow = _downloadWindow ?: [DownloadWindowController new];
}

- (void)downloadVersion:(NSString*)versionName atURL:(NSString*)downloadURL interactively:(BOOL)interactive
{
	sharedState = std::make_shared<shared_state_t>();
	secondsLeft = CGFLOAT_MAX;

	self.downloadWindow.delegate     = self;
	self.downloadWindow.activityText = [NSString stringWithFormat:@"Downloading %@…", versionName];
	self.downloadWindow.statusText   = @"Estimating time remaining";

	self.downloadWindow.isIndeterminate = NO;
	self.downloadWindow.progress        = 0;
	self.downloadWindow.isWorking       = YES;

	self.downloadWindow.canInstall      = NO;
	self.downloadWindow.canCancel       = YES;

	if(!interactive && [NSApp isActive])
			[self.downloadWindow.window orderFront:self];
	else	[self.downloadWindow showWindow:self];

	NSTimer* updateProgressTimer = [NSTimer scheduledTimerWithTimeInterval:0.04 target:self selector:@selector(updateProgress:) userInfo:[NSDate date] repeats:YES];

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_LOW, 0), ^{
		shared_state_ptr state = sharedState;
		std::string error = NULL_STR;
		std::string path = sw_update::download_update(to_s(downloadURL), keyChain, &error, &state->progress, &state->stop);

		dispatch_async(dispatch_get_main_queue(), ^{
			[updateProgressTimer invalidate];

			self.downloadWindow.progress   = 1;
			self.downloadWindow.statusText = @"";
			self.downloadWindow.isWorking  = NO;

			if(sharedState->stop)
				return;

			if(path != NULL_STR)
			{
				self.archive = [NSString stringWithCxxString:path];

				self.downloadWindow.activityText    = [NSString stringWithFormat:@"Downloaded %@", versionName];
				self.downloadWindow.canInstall      = YES;
				self.downloadWindow.showUpdateBadge = YES;

				if([NSApp isActive])
					OakPlayUISound(OakSoundDidCompleteSomethingUISound);

				[NSApp requestUserAttention:NSInformationalRequest];
			}
			else if(error != NULL_STR)
			{
				self.downloadWindow.activityText = [NSString stringWithFormat:@"Failed: %@", [NSString stringWithCxxString:error]];
			}
		});
	});
}

- (void)updateProgress:(NSTimer*)aTimer
{
	self.downloadWindow.progress = sharedState->progress;

	NSDate* downloadStartDate = [aTimer userInfo];
	NSTimeInterval secondsElapsed = -[downloadStartDate timeIntervalSinceNow];
	if(secondsElapsed < 1.0 || self.downloadWindow.progress < 0.01)
		return;

	NSTimeInterval left = secondsElapsed / self.downloadWindow.progress - secondsElapsed;
	if(left < 2.6)
	{
		self.downloadWindow.statusText = @"Time remaining: a few seconds";
	}
	else
	{
		NSTimeInterval roundedSecondsLeft = 5 * round(left / 5);
		if(roundedSecondsLeft < secondsLeft || roundedSecondsLeft - secondsLeft > 10)
		{
			NSTimeInterval const kMinute = 60;
			NSTimeInterval const kHour   = 60*60;

			if(roundedSecondsLeft < kMinute)
				self.downloadWindow.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f seconds", roundedSecondsLeft];
			else if(roundedSecondsLeft < 2*kMinute)
				self.downloadWindow.statusText = [NSString stringWithFormat:@"Time remaining: a few minutes"];
			else if(roundedSecondsLeft < kHour)
				self.downloadWindow.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f minutes", roundedSecondsLeft / kMinute];
			else
				self.downloadWindow.statusText = [NSString stringWithFormat:@"Time remaining: hours"];
			secondsLeft = roundedSecondsLeft;
		}
	}
}

// ====================
// = Delegate Methods =
// ====================

- (void)install:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Check, bug("\n"););

	self.downloadWindow.activityText    = @"Installing…";
	self.downloadWindow.isIndeterminate = YES;
	self.downloadWindow.isWorking       = YES;
	self.downloadWindow.canInstall      = NO;

	std::string err = sw_update::install_update(to_s(self.archive));
	if(err == NULL_STR)
	{
		self.downloadWindow.activityText = @"Relaunching…";
		oak::application_t::relaunch();
	}
	else
	{
		self.downloadWindow.activityText = [NSString stringWithCxxString:err];
		self.downloadWindow.isWorking    = NO;
		OakRunIOAlertPanel("%s", err.c_str());
	}
}

- (void)cancel:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Check, bug("\n"););
	path::remove(to_s(self.archive));
	[sender.window performClose:self];
}

- (void)windowWillClose:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Check, bug("\n"););
	sharedState->stop = true;

	self.downloadWindow.showUpdateBadge = NO;
	self.downloadWindow = nil;
}

// ==============
// = Properties =
// ==============

- (void)setChannels:(NSDictionary*)someChannels
{
	_channels = someChannels;
	[self scheduleVersionCheck:nil];
}

- (void)setSignee:(key_chain_t::key_t const&)aSignee
{
	keyChain.add(aSignee);
}

- (NSDate*)lastPoll                  { return [[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsLastSoftwareUpdateCheckKey]; }
- (void)setLastPoll:(NSDate*)newDate { [[NSUserDefaults standardUserDefaults] setObject:newDate forKey:kUserDefaultsLastSoftwareUpdateCheckKey]; }
@end
