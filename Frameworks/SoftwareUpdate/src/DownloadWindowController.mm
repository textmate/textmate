#import "DownloadWindowController.h"
#import "sw_update.h"
#import <ns/ns.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakSound.h>
#import <OakAppKit/OakImage.h>
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>

OAK_DEBUG_VAR(SoftwareUpdate_Download);

struct shared_state_t
{
	double progress = 0;
	bool stop = false;
};

typedef std::shared_ptr<shared_state_t> shared_state_ptr;

@interface DownloadWindowController ()
- (void)notifyUserAboutUpdate;

@property (nonatomic, retain) NSDate* downloadStartDate;
@property (nonatomic, retain) NSTimer* progressTimer;
@property (nonatomic, assign) BOOL isDownloading;
@property (nonatomic, assign) BOOL showUpdateBadge;
@property (retain) NSString* url;
@property (retain) NSString* archive;
@end

@implementation DownloadWindowController
{
	shared_state_ptr sharedState;
	CGFloat secondsLeft;
	key_chain_t keyChain;
}

- (id)initWithURL:(NSString*)aURL displayString:(NSString*)aDisplayString keyChain:(key_chain_t const&)aKeyChain
{
	D(DBF_SoftwareUpdate_Download, bug("%s\n", [aURL UTF8String]););
	if(self = [super initWithWindowNibName:@"DownloadProgress"])
	{
		sharedState.reset(new shared_state_t);
		self.downloadStartDate = [NSDate date];
		secondsLeft            = CGFLOAT_MAX;

		self.activityText  = aDisplayString;
		self.progress      = 0;
		self.statusText    = @"";
		self.isDownloading = YES;
		self.progressTimer = [NSTimer scheduledTimerWithTimeInterval:0.04 target:self selector:@selector(updateProgress:) userInfo:nil repeats:YES];

		self.url = aURL;
		keyChain = aKeyChain;
		[self performSelectorInBackground:@selector(performBackgroundDownload:) withObject:self];
	}
	return self;
}

- (void)performBackgroundDownload:(id)sender
{
	@autoreleasepool {
		shared_state_ptr state = sharedState;
		std::string error = NULL_STR;
		std::string path = sw_update::download_update(to_s(self.url), keyChain, &error, &state->progress, &state->stop);

		NSDictionary* arg = [NSDictionary dictionary];
		if(path != NULL_STR)
			arg = @{ @"path" : [NSString stringWithCxxString:path] };
		else if(error != NULL_STR)
			arg = @{ @"error" : [NSString stringWithCxxString:error] };
		[self performSelectorOnMainThread:@selector(didPerformBackgroundDownload:) withObject:arg waitUntilDone:NO];
	}
}

- (void)updateProgress:(NSTimer*)aTimer
{
	self.progress = sharedState->progress;

	NSTimeInterval secondsElapsed = -[self.downloadStartDate timeIntervalSinceNow];
	if(secondsElapsed < 1.0 || self.progress < 0.01)
		return;

	NSTimeInterval left = secondsElapsed / self.progress - secondsElapsed;
	if(left < 2.6)
	{
		self.statusText = @"Time remaining: a few seconds";
	}
	else
	{
		NSTimeInterval roundedSecondsLeft = 5 * round(left / 5);
		if(roundedSecondsLeft < secondsLeft || roundedSecondsLeft - secondsLeft > 10)
		{
			NSTimeInterval const kMinute = 60;
			NSTimeInterval const kHour   = 60*60;

			if(roundedSecondsLeft < kMinute)
				self.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f seconds", roundedSecondsLeft];
			else if(roundedSecondsLeft < 2*kMinute)
				self.statusText = [NSString stringWithFormat:@"Time remaining: a few minutes"];
			else if(roundedSecondsLeft < kHour)
				self.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f minutes", roundedSecondsLeft / kMinute];
			else
				self.statusText = [NSString stringWithFormat:@"Time remaining: hours"];
			secondsLeft = roundedSecondsLeft;
		}
	}
}

- (void)didPerformBackgroundDownload:(NSDictionary*)info
{
	[self.progressTimer invalidate];
	self.progressTimer = nil;
	self.progress      = 1;
	self.statusText    = @"";
	self.isDownloading = NO;

	if(sharedState->stop)
		return;

	if(NSString* path = [info objectForKey:@"path"])
	{
		self.archive      = path;
		self.activityText = @"Download Completed";
		self.canInstall   = YES;

		if([NSApp isActive])
			OakPlayUISound(OakSoundDidCompleteSomethingUISound);

		[self notifyUserAboutUpdate];
	}
	else
	{
		NSString* error = [info objectForKey:@"error"];
		self.activityText = [NSString stringWithFormat:@"Failed: %@", error];
	}
}

- (void)dealloc
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	ASSERT(!progressTimer);

	self.url     = nil;
	self.archive = nil;

	self.activityText      = nil;
	self.statusText        = nil;
	self.downloadStartDate = nil;

	[super dealloc];
}

+ (NSSet*)keyPathsForValuesAffectingIsWorking { return [NSSet setWithObjects:@"isDownloading", @"isInstalling", nil]; }

- (BOOL)isWorking
{
	return self.isDownloading || self.isInstalling;
}

- (void)windowDidLoad
{
	self.window.hidesOnDeactivate = YES;
	self.window.level             = NSStatusWindowLevel;
}

- (BOOL)isVisible
{
	return [self isWindowLoaded] && [[self window] isVisible];
}

- (void)setShowUpdateBadge:(BOOL)flag
{
	D(DBF_SoftwareUpdate_Download, bug("%s\n", BSTR(flag)););
	if(_showUpdateBadge == flag)
		return;

	if(_showUpdateBadge = flag)
	{
		D(DBF_SoftwareUpdate_Download, bug("alter application icon\n"););
		NSImage* appIcon = [NSApp applicationIconImage];
		NSImage* dlBadge = [[[NSImage imageNamed:@"Update Badge" inSameBundleAsClass:[self class]] copy] autorelease];
		[dlBadge setSize:NSMakeSize(appIcon.size.width / 4, appIcon.size.height / 4)];
		[NSApp setApplicationIconImage:[OakImage imageWithBase:appIcon badge:dlBadge edge:CGRectMaxXEdge]];
	}
	else
	{
		[NSApp setApplicationIconImage:nil];
	}
}

- (void)notifyUserAboutUpdate
{
	self.showUpdateBadge = YES;
	[NSApp requestUserAttention:NSInformationalRequest];
}

- (IBAction)install:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););

	self.activityText = [NSString stringWithFormat:@"Installing %@…", self.versionOfDownload ?: @"app"];
	self.isInstalling = YES;

	std::string err = sw_update::install_update(to_s(self.archive));
	if(err == NULL_STR)
	{
		// FIXME Copy/paste from <Preferences/Keys.h>
		static NSString* const kUserDefaultsDisableSessionRestoreKey = @"disableSessionRestore";

		BOOL isSessionRestoreDisabled = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
		BOOL skipUserInteraction      = isSessionRestoreDisabled == NO;

		self.activityText = @"Relaunching…";
		oak::application_t::relaunch(skipUserInteraction);
	}
	else
	{
		self.activityText = [NSString stringWithCxxString:err];
		OakRunIOAlertPanel("%s", err.c_str());
	}
}

- (IBAction)cancel:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	[[self window] performClose:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	[self.progressTimer invalidate];
	self.progressTimer = nil;
	self.isDownloading = NO;
	self.showUpdateBadge = NO;

	sharedState->stop = true;
	[self release];
}
@end
