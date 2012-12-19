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

@interface DownloadWindowController ()
- (void)notifyUserAboutUpdate;
@property (nonatomic, assign) BOOL showUpdateBadge;
@end

@implementation DownloadWindowController
- (id)init
{
	if(self = [super initWithWindowNibName:@"DownloadProgress"])
	{
	}
	return self;
}

- (void)dealloc
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););

	self.activityText = nil;
	self.statusText   = nil;

	[super dealloc];
}

- (void)windowDidLoad
{
	self.window.hidesOnDeactivate = YES;
	self.window.level             = NSStatusWindowLevel;
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

// ================
// = UI Callbacks =
// ================

- (IBAction)install:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(install:)])
		[self.delegate install:self];
}

- (IBAction)cancel:(id)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(cancel:)])
		[self.delegate cancel:self];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	if([self.delegate respondsToSelector:@selector(windowWillClose:)])
		[self.delegate windowWillClose:self];
}
@end

// ======================
// = DownloadController =
// ======================

struct shared_state_t
{
	double progress = 0;
	bool stop = false;
};

typedef std::shared_ptr<shared_state_t> shared_state_ptr;

@interface DownloadController ()
@property (nonatomic, retain) DownloadWindowController* view;

@property (nonatomic, retain) NSDate* downloadStartDate;
@property (nonatomic, retain) NSTimer* progressTimer;

@property (retain) NSString* url;
@property (retain) NSString* archive;
@end

@implementation DownloadController
{
	shared_state_ptr sharedState;
	CGFloat secondsLeft;
	key_chain_t keyChain;
}

- (id)initWithURL:(NSString*)aURL displayString:(NSString*)aDisplayString keyChain:(key_chain_t const&)aKeyChain
{
	D(DBF_SoftwareUpdate_Download, bug("%s\n", [aURL UTF8String]););
	if(self = [super init])
	{
		sharedState.reset(new shared_state_t);
		secondsLeft = CGFLOAT_MAX;

		self.url = aURL;
		keyChain = aKeyChain;

		self.view = [[[DownloadWindowController alloc] init] autorelease];
		self.view.delegate     = self;
		self.view.activityText = aDisplayString;
		self.view.statusText   = @"";
	}
	return self;
}

- (void)dealloc
{
	ASSERT(!self.progressTimer);

	self.downloadStartDate = nil;
	self.url               = nil;
	self.archive           = nil;
	self.view              = nil;

	[super dealloc];
}

- (BOOL)isVisible
{
	return [self.view isWindowLoaded] && [self.view.window isVisible];
}

- (void)startDownloadBackgroundUI:(BOOL)backgroundUIFlag
{
	self.view.isIndeterminate = NO;
	self.view.progress        = 0;
	self.view.isWorking       = YES;

	self.view.canInstall      = NO;
	self.view.canCancel       = YES;

	if(backgroundUIFlag)
			[self.view.window orderFront:self];
	else	[self.view showWindow:self];

	self.downloadStartDate = [NSDate date];
	self.progressTimer     = [NSTimer scheduledTimerWithTimeInterval:0.04 target:self selector:@selector(updateProgress:) userInfo:nil repeats:YES];

	[self performSelectorInBackground:@selector(performBackgroundDownload:) withObject:self];
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
	self.view.progress = sharedState->progress;

	NSTimeInterval secondsElapsed = -[self.downloadStartDate timeIntervalSinceNow];
	if(secondsElapsed < 1.0 || self.view.progress < 0.01)
		return;

	NSTimeInterval left = secondsElapsed / self.view.progress - secondsElapsed;
	if(left < 2.6)
	{
		self.view.statusText = @"Time remaining: a few seconds";
	}
	else
	{
		NSTimeInterval roundedSecondsLeft = 5 * round(left / 5);
		if(roundedSecondsLeft < secondsLeft || roundedSecondsLeft - secondsLeft > 10)
		{
			NSTimeInterval const kMinute = 60;
			NSTimeInterval const kHour   = 60*60;

			if(roundedSecondsLeft < kMinute)
				self.view.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f seconds", roundedSecondsLeft];
			else if(roundedSecondsLeft < 2*kMinute)
				self.view.statusText = [NSString stringWithFormat:@"Time remaining: a few minutes"];
			else if(roundedSecondsLeft < kHour)
				self.view.statusText = [NSString stringWithFormat:@"Time remaining: about %.0f minutes", roundedSecondsLeft / kMinute];
			else
				self.view.statusText = [NSString stringWithFormat:@"Time remaining: hours"];
			secondsLeft = roundedSecondsLeft;
		}
	}
}

- (void)didPerformBackgroundDownload:(NSDictionary*)info
{
	self.view.progress   = 1;
	self.view.statusText = @"";
	self.view.isWorking  = NO;

	[self.progressTimer invalidate];
	self.progressTimer = nil;

	if(sharedState->stop)
		return;

	if(NSString* path = [info objectForKey:@"path"])
	{
		self.archive = path;

		self.view.activityText = @"Download Completed";
		self.view.canInstall   = YES;

		if([NSApp isActive])
			OakPlayUISound(OakSoundDidCompleteSomethingUISound);

		[self.view notifyUserAboutUpdate];
	}
	else
	{
		NSString* error = [info objectForKey:@"error"];
		self.view.activityText = [NSString stringWithFormat:@"Failed: %@", error];
	}
}

// ====================
// = Delegate Methods =
// ====================

- (void)install:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););

	self.view.activityText    = [NSString stringWithFormat:@"Installing %@…", self.versionOfDownload ?: @"app"];
	self.view.isIndeterminate = YES;
	self.view.isWorking       = YES;
	self.view.canInstall      = NO;

	std::string err = sw_update::install_update(to_s(self.archive));
	if(err == NULL_STR)
	{
		// FIXME Copy/paste from <Preferences/Keys.h>
		static NSString* const kUserDefaultsDisableSessionRestoreKey = @"disableSessionRestore";

		BOOL isSessionRestoreDisabled = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsDisableSessionRestoreKey];
		BOOL skipUserInteraction      = isSessionRestoreDisabled == NO;

		self.view.activityText = @"Relaunching…";
		oak::application_t::relaunch(skipUserInteraction);
	}
	else
	{
		self.view.activityText = [NSString stringWithCxxString:err];
		self.view.isWorking    = NO;
		OakRunIOAlertPanel("%s", err.c_str());
	}
}

- (void)cancel:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	[sender.window performClose:self];
}

- (void)windowWillClose:(DownloadWindowController*)sender
{
	D(DBF_SoftwareUpdate_Download, bug("\n"););
	[self.progressTimer invalidate];
	self.progressTimer = nil;
	self.view.showUpdateBadge = NO;

	sharedState->stop = true;
	[self release];
}
@end
