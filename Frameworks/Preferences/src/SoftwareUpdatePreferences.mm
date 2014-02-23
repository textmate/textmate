#import "SoftwareUpdatePreferences.h"
#import "Keys.h"
#import <BundlesManager/BundlesManager.h>
#import <CrashReporter/CrashReporter.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <SoftwareUpdate/SoftwareUpdate.h>

// kUserDefaultsLastSoftwareUpdateCheckKey
// kUserDefaultsLastBundleUpdateCheckKey

@interface SoftwareUpdatePreferences ()
@property (nonatomic, assign) BOOL isChecking;
@property (nonatomic, retain) NSDate* lastPoll;
@property (nonatomic, retain) NSString* errorString;

@property (nonatomic, retain) NSString* lastPollString;
@property (nonatomic, retain) NSTimer* updateLastPollStringTimer;
@end

@implementation SoftwareUpdatePreferences
+ (NSSet*)keyPathsForValuesAffectingLastCheck { return [NSSet setWithObjects:@"isChecking", @"lastPollString", @"errorString", nil]; }

- (id)init
{
	if(self = [super initWithNibName:@"SoftwareUpdatePreferences" label:@"Software Update" image:[NSImage imageNamed:@"Software Update" inSameBundleAsClass:[self class]]])
	{
		[[CrashReporter sharedInstance] setupUserDefaultsContact:self];

		[OakStringListTransformer createTransformerWithName:@"OakSoftwareUpdateChannelTransformer" andObjectsArray:@[ kSoftwareUpdateChannelRelease, kSoftwareUpdateChannelBeta ]];
		[self bind:@"isChecking"  toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"isChecking"  options:nil];
		[self bind:@"lastPoll"    toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"lastPoll"    options:nil];
		[self bind:@"errorString" toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"errorString" options:nil];

		self.defaultsProperties = @{
			@"disableSoftwareUpdates" : kUserDefaultsDisableSoftwareUpdatesKey,
			@"disableBundleUpdates"   : kUserDefaultsDisableBundleUpdatesKey,
			@"disableCrashReports"    : kUserDefaultsDisableCrashReportingKey,
			@"softwareUpdateChannel"  : kUserDefaultsSoftwareUpdateChannelKey,
			@"askBeforeDownloading"   : kUserDefaultsAskBeforeUpdatingKey,
			@"submitUsageInfo"        : kUserDefaultsSubmitUsageInfoKey,
			@"contactInfo"            : kUserDefaultsCrashReportsContactInfoKey,
		};
	}
	return self;
}

- (IBAction)performSoftwareUpdateCheck:(id)sender
{
	[[SoftwareUpdate sharedInstance] checkForUpdates:self];
}

- (NSString*)lastCheck
{
	return _isChecking ? @"Checking…" : (_errorString ?: (_lastPollString ?: @"Never"));
}

- (void)updateLastPollString:(id)sender
{
	self.lastPollString = [self.lastPoll humanReadableTimeElapsed];
}

- (void)setLastPoll:(NSDate*)aDate
{
	_lastPoll = aDate;
	[self updateLastPollString:self];
}

- (void)viewWillAppear
{
	[self updateLastPollString:nil];
	self.updateLastPollStringTimer = [NSTimer scheduledTimerWithTimeInterval:60 target:self selector:@selector(updateLastPollString:) userInfo:nil repeats:YES];
}

- (void)viewDidDisappear
{
	[self.updateLastPollStringTimer invalidate];
	self.updateLastPollStringTimer = nil;
}
@end
