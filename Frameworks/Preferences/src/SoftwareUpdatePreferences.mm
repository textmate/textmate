#import "SoftwareUpdatePreferences.h"
#import "Keys.h"
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
@end

@implementation SoftwareUpdatePreferences
@synthesize isChecking, lastPoll, errorString;

+ (NSSet*)keyPathsForValuesAffectingLastCheck { return [NSSet setWithObjects:@"isChecking", @"lastPoll", @"errorString", nil]; }

- (id)init
{
	if(self = [super initWithNibName:@"SoftwareUpdatePreferences" label:@"Software Update" image:[NSImage imageNamed:@"Software Update" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakSoftwareUpdateChannelTransformer" andObjectsArray:@[ kSoftwareUpdateChannelRelease, kSoftwareUpdateChannelBeta ]];
		[self bind:@"isChecking"  toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"isChecking"  options:nil];
		[self bind:@"lastPoll"    toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"lastPoll"    options:nil];
		[self bind:@"errorString" toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"errorString" options:nil];

		self.defaultsProperties = [NSDictionary dictionaryWithObjectsAndKeys:
			kUserDefaultsDisableSoftwareUpdatesKey,  @"disableSoftwareUpdates",
			kUserDefaultsDisableBundleUpdatesKey,    @"disableBundleUpdates",
			kUserDefaultsDisableCrashReportingKey,   @"disableCrashReports",
			kUserDefaultsSoftwareUpdateChannelKey,   @"softwareUpdateChannel",
			kUserDefaultsAskBeforeUpdatingKey,       @"askBeforeDownloading",
			kUserDefaultsSubmitUsageInfoKey,         @"submitUsageInfo",
			kUserDefaultsCrashReportsContactInfoKey, @"contactInfo",
		nil];
	}
	return self;
}

- (IBAction)performSoftwareUpdateCheck:(id)sender
{
	[[SoftwareUpdate sharedInstance] checkForUpdates:self];
}

- (NSString*)lastCheck
{
	return isChecking ? @"Checking…" : (errorString ?: ([lastPoll humanReadableTimeElapsed] ?: @"Never"));
}

- (void)updateLastCheck:(id)sender
{
	[self willChangeValueForKey:@"lastCheck"];
	[self didChangeValueForKey:@"lastCheck"];
}

- (void)viewWillAppear
{
	[self updateLastCheck:nil];
	updateLastCheckTimer = [[NSTimer scheduledTimerWithTimeInterval:60 target:self selector:@selector(updateLastCheck:) userInfo:nil repeats:YES] retain];
}

- (void)viewDidDisappear
{
	[updateLastCheckTimer invalidate];
	[updateLastCheckTimer release];
	updateLastCheckTimer = nil;
}
@end
