#import "SoftwareUpdatePreferences.h"
#import "Keys.h"
#import <BundlesManager/BundlesManager.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/NSDate Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <SoftwareUpdate/SoftwareUpdate.h>

// kUserDefaultsLastSoftwareUpdateCheckKey

@interface SoftwareUpdatePreferences ()
@property (nonatomic, readwrite, getter = isChecking) BOOL checking;
@property (nonatomic) NSDate* lastPoll;
@property (nonatomic) NSString* errorString;

@property (nonatomic) NSString* lastPollString;
@property (nonatomic) NSTimer* updateLastPollStringTimer;
@end

@implementation SoftwareUpdatePreferences
+ (NSSet*)keyPathsForValuesAffectingLastCheck { return [NSSet setWithObjects:@"checking", @"lastPollString", @"errorString", nil]; }

- (id)init
{
	if(self = [super initWithNibName:@"SoftwareUpdatePreferences" label:@"Software Update" image:[NSImage imageNamed:@"Software Update" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakSoftwareUpdateChannelTransformer" andObjectsArray:@[ kSoftwareUpdateChannelRelease, kSoftwareUpdateChannelPrerelease ]];
		[self bind:@"checking"    toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"checking"    options:nil];
		[self bind:@"lastPoll"    toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"lastPoll"    options:nil];
		[self bind:@"errorString" toObject:[SoftwareUpdate sharedInstance] withKeyPath:@"errorString" options:nil];

		self.defaultsProperties = @{
			@"disableSoftwareUpdates": kUserDefaultsDisableSoftwareUpdatesKey,
			@"disableCrashReports":    kUserDefaultsDisableCrashReportingKey,
			@"softwareUpdateChannel":  kUserDefaultsSoftwareUpdateChannelKey,
			@"askBeforeDownloading":   kUserDefaultsAskBeforeUpdatingKey,
			@"submitUsageInfo":        kUserDefaultsSubmitUsageInfoKey,
			@"contactInfo":            kUserDefaultsCrashReportsContactInfoKey,
		};
	}
	return self;
}

- (NSString*)lastCheck
{
	return _checking ? @"Checking…" : (_errorString ?: (_lastPollString ?: @"Never"));
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
