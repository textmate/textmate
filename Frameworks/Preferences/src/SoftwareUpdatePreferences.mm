#import "SoftwareUpdatePreferences.h"
#import "Keys.h"
#import <BundlesManager/BundlesManager.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <MenuBuilder/MenuBuilder.h>

// kUserDefaultsLastSoftwareUpdateCheckKey

@interface SoftwareUpdatePreferences ()
@property (nonatomic, readonly) NSString* lastCheck;
@property (nonatomic, getter = isChecking) BOOL checking;
@property (nonatomic) NSDate* lastPoll;
@property (nonatomic) NSString* errorString;

@property (nonatomic) NSString* lastPollString;
@property (nonatomic) NSTimer* updateLastPollStringTimer;
@end

@implementation SoftwareUpdatePreferences
+ (NSSet*)keyPathsForValuesAffectingLastCheck { return [NSSet setWithObjects:@"checking", @"lastPollString", @"errorString", nil]; }

- (id)init
{
	if(self = [super initWithNibName:nil label:@"Software Update" image:[NSImage imageNamed:@"Software Update" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakSoftwareUpdateChannelTransformer" andObjectsArray:@[ kSoftwareUpdateChannelRelease, kSoftwareUpdateChannelPrerelease ]];
		[self bind:@"checking"    toObject:SoftwareUpdate.sharedInstance withKeyPath:@"checking"    options:nil];
		[self bind:@"lastPoll"    toObject:SoftwareUpdate.sharedInstance withKeyPath:@"lastPoll"    options:nil];
		[self bind:@"errorString" toObject:SoftwareUpdate.sharedInstance withKeyPath:@"errorString" options:nil];

		self.defaultsProperties = @{
			@"disableSoftwareUpdates": kUserDefaultsDisableSoftwareUpdatesKey,
			@"disableCrashReports":    kUserDefaultsDisableCrashReportingKey,
			@"softwareUpdateChannel":  kUserDefaultsSoftwareUpdateChannelKey,
			@"askBeforeDownloading":   kUserDefaultsAskBeforeUpdatingKey,
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
#if defined(MAC_OS_X_VERSION_10_15) && (MAC_OS_X_VERSION_10_15 <= MAC_OS_X_VERSION_MAX_ALLOWED)
	if(@available(macos 10.15, *))
	{
		self.lastPollString = -[_lastPoll timeIntervalSinceNow] < 5 ? @"Just now" : [[[NSRelativeDateTimeFormatter alloc] init] localizedStringForDate:_lastPoll relativeToDate:NSDate.now];
	}
	else
#endif
	{
		NSTimeInterval const minute =  60;
		NSTimeInterval const hour   =  60*minute;
		NSTimeInterval const day    =  24*hour;
		NSTimeInterval const week   =   7*day;
		NSTimeInterval const month  =  31*day;
		NSTimeInterval const year   = 365*day;

		NSString* res;

		NSTimeInterval t = [[NSDate date] timeIntervalSinceDate:_lastPoll];
		if(t < 1)
			res = @"Just now";
		else if(t < minute)
			res = @"Less than a minute ago";
		else if(t < 2 * minute)
			res = @"1 minute ago";
		else if(t < hour)
			res = [NSString stringWithFormat:@"%.0f minutes ago", t / minute];
		else if(t < 2 * hour)
			res = @"1 hour ago";
		else if(t < day)
			res = [NSString stringWithFormat:@"%.0f hours ago", t / hour];
		else if(t < 2*day)
			res = @"Yesterday";
		else if(t < week)
			res = [NSString stringWithFormat:@"%.0f days ago", t / day];
		else if(t < 2*week)
			res = @"Last week";
		else if(t < month)
			res = [NSString stringWithFormat:@"%.0f weeks ago", t / week];
		else if(t < 2*month)
			res = @"Last month";
		else if(t < year)
			res = [NSString stringWithFormat:@"%.0f months ago", t / month];
		else if(t < 2*year)
			res = @"Last year";
		else
			res = [NSString stringWithFormat:@"%.0f years ago", t / year];

		self.lastPollString = res;
	}
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

- (void)loadView
{
	NSButton* watchForUpdatesCheckBox      = OakCreateCheckBox(@"Watch for:");
	NSPopUpButton* updateChannelPopUp      = OakCreatePopUpButton();
	NSButton* askBeforeDownloadingCheckBox = OakCreateCheckBox(@"Ask before downloading updates");

	NSStackView* watchForStackView = [NSStackView stackViewWithViews:@[ watchForUpdatesCheckBox, updateChannelPopUp ]];
	watchForStackView.alignment = NSLayoutAttributeFirstBaseline;

	NSTextField* lastCheckTextField        = OakCreateLabel(@"Some time ago");
	NSButton* checkNowButton               = [NSButton buttonWithTitle:@"Check Now" target:nil action:@selector(performSoftwareUpdateCheck:)];

	NSButton* submitCrashReportsCheckBox   = OakCreateCheckBox(@"Submit to MacroMates");

	NSTextField* contactTextField          = [NSTextField textFieldWithString:@"Anonymous"];

	NSFont* smallFont = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	contactTextField.font        = smallFont;
	contactTextField.controlSize = NSControlSizeSmall;

	NSStackView* contactStackView = [NSStackView stackViewWithViews:@[
		OakCreateLabel(@"Contact:", smallFont), contactTextField
	]];
	contactStackView.alignment  = NSLayoutAttributeFirstBaseline;
	contactStackView.edgeInsets = { .left = 18 };
	[contactStackView setHuggingPriority:NSLayoutPriorityDefaultHigh-1 forOrientation:NSLayoutConstraintOrientationVertical];

	MBMenu const updateChannelMenuItems = {
		{ @"Normal releases", .tag = 0 },
		{ @"Prereleases",     .tag = 1 },
	};
	MBCreateMenu(updateChannelMenuItems, updateChannelPopUp.menu);

	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		@[ OakCreateLabel(@"Software update:"),        watchForStackView                 ],
		@[ NSGridCell.emptyContentView,                askBeforeDownloadingCheckBox      ],
		@[ ],
		@[ OakCreateLabel(@"Last check:"),             lastCheckTextField                ],
		@[ NSGridCell.emptyContentView,                checkNowButton                    ],
		@[ ],
		@[ OakCreateLabel(@"Crash reports:"),          submitCrashReportsCheckBox        ],
		@[ NSGridCell.emptyContentView,                contactStackView                  ],
	]];

	[contactTextField.trailingAnchor constraintEqualToAnchor:updateChannelPopUp.trailingAnchor].active = YES;

	self.view = OakSetupGridViewWithSeparators(gridView, { 2, 5 });

	[watchForUpdatesCheckBox      bind:NSValueBinding       toObject:self withKeyPath:@"disableSoftwareUpdates" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[updateChannelPopUp           bind:NSSelectedTagBinding toObject:self withKeyPath:@"softwareUpdateChannel"  options:@{ NSValueTransformerNameBindingOption: @"OakSoftwareUpdateChannelTransformer" }];
	[askBeforeDownloadingCheckBox bind:NSValueBinding       toObject:self withKeyPath:@"askBeforeDownloading"   options:nil];
	[lastCheckTextField           bind:NSValueBinding       toObject:self withKeyPath:@"lastCheck"              options:nil];
	[submitCrashReportsCheckBox   bind:NSValueBinding       toObject:self withKeyPath:@"disableCrashReports"    options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[contactTextField             bind:NSValueBinding       toObject:self withKeyPath:@"contactInfo"            options:nil];

	[updateChannelPopUp           bind:NSEnabledBinding     toObject:self withKeyPath:@"disableSoftwareUpdates" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[askBeforeDownloadingCheckBox bind:NSEnabledBinding     toObject:self withKeyPath:@"disableSoftwareUpdates" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[checkNowButton               bind:NSEnabledBinding     toObject:self withKeyPath:@"checking"               options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[contactTextField             bind:NSEnabledBinding     toObject:self withKeyPath:@"disableCrashReports"    options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
}
@end
