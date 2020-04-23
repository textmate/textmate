#import "SoftwareUpdatePreferences.h"
#import "Keys.h"
#import <BundlesManager/BundlesManager.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSDate Additions.h>
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

	MBMenu const updateChannelMenuItems = {
		{ @"Normal Releases", .tag = 0 },
		{ @"Nightly Builds",  .tag = 1 },
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
