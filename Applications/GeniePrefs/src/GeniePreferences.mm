#import "GeniePreferences.h"
#import "GenieTableViewController.h"
#import "ViewControllers.h"
#import "AddAutoLayoutViews.h"
#import "DryRunController.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <GenieManager/GenieUserDefaults.h>
#import <MenuBuilder/MenuBuilder.h>
#import <SoftwareUpdate/SoftwareUpdate.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <ServiceManagement/ServiceManagement.h>
#import <oak/debug.h>

static NSSet* const kExistingDefaultsKeys = [NSSet setWithArray:@[
	kDisableLaunchAtLoginSettingsKey,
	kActivationKeyEventSettingsKey,
	kEnableClipboardHistorySettingsKey,
	kClipboardHistoryIgnoreAppsSettingsKey,
	kClipboardHistoryExpireAfterSettingsKey,
	kDisableSoftwareUpdateSettingsKey,
	kSoftwareUpdatePrereleasesEnabled,
	kUserDefaultsSoftwareUpdateChannelKey,
]];

@interface GenieUserDefaultsProxy : NSObject
{
	NSUserDefaults* _genieUserDefaults;
	NSMutableDictionary* _cachedUserDefaults;
}
@property (nonatomic) BOOL softwareUpdatePrereleasesEnabled;
@end

@implementation GenieUserDefaultsProxy
- (instancetype)init
{
	if(self = [super init])
	{
		_genieUserDefaults  = GenieManager.userDefaults;
		_cachedUserDefaults = [NSMutableDictionary dictionary];
		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:kGenieBundleIdentifier];
	}
	return self;
}

- (id)valueForKey:(NSString*)aKey
{
	if(![kExistingDefaultsKeys containsObject:aKey])
		return [super valueForKey:aKey];

	id res = _cachedUserDefaults[aKey];
	if(!res)
	{
		if(res = [_genieUserDefaults objectForKey:aKey])
			_cachedUserDefaults[aKey] = res;
	}
	return res;
}

- (void)setValue:(id)someValue forKey:(NSString*)aKey
{
	if(![kExistingDefaultsKeys containsObject:aKey])
		return [super setValue:someValue forKey:aKey];

	id oldValue = _cachedUserDefaults[aKey];
	if([someValue isEqual:oldValue])
		return;

	[self willChangeValueForKey:aKey];
	_cachedUserDefaults[aKey] = someValue;
	[self didChangeValueForKey:aKey];

	[_genieUserDefaults setObject:someValue forKey:aKey];
	[_genieUserDefaults synchronize];
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:kGeniePrefsBundleIdentifier userInfo:nil deliverImmediately:YES];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	for(NSString* key in kExistingDefaultsKeys)
		[self setValue:[_genieUserDefaults objectForKey:key] forKey:key];
}
@end

@interface GeneralSettingsViewController () <NSMenuDelegate>
@property (nonatomic) GenieUserDefaultsProxy* userDefaultsProxy;
@property (nonatomic) NSButton* clearClipboardHistoryButton;
@property (nonatomic, getter = isSoftwareUpdatePrereleasesEnabled) BOOL softwareUpdatePrereleasesEnabled;
@property (nonatomic) NSArrayController* timeIntervalArrayController;
@property (nonatomic) NSPopUpButton* ignoredApplicationsPopUpButton;
@end

@implementation GeneralSettingsViewController
+ (NSSet*)keyPathsForValuesAffectingSoftwareUpdatePrereleasesEnabled { return [NSSet setWithArray:@[ [@"userDefaultsProxy." stringByAppendingString:kDisableSoftwareUpdateSettingsKey] ]]; }

- (BOOL)isSoftwareUpdatePrereleasesEnabled
{
	NSString* channel = [_userDefaultsProxy valueForKey:kUserDefaultsSoftwareUpdateChannelKey];
	return ![[_userDefaultsProxy valueForKey:kDisableSoftwareUpdateSettingsKey] boolValue] && [channel isEqual:kSoftwareUpdateChannelPrerelease];
}

- (void)setSoftwareUpdatePrereleasesEnabled:(BOOL)flag
{
	[_userDefaultsProxy setValue:(flag ? kSoftwareUpdateChannelPrerelease : nil) forKey:kUserDefaultsSoftwareUpdateChannelKey];
}

- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"General";
		_userDefaultsProxy = [[GenieUserDefaultsProxy alloc] init];

		// Upgrading from 0.12-beta
		if([[_userDefaultsProxy valueForKey:kSoftwareUpdatePrereleasesEnabled] boolValue])
		{
			[_userDefaultsProxy setValue:kSoftwareUpdateChannelPrerelease forKey:kUserDefaultsSoftwareUpdateChannelKey];
			[_userDefaultsProxy setValue:nil forKey:kSoftwareUpdatePrereleasesEnabled];
		}
	}
	return self;
}

- (NSBox*)horizontalSeparator
{
	NSBox* separator = [[NSBox alloc] initWithFrame:NSMakeRect(0, 0, 200, 1)];
	separator.boxType = NSBoxSeparator;
	[separator setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	return separator;
}

- (NSTextField*)textFieldWithInformation:(NSString*)someText
{
	NSTextField* textField = [NSTextField wrappingLabelWithString:someText];
	textField.textColor = NSColor.secondaryLabelColor;
	textField.font = [NSFont systemFontOfSize:NSFont.smallSystemFontSize];
	return textField;
}

- (void)loadView
{
	NSArray* timeIntervals = @[
		@{ @"title": @"24 hours", @"duration": @"24 hours" },
		@{ @"title": @"1 week",   @"duration": @"7 days"   },
		@{ @"title": @"1 month",  @"duration": @"1 month"  },
		@{ @"title": @"1 year",   @"duration": @"1 year"   },
	];

	NSString* expireAfter = [_userDefaultsProxy valueForKey:kClipboardHistoryExpireAfterSettingsKey];
	if([timeIntervals filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"duration == %@", expireAfter]].count == 0)
		timeIntervals = [timeIntervals arrayByAddingObject:@{ @"title": expireAfter, @"duration": expireAfter }];

	_timeIntervalArrayController = [[NSArrayController alloc] init];
	_timeIntervalArrayController.content = timeIntervals;

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	NSButton* launchAtLoginEnabledCheckbox           = [NSButton checkboxWithTitle:@"Launch Genie when logging in" target:nil action:nil];
	OakKeyEquivalentView* showGenieKeyEquivalentView = [[OakKeyEquivalentView alloc] initWithFrame:NSZeroRect];
	NSTextField* shortcutExplanation                 = [self textFieldWithInformation:@"If you want to use ⌘Space to bring up Genie then you may need to disable “Show Spotlight search” under Shortcuts in Keyboard Preferences."];
	NSButton* clipboardHistoryEnabledCheckbox        = [NSButton checkboxWithTitle:@"Enable clipboard history" target:nil action:nil];
	NSTextField* clipboardExplanation                = [self textFieldWithInformation:@"This option requires that you allow Genie to control your computer in Security & Privacy settings."];
	NSPopUpButton* expireClipboardPopUpButton        = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	NSButton* softwareUpdateEnabledCheckbox          = [NSButton checkboxWithTitle:@"Automatically check for updates" target:nil action:nil];
	NSButton* softwareUpdatePrereleasesCheckbox      = [NSButton checkboxWithTitle:@"Include pre-releases" target:nil action:nil];

	_clearClipboardHistoryButton = [NSButton buttonWithTitle:@"Clear History" target:nil action:@selector(clearClipboardHistory:)];

	_ignoredApplicationsPopUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:YES];
	[_ignoredApplicationsPopUpButton.menu addItem:[self ignoredAppsSummaryMenuItem]];
	_ignoredApplicationsPopUpButton.menu.delegate = self;

	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		@[ [NSTextField labelWithString:@"Startup:"],                  launchAtLoginEnabledCheckbox,            [NSButton buttonWithTitle:@"Quit Genie" target:nil action:@selector(terminateGenie:)] ],

		@[ [self horizontalSeparator] ],

		@[ [NSTextField labelWithString:@"Genie shortcut:"],           showGenieKeyEquivalentView ],
		@[ NSGridCell.emptyContentView,                                shortcutExplanation ],

		@[ [self horizontalSeparator] ],

		@[ [NSTextField labelWithString:@"Clipboard history:"],        clipboardHistoryEnabledCheckbox,         _clearClipboardHistoryButton ],
		@[ NSGridCell.emptyContentView,                                clipboardExplanation ],
		@[ [NSTextField labelWithString:@"Ignore text copied from:"],  _ignoredApplicationsPopUpButton ],
		@[ [NSTextField labelWithString:@"Keep history for:"],         expireClipboardPopUpButton ],

		@[ [self horizontalSeparator] ],

		@[ [NSTextField labelWithString:@"Software update:"],          softwareUpdateEnabledCheckbox,           [NSButton buttonWithTitle:@"Check Now" target:nil action:@selector(checkForSoftwareUpdate:)] ],
		@[ NSGridCell.emptyContentView,                                softwareUpdatePrereleasesCheckbox ],
	]];

	gridView.rowAlignment = NSGridRowAlignmentLastBaseline;
	[gridView columnAtIndex:0].xPlacement = NSGridCellPlacementTrailing;
	[gridView columnAtIndex:2].leadingPadding = 20;
	[gridView cellForView:showGenieKeyEquivalentView].customPlacementConstraints = [NSLayoutConstraint constraintsWithVisualFormat:@"H:[shortcut(<=140,==140@250)]" options:0 metrics:nil views:@{ @"shortcut": showGenieKeyEquivalentView }];
	[gridView cellForView:clipboardExplanation].row.bottomPadding = 4;

	for(NSUInteger rowIndex : { 1, 4, 9 })
	{
		[gridView rowAtIndex:rowIndex].height = 12;
		[gridView rowAtIndex:rowIndex].yPlacement = NSGridCellPlacementCenter;
		[gridView mergeCellsInHorizontalRange:NSMakeRange(0, 3) verticalRange:NSMakeRange(rowIndex, 1)];
	}

	for(NSView* view in @[ shortcutExplanation, clipboardExplanation])
	{
		NSGridCell* gridCell = [gridView cellForView:view];
		gridCell.customPlacementConstraints = [NSLayoutConstraint constraintsWithVisualFormat:@"H:[text(<=400)]" options:0 metrics:nil views:@{ @"text": view }];
		[gridView mergeCellsInHorizontalRange:NSMakeRange(1, 2) verticalRange:NSMakeRange([gridView indexOfRow:gridCell.row], 1)];
	}

	NSDictionary* views = @{
		@"gridView": gridView,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, gridView ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[gridView]-(>=20)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[gridView]-(>=20)-|"        options:0 metrics:nil views:views]];
	[contentView addConstraint:[NSLayoutConstraint constraintWithItem:gridView attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeCenterX multiplier:1 constant:0]];

	[launchAtLoginEnabledCheckbox             bind:NSValueBinding           toObject:_userDefaultsProxy withKeyPath:kDisableLaunchAtLoginSettingsKey                   options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[showGenieKeyEquivalentView               bind:NSValueBinding           toObject:_userDefaultsProxy withKeyPath:kActivationKeyEventSettingsKey                     options:nil];

	[clipboardHistoryEnabledCheckbox          bind:NSValueBinding           toObject:_userDefaultsProxy           withKeyPath:kEnableClipboardHistorySettingsKey       options:nil];
	[expireClipboardPopUpButton               bind:NSContentBinding         toObject:_timeIntervalArrayController withKeyPath:@"arrangedObjects"                       options:nil];
	[expireClipboardPopUpButton               bind:NSContentObjectsBinding  toObject:_timeIntervalArrayController withKeyPath:@"arrangedObjects.duration"              options:nil];
	[expireClipboardPopUpButton               bind:NSContentValuesBinding   toObject:_timeIntervalArrayController withKeyPath:@"arrangedObjects.title"                 options:nil];
	[expireClipboardPopUpButton               bind:NSSelectedObjectBinding  toObject:_userDefaultsProxy           withKeyPath:kClipboardHistoryExpireAfterSettingsKey  options:nil];
	[expireClipboardPopUpButton               bind:NSEnabledBinding         toObject:_userDefaultsProxy           withKeyPath:kEnableClipboardHistorySettingsKey       options:nil];
	[_ignoredApplicationsPopUpButton          bind:NSEnabledBinding         toObject:_userDefaultsProxy           withKeyPath:kEnableClipboardHistorySettingsKey       options:nil];

	[softwareUpdateEnabledCheckbox            bind:NSValueBinding           toObject:_userDefaultsProxy           withKeyPath:kDisableSoftwareUpdateSettingsKey        options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[softwareUpdatePrereleasesCheckbox        bind:NSEnabledBinding         toObject:_userDefaultsProxy           withKeyPath:kDisableSoftwareUpdateSettingsKey        options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[softwareUpdatePrereleasesCheckbox        bind:NSValueBinding           toObject:self                         withKeyPath:@"softwareUpdatePrereleasesEnabled"      options:nil];

	self.view = contentView;
}

- (void)viewWillAppear
{
	_clearClipboardHistoryButton.enabled = [NSFileManager.defaultManager fileExistsAtPath:[GenieClipboardHistoryPath stringByExpandingTildeInPath]];
	[super viewWillAppear];
}

- (void)viewDidDisappear
{
	[super viewDidDisappear];
	[self updateIgnoredAppsUsingBlock:^(NSArray<NSDictionary*>* ignoredApps){
		return [ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES"]];
	}];
}

- (void)terminateGenie:(id)sender
{
	BOOL didTerminate = YES;
	for(NSRunningApplication* app in [NSRunningApplication runningApplicationsWithBundleIdentifier:kGenieBundleIdentifier])
		didTerminate = [app terminate];

	if(didTerminate)
		[NSApp performSelector:@selector(terminate:) withObject:self afterDelay:0];
}

- (void)checkForSoftwareUpdate:(id)sender
{
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:@"com.macromates.perform-software-update-check" object:kGeniePrefsBundleIdentifier userInfo:nil deliverImmediately:YES];
}

// =====================
// = Clipboard History =
// =====================

- (void)clearClipboardHistory:(id)sender
{
	NSString* path = [GenieClipboardHistoryPath stringByExpandingTildeInPath];

	NSError* error;
	if(![NSFileManager.defaultManager removeItemAtPath:path error:&error])
	{
		if(!([error.domain isEqualToString:NSPOSIXErrorDomain] && error.code == ENOENT))
			[self.view.window presentError:error modalForWindow:self.view.window delegate:nil didPresentSelector:NULL contextInfo:nullptr];
	}
	_clearClipboardHistoryButton.enabled = [NSFileManager.defaultManager fileExistsAtPath:path];
}

- (NSMenuItem*)ignoredAppsSummaryMenuItem
{
	NSArray* ignoredApps = [_userDefaultsProxy valueForKey:kClipboardHistoryIgnoreAppsSettingsKey];
	ignoredApps = [ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"disabled != YES"]];

	if(ignoredApps.count == 0)
		return [[NSMenuItem alloc] initWithTitle:@"Select Application" action:nil keyEquivalent:@""];
	else if(ignoredApps.count > 1)
		return [[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"%ld applications", ignoredApps.count] action:nil keyEquivalent:@""];

	NSString* bundleIdentifier = ignoredApps.firstObject[@"bundleIdentifier"];
	NSString* title = ignoredApps.firstObject[@"localizedName"];
	NSImage* icon;

	if(NSRunningApplication* app = [NSRunningApplication runningApplicationsWithBundleIdentifier:bundleIdentifier].firstObject)
	{
		title = app.localizedName;
		icon  = [app.icon copy];
	}
	else if(NSString* bundlePath = [NSWorkspace.sharedWorkspace absolutePathForAppBundleWithIdentifier:bundleIdentifier])
	{
		title = [NSFileManager.defaultManager displayNameAtPath:bundlePath];
		icon  = [[NSWorkspace.sharedWorkspace iconForFile:bundlePath] copy];
	}
	icon.size = NSMakeSize(16, 16);

	NSMenuItem* menuItem = [[NSMenuItem alloc] initWithTitle:title action:nil keyEquivalent:@""];
	menuItem.image = icon;
	return menuItem;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	NSMutableSet* ignoredAppIdentifiers = [NSMutableSet set];

	NSMutableArray<NSMenuItem*>* applicationMenuItems = [NSMutableArray array];
	for(NSDictionary* appInfo in [_userDefaultsProxy valueForKey:kClipboardHistoryIgnoreAppsSettingsKey])
	{
		NSString* bundleIdentifier = appInfo[@"bundleIdentifier"];
		if(!bundleIdentifier || [ignoredAppIdentifiers containsObject:bundleIdentifier])
			continue;
		[ignoredAppIdentifiers addObject:bundleIdentifier];

		BOOL disabled   = [appInfo[@"disabled"] boolValue];
		NSString* title = appInfo[@"localizedName"];
		NSImage* icon;

		if(NSRunningApplication* app = [NSRunningApplication runningApplicationsWithBundleIdentifier:bundleIdentifier].firstObject)
		{
			title = app.localizedName;
			icon  = [app.icon copy];
		}
		else if(NSString* bundlePath = [NSWorkspace.sharedWorkspace absolutePathForAppBundleWithIdentifier:bundleIdentifier])
		{
			title = [NSFileManager.defaultManager displayNameAtPath:bundlePath];
			icon  = [[NSWorkspace.sharedWorkspace iconForFile:bundlePath] copy];
		}
		else if(disabled)
		{
			continue;
		}
		icon.size = NSMakeSize(16, 16);

		NSMenuItem* appItem = [[NSMenuItem alloc] initWithTitle:title action:disabled ? @selector(ignoreApplication:) : @selector(unignoreApplication:) keyEquivalent:@""];
		appItem.representedObject = @{ @"bundleIdentifier": bundleIdentifier, @"localizedName": title };
		appItem.state             = disabled ? NSOffState : NSOnState;
		appItem.image             = icon;
		[applicationMenuItems addObject:appItem];
	}

	for(NSRunningApplication* runningApp in [[NSWorkspace.sharedWorkspace runningApplications] filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"activationPolicy == %@ && bundleIdentifier != %@", @(NSApplicationActivationPolicyRegular), [NSBundle.mainBundle bundleIdentifier]]])
	{
		if([ignoredAppIdentifiers containsObject:runningApp.bundleIdentifier])
			continue;

		NSImage* appIcon = [runningApp.icon copy];
		appIcon.size = NSMakeSize(16, 16);

		NSMenuItem* appItem = [[NSMenuItem alloc] initWithTitle:runningApp.localizedName action:@selector(ignoreApplication:) keyEquivalent:@""];
		appItem.representedObject = @{ @"bundleIdentifier": runningApp.bundleIdentifier, @"localizedName": runningApp.localizedName };
		appItem.image = appIcon;
		[applicationMenuItems addObject:appItem];
	}

	[aMenu removeAllItems];
	[aMenu addItem:[self ignoredAppsSummaryMenuItem]];

	if(applicationMenuItems.count)
	{
		for(NSMenuItem* menuItem in [applicationMenuItems sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"title" ascending:YES selector:@selector(caseInsensitiveCompare:)]]])
			[aMenu addItem:menuItem];
		[aMenu addItem:[NSMenuItem separatorItem]];
	}

	[aMenu addItemWithTitle:@"Other…" action:@selector(ignoreOtherApplication:) keyEquivalent:@""];
}

- (void)updateIgnoredAppsUsingBlock:(NSArray<NSDictionary*>*(^)(NSArray<NSDictionary*>*))transformer
{
	NSArray* ignoredApps = [_userDefaultsProxy valueForKey:kClipboardHistoryIgnoreAppsSettingsKey] ?: @[ ];
	[_userDefaultsProxy setValue:transformer(ignoredApps) forKey:kClipboardHistoryIgnoreAppsSettingsKey];

	[_ignoredApplicationsPopUpButton.menu removeAllItems];
	[_ignoredApplicationsPopUpButton.menu addItem:[self ignoredAppsSummaryMenuItem]];
	[_ignoredApplicationsPopUpButton synchronizeTitleAndSelectedItem];
}

- (void)ignoreApplication:(id)sender
{
	if(NSDictionary* appInfo = [sender representedObject])
	{
		[self updateIgnoredAppsUsingBlock:^(NSArray<NSDictionary*>* ignoredApps){
			return [[ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"bundleIdentifier != %@", appInfo[@"bundleIdentifier"]]] arrayByAddingObject:appInfo];
		}];
	}
}

- (void)unignoreApplication:(id)sender
{
	NSDictionary* appInfo = [sender representedObject];
	if(NSString* bundleIdentifier = appInfo[@"bundleIdentifier"])
	{
		[self updateIgnoredAppsUsingBlock:^(NSArray<NSDictionary*>* ignoredApps){
			return [[ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"bundleIdentifier != %@", bundleIdentifier]] arrayByAddingObject:@{ @"bundleIdentifier": bundleIdentifier, @"localizedName": appInfo[@"localizedName"], @"disabled": @YES }];
		}];
	}
}

- (void)ignoreOtherApplication:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.allowedFileTypes = @[ @"com.apple.application-bundle" ];
	openPanel.directoryURL     = [NSURL fileURLWithPath:NSSearchPathForDirectoriesInDomains(NSApplicationDirectory, NSLocalDomainMask, YES).firstObject];
	openPanel.prompt           = @"Select";

	[openPanel beginSheetModalForWindow:self.view.window completionHandler:^(NSInteger result){
		if(result == NSFileHandlingPanelOKButton)
		{
			if(NSBundle* bundle = [NSBundle bundleWithURL:openPanel.URL])
			{
				NSString* bundleIdentifier = bundle.bundleIdentifier;
				NSString* title = [NSFileManager.defaultManager displayNameAtPath:bundle.bundlePath];

				[self updateIgnoredAppsUsingBlock:^(NSArray<NSDictionary*>* ignoredApps){
					return [[ignoredApps filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"bundleIdentifier != %@", bundleIdentifier]] arrayByAddingObject:@{ @"bundleIdentifier": bundleIdentifier, @"localizedName": title }];
				}];
			}
		}
	}];
}
@end

@interface VariablesViewController ()
{
	GenieTableViewController* _variablesTableViewController;
}
@property (nonatomic) NSResponder* initialFirstResponder;
@end

@implementation VariablesViewController
- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"Variables";
	}
	return self;
}

- (void)loadView
{
	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	_variablesTableViewController = [[GenieTableViewController alloc] initWithColumnNames:@[ @"disabled", @"name", @"value" ] visibleRows:10 showHeaderView:YES prototype:@{ @"name": @"variable", @"value": @"value" }];
	[_variablesTableViewController.arrayController bind:NSContentArrayBinding toObject:GenieManager.sharedInstance withKeyPath:@"variables" options:nil];

	NSDictionary* views = @{
		@"table": _variablesTableViewController.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, _variablesTableViewController.view ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[table(<=400,==400@250)]-(>=20)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[table]-(>=20)-|"                         options:0 metrics:nil views:views]];
	[contentView addConstraint:[NSLayoutConstraint constraintWithItem:_variablesTableViewController.view attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeCenterX multiplier:1 constant:0]];

	NSArray<NSTableColumn*>* tableColumns = _variablesTableViewController.tableView.tableColumns;
	tableColumns[0].title = @"";
	tableColumns[1].title = @"Variable Name";
	tableColumns[2].title = @"Value";

	self.initialFirstResponder = _variablesTableViewController.tableView;
	self.view = contentView;
}
@end

@interface WebViewController () <WKNavigationDelegate>
@property (nonatomic) NSResponder* initialFirstResponder;
@property (nonatomic) WKWebView* webView;
@end

@implementation WebViewController
- (void)webView:(WKWebView*)webView decidePolicyForNavigationAction:(WKNavigationAction*)navigationAction decisionHandler:(void(^)(WKNavigationActionPolicy))handler
{
	NSURLRequest* request = navigationAction.request;
	if(![[request.URL scheme] isEqualToString:@"file"] && [NSWorkspace.sharedWorkspace openURL:request.URL] || ![NSURLConnection canHandleRequest:request])
			handler(WKNavigationActionPolicyCancel);
	else	handler(WKNavigationActionPolicyAllow);
}

- (void)loadView
{
	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	WKWebViewConfiguration* webConfig = [[WKWebViewConfiguration alloc] init];
	webConfig.suppressesIncrementalRendering = YES;

	_webView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:webConfig];
	_webView.navigationDelegate = self;
	_webView.autoresizingMask   = NSViewWidthSizable|NSViewHeightSizable;
	[contentView addSubview:_webView];

	self.initialFirstResponder = _webView;
	self.view = contentView;
}
@end

@implementation DocumentationViewController
- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"Docs";
	}
	return self;
}

- (void)loadView
{
	[super loadView];
	if(NSURL* url = [[NSBundle mainBundle] URLForResource:@"Docs" withExtension:@"html"])
		[self.webView loadFileURL:url allowingReadAccessToURL:[NSURL fileURLWithPath:[url.path stringByDeletingLastPathComponent]]];
}
@end

@implementation ChangesViewController
- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"Changes";
	}
	return self;
}

- (void)loadView
{
	[super loadView];
	if(NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"])
		[self.webView loadFileURL:url allowingReadAccessToURL:[NSURL fileURLWithPath:[url.path stringByDeletingLastPathComponent]]];
}
@end

@interface CatalogViewController () <NSOutlineViewDataSource>
{
	NSTreeController* _treeController;

	NSTextView* _textView;
	NSScrollView* _textViewScrollView;

	// ====================
	// = View Controllers =
	// ====================

	NSDictionary<NSString*, NSViewController*>* _viewControllers;
	NSSplitView* _splitView;
	NSView* _containerView;
	NSButton* _advancedButton;

	NSString* _dragType;
	NSArray* _draggedNodes;
}
@property (nonatomic) GenieItemKind selectedItemKind;
@property (nonatomic) NSOutlineView* outlineView;
@property (nonatomic) NSButton* addButton;
@property (nonatomic) NSButton* removeButton;
@property (nonatomic) NSUInteger countOfAdvancedKeys;
@property (nonatomic) NSResponder* initialFirstResponder;
@end

static NSIndexPath* IndexPathForGenieItemWithIdentifier (NSString* identifier, NSArray<GenieItem*>* items, NSIndexPath* parent = nil)
{
	for(NSUInteger i = 0; i < items.count; ++i)
	{
		if([items[i].identifier isEqualToString:identifier])
			return parent ? [parent indexPathByAddingIndex:i] : [NSIndexPath indexPathWithIndex:i];
	}

	for(NSUInteger i = 0; i < items.count; ++i)
	{
		if(NSArray* children = items[i].children)
		{
			if(NSIndexPath* res = IndexPathForGenieItemWithIdentifier(identifier, children, parent ? [parent indexPathByAddingIndex:i] : [NSIndexPath indexPathWithIndex:i]))
				return res;
		}
	}

	return nil;
}

@implementation CatalogViewController
- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"Catalog";

		_treeController = [[NSTreeController alloc] init];
		_treeController.childrenKeyPath = @"children";
		_treeController.objectClass     = [GenieItem class];
		_treeController.content         = GenieManager.sharedInstance.items;

		_viewControllers = @{
			[NSString stringWithFormat:@"%lu", kGenieItemKindGroup]:               [[BasicProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindWebAddress]:          [[URLProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindRunScript]:           [[ShellProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindOpenFile]:            [[FileProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindSpotlight]:           [[SpotlightProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindSQLite]:              [[SQLiteProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindCommandResult]:       [[ExecDataSourceProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindRecentDocuments]:     [[RecentDocumentsProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindPredicateGroup]:      [[PredicateGroupProperties alloc] initWithTreeController:_treeController],
		};

		[self bind:@"selectedItemKind" toObject:_treeController withKeyPath:@"selection.kind" options:nil];

		if(NSString* selectedItemIdentifier = [[NSUserDefaults standardUserDefaults] stringForKey:@"selectedItemIdentifier"])
		{
			if(NSIndexPath* indexPath = IndexPathForGenieItemWithIdentifier(selectedItemIdentifier, GenieManager.sharedInstance.items))
				_treeController.selectionIndexPath = indexPath;
		}
	}
	return self;
}

- (void)setCountOfAdvancedKeys:(NSUInteger)newCountOfAdvancedKeys
{
	_countOfAdvancedKeys = newCountOfAdvancedKeys;
	_advancedButton.title = newCountOfAdvancedKeys ? [NSString stringWithFormat:@"Advanced (%lu)…", _countOfAdvancedKeys] : @"Advanced…";
}

- (void)loadView
{
	_dragType = [NSUUID UUID].UUIDString;

	// ======================
	// = Left of Split View =
	// ======================

	NSOutlineView* outlineView = [[NSOutlineView alloc] initWithFrame:NSZeroRect];
	[outlineView registerForDraggedTypes:@[ _dragType ]];
	_outlineView = outlineView;

	NSTableColumn* spaceColumn = [[NSTableColumn alloc] initWithIdentifier:@"space"];
	spaceColumn.width    = 0;
	spaceColumn.editable = NO;
	[outlineView addTableColumn:spaceColumn];

	NSTableColumn* enabledColumn = [[NSTableColumn alloc] initWithIdentifier:@"enabled"];
	NSButtonCell* checkboxCell = [[NSButtonCell alloc] init];
	checkboxCell.buttonType = NSSwitchButton;
	checkboxCell.title      = @"";
	enabledColumn.dataCell = checkboxCell;
	enabledColumn.width    = 16;
	[outlineView addTableColumn:enabledColumn];

	NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:@"title"];
	[outlineView addTableColumn:tableColumn];
	outlineView.intercellSpacing = NSMakeSize(3, 12);
	outlineView.outlineTableColumn = tableColumn;
	outlineView.headerView = nil;
	outlineView.usesAlternatingRowBackgroundColors = YES;
	outlineView.indentationMarkerFollowsCell = YES;
	outlineView.indentationPerLevel = 16;
	outlineView.autoresizesOutlineColumn = NO;
	outlineView.dataSource = self;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = outlineView;

	_addButton    = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameAddTemplate] target:_treeController action:@selector(insert:)];
	_removeButton = [NSButton buttonWithImage:[NSImage imageNamed:NSImageNameRemoveTemplate] target:_treeController action:@selector(remove:)];
	for(NSButton* button in @[ _addButton, _removeButton ])
		button.bezelStyle = NSSmallSquareBezelStyle;

	NSDictionary* leftViews = @{
		@"scrollView":   scrollView,
		@"add":          _addButton,
		@"remove":       _removeButton,
	};

	NSView* leftContentView = [[NSView alloc] initWithFrame:NSZeroRect];
	GenieAddAutoLayoutViewsToSuperview(leftViews, leftContentView);

	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView]-(6)-|"                       options:0 metrics:nil views:leftViews]];
	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[add(==21)]-(-1)-[remove(==21)]-(>=20)-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:leftViews]];
	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView]-[add(==21)]-|"               options:0 metrics:nil views:leftViews]];

	[outlineView   bind:NSContentBinding             toObject:_treeController withKeyPath:@"arrangedObjects" options:nil];
	[outlineView   bind:NSSelectionIndexPathsBinding toObject:_treeController withKeyPath:NSSelectionIndexPathsBinding options:nil];
	[enabledColumn bind:NSValueBinding               toObject:_treeController withKeyPath:@"arrangedObjects.disabled" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[tableColumn   bind:NSValueBinding               toObject:_treeController withKeyPath:@"arrangedObjects.displayName" options:nil];
	[tableColumn   bind:NSEditableBinding            toObject:_treeController withKeyPath:@"arrangedObjects.canEditDisplayName" options:nil];

	[_addButton    bind:NSEnabledBinding toObject:_treeController withKeyPath:@"canInsert" options:nil];
	[_removeButton bind:NSEnabledBinding toObject:_treeController withKeyPath:@"canRemove" options:nil];

	_addButton.target    = self;
	_addButton.action    = @selector(insertCatalogItem:);

	_removeButton.target = self;
	_removeButton.action = @selector(removeCatalogItem:);

	// =======================
	// = Right of Split View =
	// =======================

	MBMenu const items = {
		{ @"Group Item",            .tag = kGenieItemKindGroup,                       },
		{ @"Action",                @selector(nop:)                                   },
		{ @"Go to Web Address",     .tag = kGenieItemKindWebAddress,      .indent = 1 },
		{ @"Run Script",            .tag = kGenieItemKindRunScript,       .indent = 1 },
		{ @"Open File",             .tag = kGenieItemKindOpenFile,        .indent = 1 },
		{ @"Data Source",           @selector(nop:)                                   },
		{ @"Spotlight Search",      .tag = kGenieItemKindSpotlight,       .indent = 1 },
		{ @"SQLite Search",         .tag = kGenieItemKindSQLite,          .indent = 1 },
		{ @"Items from Script",     .tag = kGenieItemKindCommandResult,   .indent = 1 },
		{ @"Recent Documents",      .tag = kGenieItemKindRecentDocuments, .indent = 1 },
		{ @"Child Actions",         @selector(nop:)                                   },
		{ @"Predicate Group",       .tag = kGenieItemKindPredicateGroup,  .indent = 1 },
	};

	NSTextField* actionLabel   = [NSTextField labelWithString:@"Item Type:"];
	NSPopUpButton* popUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	popUpButton.menu = MBCreateMenu(items);

	NSBox* leftSeparator  = [[NSBox alloc] initWithFrame:NSZeroRect];
	NSBox* rightSeparator = [[NSBox alloc] initWithFrame:NSZeroRect];

	for(NSBox* separator in @[ leftSeparator, rightSeparator ])
	{
		separator.boxType = NSBoxSeparator;
		[separator setContentHuggingPriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationHorizontal];
	}

	_containerView  = [[NSView alloc] initWithFrame:NSZeroRect];
	_advancedButton = [NSButton buttonWithTitle:@"Advanced…" target:nil action:@selector(showAdvancedSettings:)];

	NSDictionary* boxedViews = @{
		@"actionLabel":     actionLabel,
		@"action":          popUpButton,
		@"leftSeparator":   leftSeparator,
		@"rightSeparator":  rightSeparator,
		@"container":       _containerView,
		@"advanced":        _advancedButton,
	};

	NSBox* boxView = [[NSBox alloc] initWithFrame:NSZeroRect];
	boxView.titlePosition = NSNoTitle;
	GenieAddAutoLayoutViewsToSuperview(boxedViews, boxView);

	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[leftSeparator(>=10)]-[actionLabel]"               options:NSLayoutFormatAlignAllCenterY metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[actionLabel]-[action]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[action]-[rightSeparator(==leftSeparator)]-|"        options:0 metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[container]|"                                       options:0 metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[advanced]-|"                               options:0 metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[action]-[container][advanced]-|"                  options:0 metrics:nil views:boxedViews]];
	[boxView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[leftSeparator(==rightSeparator,==1)]"               options:0 metrics:nil views:boxedViews]];

	NSLayoutConstraint* separatorConstraint = [NSLayoutConstraint constraintWithItem:leftSeparator attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:rightSeparator attribute:NSLayoutAttributeTop multiplier:1 constant:0];
	[boxView addConstraint:separatorConstraint];

	[popUpButton bind:NSSelectedTagBinding toObject:_treeController withKeyPath:@"selection.kind" options:nil];
	[self bind:@"countOfAdvancedKeys" toObject:_treeController withKeyPath:@"selection.countOfAdvancedKeys" options:nil];

	NSDictionary* rightViews = @{
		@"box":     boxView,
	};

	NSView* rightContentView = [[NSView alloc] initWithFrame:NSZeroRect];
	GenieAddAutoLayoutViewsToSuperview(rightViews, rightContentView);

	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(6)-[box]-|" options:0 metrics:nil views:rightViews]];

	// ==============
	// = Split View =
	// ==============

	GenieSetupKeyViewLoop(@[ leftContentView, outlineView, _addButton, _removeButton, popUpButton, _containerView, _advancedButton ]);

	NSSplitView* splitView = [[NSSplitView alloc] initWithFrame:NSZeroRect];
	splitView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	splitView.vertical = YES;

	[splitView addSubview:leftContentView];
	[splitView addSubview:rightContentView];
	[splitView setHoldingPriority:NSLayoutPriorityDefaultLow+1 forSubviewAtIndex:0];

	NSLayoutConstraint* boxTopConstraint    = [NSLayoutConstraint constraintWithItem:boxView attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:scrollView attribute:NSLayoutAttributeTop multiplier:1 constant:0];
	NSLayoutConstraint* boxBottomConstraint = [NSLayoutConstraint constraintWithItem:boxView attribute:NSLayoutAttributeBottom relatedBy:NSLayoutRelationEqual toItem:scrollView attribute:NSLayoutAttributeBottom multiplier:1 constant:0];
	[splitView addConstraints:@[ boxTopConstraint, boxBottomConstraint ]];

	_splitView = splitView;

	self.initialFirstResponder = _outlineView;
	self.view = _splitView;

	// Select the proper view controller for currently selected item
	self.selectedItemKind = _selectedItemKind;
}

- (void)viewWillAppear
{
	if(![[NSUserDefaults standardUserDefaults] objectForKey:@"NSSplitView Subview Frames Catalog"])
		[_splitView setPosition:225 ofDividerAtIndex:0];
	[_splitView setAutosaveName:@"Catalog"];

	[super viewWillAppear];
}

- (void)viewDidAppear
{
	NSInteger selectedRow = _outlineView.selectedRow;
	if(selectedRow > 0)
		[_outlineView scrollRowToVisible:selectedRow];
	[super viewDidAppear];
}

- (void)viewDidDisappear
{
	[super viewDidDisappear];
	[GenieManager.sharedInstance synchronize];
}

- (BOOL)commitEditing
{
	BOOL res = [super commitEditing] && [_treeController commitEditing];
	if(GenieItem* selectedItem = _treeController.selectedObjects.firstObject)
			[[NSUserDefaults standardUserDefaults] setObject:selectedItem.identifier forKey:@"selectedItemIdentifier"];
	else	[[NSUserDefaults standardUserDefaults] removeObjectForKey:@"selectedItemIdentifier"];
	return res;
}

- (void)setSelectedItemKind:(GenieItemKind)newSelectedItemKind
{
	_selectedItemKind = newSelectedItemKind;

	NSString* tag = [NSString stringWithFormat:@"%lu", _selectedItemKind];
	if(NSViewController* viewController = _viewControllers[tag])
	{
		NSView* newView = viewController.view;
		newView.frame = { NSZeroPoint, _containerView.frame.size };

		if(NSView* oldView = _containerView.subviews.firstObject)
				[_containerView replaceSubview:oldView with:newView];
		else	[_containerView addSubview:newView];
	}
	else
	{
		NSLog(@"[%@ setSelectedItemKind:%@] no view controller for this kind", [self class], @(newSelectedItemKind));
	}
}

// =======================
// = Data Source Dry Run =
// =======================

- (GenieItem*)findDataSourceItem
{
	GenieItem* item = _treeController.selectedObjects.firstObject;
	while(item)
	{
		switch(item.kind)
		{
			case kGenieItemKindSpotlight:
			case kGenieItemKindSQLite:
			case kGenieItemKindCommandResult:
			case kGenieItemKindRecentDocuments:
				return item;
			break;

			default:
				item = item.parentItem;
			break;
		}
	}

	item = _treeController.selectedObjects.firstObject;
	while(item)
	{
		switch(item.kind)
		{
			case kGenieItemKindSpotlight:
			case kGenieItemKindSQLite:
			case kGenieItemKindCommandResult:
			case kGenieItemKindRecentDocuments:
				return item;
			break;

			default:
				item = item.children.firstObject;
			break;
		}
	}

	return nil;
}

- (void)performDataSourceDryRun:(id)sender
{
	[_treeController commitEditing];

	if(GenieItem* dataSourceItem = [self findDataSourceItem])
	{
		DryRunViewController* viewController = [[DryRunViewController alloc] initWithDataSourceItem:dataSourceItem];
		[self presentViewControllerAsSheet:viewController];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(performDataSourceDryRun:))
		return [self findDataSourceItem] != nil;
	else if(aMenuItem.action == @selector(delete:))
		return _removeButton.isEnabled;
	return YES;
}

// =======================
// = Create/delete Items =
// =======================

- (void)newDocument:(id)sender
{
	[self insertCatalogItem:sender];
}

- (void)insertCatalogItem:(id)sender
{
	NSDictionary* defaultValues = @{
		@"uid":   [[NSUUID UUID] UUIDString],
		@"kind":  @"script",
		@"title": @"New Item",
	};

	GenieItem* newItem = [[GenieItem alloc] initWithValues:defaultValues parentItem:nil directory:nil];

	NSIndexPath* indexPath = _treeController.selectionIndexPath;
	if(!indexPath)
		indexPath = [NSIndexPath indexPathWithIndex:[[_treeController.arrangedObjects childNodes] count]];
	[_treeController insertObject:newItem atArrangedObjectIndexPath:indexPath];

	NSArray<NSTableColumn*>* tableColumns = _outlineView.tableColumns;
	for(NSUInteger i = 0; i < tableColumns.count; ++i)
	{
		if(tableColumns[i].isEditable && [tableColumns[i].dataCell isKindOfClass:[NSTextFieldCell class]])
		{
			NSInteger rowIndex = _outlineView.selectedRow;
			if(rowIndex != -1)
				[_outlineView editColumn:i row:rowIndex withEvent:nil select:YES];
			break;
		}
	}
}

- (void)removeCatalogItem:(id)sender
{
	[_treeController remove:sender];
}

// ==================
// = Advanced Sheet =
// ==================

- (void)showAdvancedSettings:(id)sender
{
	_textViewScrollView = GenieCreateTextView();
	_textView = _textViewScrollView.documentView;

	[_textView bind:NSValueBinding toObject:_treeController withKeyPath:@"selection.plistDump" options:0];

	NSButton* okButton     = [NSButton buttonWithTitle:@"OK" target:self action:@selector(acceptAdvancedSettings:)];
	NSButton* cancelButton = [NSButton buttonWithTitle:@"Cancel" target:self action:@selector(cancelAdvancedSettings:)];
	[okButton setKeyEquivalent:@"\r"];
	[cancelButton setKeyEquivalent:@"\e"];

	NSDictionary* views = @{
		@"textView":  _textViewScrollView,
		@"ok":        okButton,
		@"cancel":    cancelButton,
	};

	NSView* contentView = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 200, 200)];
	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[textView(>=200)]-|"             options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=8)-[cancel]-[ok(==cancel)]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[textView]-[ok]-|"               options:0 metrics:nil views:views]];

	NSWindow* sheet = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 500, 500) styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskResizable|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO];
	sheet.contentView = contentView;

	[self.view.window beginSheet:sheet completionHandler:^(NSModalResponse){
		[_textView unbind:NSValueBinding];
	}];
}

- (void)acceptAdvancedSettings:(id)sender
{
	if([_treeController commitEditing])
		[self.view.window endSheet:[sender window] returnCode:NSModalResponseOK];
}

- (void)cancelAdvancedSettings:(id)sender
{
 	[self.view.window endSheet:[sender window] returnCode:NSModalResponseCancel];
}

// ===================
// = Catalog Support =
// ===================

- (void)delete:(id)sender
{
	[_removeButton performClick:self];
}

- (void)cancel:(id)sender
{
	NSResponder* firstResponder = _outlineView.window.firstResponder;
	if([firstResponder isKindOfClass:[NSTextView class]] && ((NSTextView*)firstResponder).delegate == _outlineView)
	{
		[_treeController discardEditing];
		[_outlineView.window makeFirstResponder:_outlineView];
	}
	else
	{
		[self.nextResponder doCommandBySelector:@selector(cancel:)];
	}
}

- (NSInteger)outlineView:(NSOutlineView*)anOutlineView numberOfChildrenOfItem:(id)item                                 { return 0; }
- (BOOL)outlineView:(NSOutlineView*)anOutlineView isItemExpandable:(id)item                                            { return NO; }
- (id)outlineView:(NSOutlineView*)anOutlineView child:(NSInteger)index ofItem:(id)item                                 { return nil; }
- (id)outlineView:(NSOutlineView*)anOutlineView objectValueForTableColumn:(NSTableColumn*)aTableColumn byItem:(id)item { return nil; }

- (void)outlineView:(NSOutlineView*)outlineView draggingSession:(NSDraggingSession*)session willBeginAtPoint:(NSPoint)screenPoint forItems:(NSArray*)draggedItems
{
	_draggedNodes = draggedItems;
	[session.draggingPasteboard setData:[NSData data] forType:_dragType];
}

- (void)outlineView:(NSOutlineView*)outlineView draggingSession:(NSDraggingSession*)session endedAtPoint:(NSPoint)screenPoint operation:(NSDragOperation)operation
{
	if(operation == NSDragOperationDelete)
		NSLog(@"%s trash items", sel_getName(_cmd));
	_draggedNodes = nil;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView writeItems:(NSArray*)items toPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray* itemUids = [NSMutableArray array];
	for(NSTreeNode* item in items)
		[itemUids addObject:[item.representedObject identifier]];

	[pboard clearContents];
	[pboard setPropertyList:itemUids forType:_dragType];
	return YES;
}

- (NSDragOperation)outlineView:(NSOutlineView*)anOutlineView validateDrop:(id<NSDraggingInfo>)info proposedItem:(id)item proposedChildIndex:(NSInteger)childIndex
{
	BOOL optionDown = ([NSEvent modifierFlags] & NSEventModifierFlagOption) == NSEventModifierFlagOption;
	return optionDown ? NSDragOperationCopy : NSDragOperationMove;
}

- (BOOL)outlineView:(NSOutlineView*)anOutlineView acceptDrop:(id<NSDraggingInfo>)info item:(id)item childIndex:(NSInteger)childIndex
{
	// TODO Duplicate
	// TODO Check if drop destination is valid (not dragging item into itself)
	// TODO Drag to trash?

	if(_draggedNodes)
	{
		if(childIndex == NSOutlineViewDropOnItemIndex)
			childIndex = 0;

		NSIndexPath* indexPath = item ? [((NSTreeNode*)item).indexPath indexPathByAddingIndex:childIndex] : [NSIndexPath indexPathWithIndex:childIndex];
		NSLog(@"%s move %@ to %@", sel_getName(_cmd), _draggedNodes, indexPath);
		[_treeController moveNodes:_draggedNodes toIndexPath:indexPath];
		return YES;
	}

	// NSPasteboard* pboard = info.draggingPasteboard;
	// if(NSArray* draggedUids = [pboard propertyListForType:_dragType])
	// {
	// 	NSDragOperation op = [info draggingSourceOperationMask];
	// 	BOOL move = (op & NSDragOperationMove) == NSDragOperationMove;
	// }
	return NO;
}
@end
