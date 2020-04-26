#import "ProjectsPreferences.h"
#import "Keys.h"
#import <settings/settings.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakTabBarView/OakTabBarView.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <MenuBuilder/MenuBuilder.h>

@interface ProjectsPreferences ()
{
	NSPopUpButton* fileBrowserPathPopUp;
}
@end

@implementation ProjectsPreferences
- (id)init
{
	if(self = [super initWithNibName:nil label:@"Projects" image:[NSImage imageNamed:@"Projects" inSameBundleAsClass:[self class]]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakFileBrowserPlacementSettingsTransformer" andObjectsArray:@[ @"left", @"right" ]];
		[OakStringListTransformer createTransformerWithName:@"OakHTMLOutputPlacementSettingsTransformer" andObjectsArray:@[ @"bottom", @"right", @"window" ]];

		self.defaultsProperties = @{
			@"foldersOnTop":                 kUserDefaultsFoldersOnTopKey,
			@"showFileExtensions":           kUserDefaultsShowFileExtensionsKey,
			@"disableTabBarCollapsing":      kUserDefaultsDisableTabBarCollapsingKey,
			@"disableAutoResize":            kUserDefaultsDisableFileBrowserWindowResizeKey,
			@"autoRevealFile":               kUserDefaultsAutoRevealFileKey,
			@"fileBrowserPlacement":         kUserDefaultsFileBrowserPlacementKey,
			@"htmlOutputPlacement":          kUserDefaultsHTMLOutputPlacementKey,

			@"allowExpandingLinks":          kUserDefaultsAllowExpandingLinksKey,
			@"fileBrowserSingleClickToOpen": kUserDefaultsFileBrowserSingleClickToOpenKey,
			@"disableTabReordering":         kUserDefaultsDisableTabReorderingKey,
			@"disableTabAutoClose":          kUserDefaultsDisableTabAutoCloseKey,
		};

		self.tmProperties = @{
			@"excludePattern": [NSString stringWithCxxString:kSettingsExcludeKey],
			@"includePattern": [NSString stringWithCxxString:kSettingsIncludeKey],
			@"binaryPattern":  [NSString stringWithCxxString:kSettingsBinaryKey],
		};
	}
	return self;
}

- (void)selectOtherFileBrowserPath:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	[openPanel setCanChooseFiles:NO];
	[openPanel setCanChooseDirectories:YES];
	[openPanel beginSheetModalForWindow:[self view].window completionHandler:^(NSModalResponse result) {
		if(result == NSModalResponseOK)
			[NSUserDefaults.standardUserDefaults setObject:[[openPanel URL] absoluteString] forKey:kUserDefaultsInitialFileBrowserURLKey];
		[self updatePathPopUp];
	}];
}

- (void)takeFileBrowserPathFrom:(id)sender
{
	[NSUserDefaults.standardUserDefaults setObject:[[sender representedObject] absoluteString] forKey:kUserDefaultsInitialFileBrowserURLKey];
	[self updatePathPopUp];
}

- (NSMenuItem*)menuItemForURL:(NSURL*)aURL
{
	NSMenuItem* res = [[NSMenuItem alloc] initWithTitle:[NSFileManager.defaultManager displayNameAtPath:[aURL path]] action:@selector(takeFileBrowserPathFrom:) keyEquivalent:@""];
	[res setTarget:self];
	[res setRepresentedObject:aURL];
	if([aURL isFileURL])
		[res setIconForFile:[aURL path]];
	return res;
}

- (void)updatePathPopUp
{
	NSMenu* menu = [fileBrowserPathPopUp menu];
	[menu removeAllItems];

	NSArray<NSURL*>* const defaultURLs = @[
		[NSFileManager.defaultManager URLForDirectory:NSDesktopDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:NO error:nil],
		NSFileManager.defaultManager.homeDirectoryForCurrentUser,
		[NSURL fileURLWithPath:@"/" isDirectory:YES],
	];

	NSURL* url = defaultURLs[1];
	if(NSString* urlString = [NSUserDefaults.standardUserDefaults stringForKey:kUserDefaultsInitialFileBrowserURLKey])
		url = [NSURL URLWithString:urlString];

	if(![defaultURLs containsObject:url])
	{
		[menu addItem:[self menuItemForURL:url]];
		[menu addItem:[NSMenuItem separatorItem]];
	}

	for(NSURL* defaultURL in defaultURLs)
	{
		[menu addItem:[self menuItemForURL:defaultURL]];
		if([defaultURL isEqual:url])
			[fileBrowserPathPopUp selectItemAtIndex:[menu numberOfItems]-1];
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[menu addItemWithTitle:@"Other…" action:@selector(selectOtherFileBrowserPath:) keyEquivalent:@""];
}

- (void)loadView
{
	NSPopUpButton* fileBrowserLocationPopUp            = OakCreatePopUpButton();
	NSButton* foldersOnTopCheckBox                     = OakCreateCheckBox(@"Folders on top");
	NSButton* showLinksAsExpandableCheckBox            = OakCreateCheckBox(@"Show links as expandable");
	NSButton* openFilesOnSingleClickCheckBox           = OakCreateCheckBox(@"Open files on single click");
	NSButton* keepCurrentDocumentSelectedCheckBox      = OakCreateCheckBox(@"Keep current document selected");

	NSPopUpButton* fileBrowserPositionPopUp            = OakCreatePopUpButton();
	NSButton* adjustWindowWhenToggleingDisplayCheckBox = OakCreateCheckBox(@"Adjust window when toggleing display");

	NSButton* showForSingleDocumentCheckBox            = OakCreateCheckBox(@"Show for single document");
	NSButton* reOrderWhenOpeningAFileCheckBox          = OakCreateCheckBox(@"Re-order when opening a file");
	NSButton* automaticallyCloseUnusedTabsCheckBox     = OakCreateCheckBox(@"Automatically close unused tabs");

	NSTextField* excludeFilesTextField                 = [NSTextField textFieldWithString:@""];
	NSTextField* includeFilesTextField                 = [NSTextField textFieldWithString:@""];
	NSTextField* nonTextFilesTextField                 = [NSTextField textFieldWithString:@""];

	NSPopUpButton* showCommandOutputPopUp              = OakCreatePopUpButton();

	MBMenu const fileBrowserPositionMenuItems = {
		{ @"Left side",  .tag = 0 },
		{ @"Right side", .tag = 1 },
	};
	MBCreateMenu(fileBrowserPositionMenuItems, fileBrowserPositionPopUp.menu);

	MBMenu const showCommandOutputMenuItems = {
		{ @"Below text view",    .tag = 0 },
		{ @"Right of text view", .tag = 1 },
		{ @"New window",         .tag = 2 },
	};
	MBCreateMenu(showCommandOutputMenuItems, showCommandOutputPopUp.menu);

	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		@[ OakCreateLabel(@"File browser location:"),  fileBrowserLocationPopUp                 ],
		@[ NSGridCell.emptyContentView,                foldersOnTopCheckBox                     ],
		@[ NSGridCell.emptyContentView,                showLinksAsExpandableCheckBox            ],
		@[ NSGridCell.emptyContentView,                openFilesOnSingleClickCheckBox           ],
		@[ NSGridCell.emptyContentView,                keepCurrentDocumentSelectedCheckBox      ],
		@[ ],
		@[ OakCreateLabel(@"Show file browser on:"),   fileBrowserPositionPopUp                 ],
		@[ NSGridCell.emptyContentView,                adjustWindowWhenToggleingDisplayCheckBox ],
		@[ ],
		@[ OakCreateLabel(@"Document tabs:"),          showForSingleDocumentCheckBox            ],
		@[ NSGridCell.emptyContentView,                reOrderWhenOpeningAFileCheckBox          ],
		@[ NSGridCell.emptyContentView,                automaticallyCloseUnusedTabsCheckBox     ],
		@[ ],
		@[ OakCreateLabel(@"Exclude files matching:"), excludeFilesTextField                    ],
		@[ OakCreateLabel(@"Include files matching:"), includeFilesTextField                    ],
		@[ OakCreateLabel(@"Non-text files:"),         nonTextFilesTextField                    ],
		@[ ],
		@[ OakCreateLabel(@"Show command output:"),    showCommandOutputPopUp                   ],
	]];

	for(NSView* popUpButton in @[ fileBrowserPositionPopUp, showCommandOutputPopUp ])
		[popUpButton.widthAnchor constraintEqualToAnchor:fileBrowserLocationPopUp.widthAnchor].active = YES;

	[excludeFilesTextField.widthAnchor constraintEqualToConstant:360].active = YES;
	for(NSView* textField in @[ includeFilesTextField, nonTextFilesTextField ])
		[textField.widthAnchor constraintEqualToAnchor:excludeFilesTextField.widthAnchor].active = YES;

	self.view = OakSetupGridViewWithSeparators(gridView, { 5, 8, 12, 16 });

	fileBrowserPathPopUp = fileBrowserLocationPopUp;
	[self updatePathPopUp];

	[foldersOnTopCheckBox                     bind:NSValueBinding       toObject:self withKeyPath:@"foldersOnTop"                 options:nil];
	[showLinksAsExpandableCheckBox            bind:NSValueBinding       toObject:self withKeyPath:@"allowExpandingLinks"          options:nil];
	[openFilesOnSingleClickCheckBox           bind:NSValueBinding       toObject:self withKeyPath:@"fileBrowserSingleClickToOpen" options:nil];
	[keepCurrentDocumentSelectedCheckBox      bind:NSValueBinding       toObject:self withKeyPath:@"autoRevealFile"               options:nil];
	[fileBrowserPositionPopUp                 bind:NSSelectedTagBinding toObject:self withKeyPath:@"fileBrowserPlacement"         options:@{ NSValueTransformerNameBindingOption: @"OakFileBrowserPlacementSettingsTransformer" }];
	[adjustWindowWhenToggleingDisplayCheckBox bind:NSValueBinding       toObject:self withKeyPath:@"disableAutoResize"            options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[showForSingleDocumentCheckBox            bind:NSValueBinding       toObject:self withKeyPath:@"disableTabBarCollapsing"      options:nil];
	[reOrderWhenOpeningAFileCheckBox          bind:NSValueBinding       toObject:self withKeyPath:@"disableTabReordering"         options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[automaticallyCloseUnusedTabsCheckBox     bind:NSValueBinding       toObject:self withKeyPath:@"disableTabAutoClose"          options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[excludeFilesTextField                    bind:NSValueBinding       toObject:self withKeyPath:@"excludePattern"               options:nil];
	[includeFilesTextField                    bind:NSValueBinding       toObject:self withKeyPath:@"includePattern"               options:nil];
	[nonTextFilesTextField                    bind:NSValueBinding       toObject:self withKeyPath:@"binaryPattern"                options:nil];
	[showCommandOutputPopUp                   bind:NSSelectedTagBinding toObject:self withKeyPath:@"htmlOutputPlacement"          options:@{ NSValueTransformerNameBindingOption: @"OakHTMLOutputPlacementSettingsTransformer" }];
}
@end
