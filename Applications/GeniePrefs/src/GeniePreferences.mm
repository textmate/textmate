#import "GeniePreferences.h"
#import "ViewControllers.h"
#import "AddAutoLayoutViews.h"
#import "DryRunController.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <ServiceManagement/ServiceManagement.h>
#import <oak/debug.h>

static NSString* kEnableClipboardHistorySettingsKey = @"enableClipboardHistory";
static NSString* kDisableLaunchAtLoginSettingsKey   = @"disableLaunchAtLogin";

@interface GenieUserDefaultsProxy : NSObject
{
	NSUserDefaults* _genieUserDefaults;
}
@property (nonatomic) BOOL enableClipboardHistory;
@property (nonatomic) BOOL disableLaunchAtLogin;
@end

@implementation GenieUserDefaultsProxy
- (instancetype)init
{
	if(self = [super init])
	{
		_genieUserDefaults      = [[NSUserDefaults alloc] initWithSuiteName:@"com.macromates.Genie"];
		_enableClipboardHistory = [_genieUserDefaults boolForKey:kEnableClipboardHistorySettingsKey];
		_disableLaunchAtLogin   = [_genieUserDefaults boolForKey:kDisableLaunchAtLoginSettingsKey];

		[[NSDistributedNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:@"com.macromates.Genie"];
	}
	return self;
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	[self willChangeValueForKey:@"enableClipboardHistory"];
	_enableClipboardHistory = [_genieUserDefaults boolForKey:kEnableClipboardHistorySettingsKey];
	[self didChangeValueForKey:@"enableClipboardHistory"];

	[self willChangeValueForKey:@"disableLaunchAtLogin"];
	_disableLaunchAtLogin = [_genieUserDefaults boolForKey:kDisableLaunchAtLoginSettingsKey];
	[self didChangeValueForKey:@"disableLaunchAtLogin"];
}

- (void)setEnableClipboardHistory:(BOOL)flag
{
	if(_enableClipboardHistory == flag)
		return;
	[_genieUserDefaults setBool:(_enableClipboardHistory = flag) forKey:kEnableClipboardHistorySettingsKey];
	[_genieUserDefaults synchronize];
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:@"com.macromates.GeniePrefs" userInfo:nil deliverImmediately:YES];
}

- (void)setDisableLaunchAtLogin:(BOOL)flag
{
	if(_disableLaunchAtLogin == flag)
		return;
	[_genieUserDefaults setBool:(_disableLaunchAtLogin = flag) forKey:kDisableLaunchAtLoginSettingsKey];
	[_genieUserDefaults synchronize];
	[[NSDistributedNotificationCenter defaultCenter] postNotificationName:NSUserDefaultsDidChangeNotification object:@"com.macromates.GeniePrefs" userInfo:nil deliverImmediately:YES];
}
@end

@interface GeniePreferences ()
{
	GenieUserDefaultsProxy* _genieSettings;
	NSObjectController* _genieSettingsController;

	NSTreeController* _treeController;

	TableViewController* _variablesTable;
	NSButton* _enableClipboardHistoryButton;
	NSButton* _launchAtLoginButton;

	NSTextView* _textView;
	NSScrollView* _textViewScrollView;

	BOOL _didLoadView;

	// ====================
	// = View Controllers =
	// ====================

	CatalogViewController* _catalogViewController;
	Properties* _properties;

	NSDictionary<NSString*, NSViewController*>* _viewControllers;
	NSSplitView* _splitView;
	NSView* _containerView;

	NSButton* _advancedButton;
}
@property (nonatomic) GenieItemKind selectedItemKind;
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

@implementation GeniePreferences
- (instancetype)init
{
	if(self = [super init])
	{
		_genieSettings           = [[GenieUserDefaultsProxy alloc] init];
		_genieSettingsController = [[NSObjectController alloc] initWithContent:_genieSettings];

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
			[NSString stringWithFormat:@"%lu", kGenieItemKindSqlite]:              [[SqliteProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindCommandResult]:       [[ExecDataSourceProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindRecentDocuments]:     [[RecentDocumentsProperties alloc] initWithTreeController:_treeController],
			[NSString stringWithFormat:@"%lu", kGenieItemKindPredicateGroup]:      [[PredicateGroupProperties alloc] initWithTreeController:_treeController],
		};

		_catalogViewController = [[CatalogViewController alloc] initWithTreeController:_treeController];
		_properties            = [[Properties alloc] initWithTreeController:_treeController];

		[self addChildViewController:_catalogViewController];
		[self addChildViewController:_properties];

		[self bind:@"selectedItemKind" toObject:_treeController withKeyPath:@"selection.kind" options:nil];

		if(NSString* selectedItemIdentifier = [[NSUserDefaults standardUserDefaults] stringForKey:@"selectedItemIdentifier"])
		{
			if(NSIndexPath* indexPath = IndexPathForGenieItemWithIdentifier(selectedItemIdentifier, GenieManager.sharedInstance.items))
				_treeController.selectionIndexPath = indexPath;
		}
	}
	return self;
}

- (void)loadView
{
	if(_didLoadView)
		return;
	_didLoadView = YES;

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
	contentView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;

	NSTabView* tabView = [[NSTabView alloc] initWithFrame:NSZeroRect];
	[tabView addTabViewItem:[self catalogTabViewItem]];
	[tabView addTabViewItem:[self generalTabViewItem]];
	[tabView addTabViewItem:[self changesTabViewItem]];

	NSDictionary* views = @{
		@"tabView": tabView,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[tabView]-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[tabView]-|" options:0 metrics:nil views:views]];

	self.view = contentView;

	if([[NSUserDefaults standardUserDefaults] boolForKey:@"showChanges"])
		[tabView selectTabViewItemWithIdentifier:@"Changes"];

	// Select the proper view controller for currently selected item
	self.selectedItemKind = _selectedItemKind;
}

- (void)viewWillAppear
{
	[_splitView setPosition:225 ofDividerAtIndex:0];
	[_splitView setAutosaveName:@"Catalog"];

	[super viewWillAppear];
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

// ==================
// = Tab View Items =
// ==================

- (NSTabViewItem*)catalogTabViewItem
{
	NSSplitView* splitView = [[NSSplitView alloc] initWithFrame:NSZeroRect];
	splitView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	splitView.vertical = YES;

	[splitView addSubview:_catalogViewController.view];
	[splitView addSubview:_properties.view];
	[splitView setHoldingPriority:NSLayoutPriorityDefaultLow+1 forSubviewAtIndex:0];

	_splitView = splitView;

	NSTabViewItem* tabViewItem = [[NSTabViewItem alloc] initWithIdentifier:@"General"];
	tabViewItem.label = @"Catalog";
	tabViewItem.view = splitView;
	tabViewItem.initialFirstResponder = _catalogViewController.outlineView;

	_containerView = _properties.containerView;

	_catalogViewController.addButton.target    = self;
	_catalogViewController.addButton.action    = @selector(insertCatalogItem:);
	_catalogViewController.removeButton.target = self;
	_catalogViewController.removeButton.action = @selector(removeCatalogItem:);

	return tabViewItem;
}

- (NSTabViewItem*)generalTabViewItem
{
	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	NSTextField* variablesLabel = [NSTextField labelWithString:@"Variables:"];
	_variablesTable = [[TableViewController alloc] initWithColumnNames:@[ @"disabled", @"name", @"value" ] visibleRows:5 showHeaderView:YES prototype:@{ @"name": @"variable", @"value": @"value" }];

	_launchAtLoginButton          = [NSButton checkboxWithTitle:@"Launch at Login" target:nil action:nil];
	_enableClipboardHistoryButton = [NSButton checkboxWithTitle:@"Clipboard History" target:nil action:nil];

	[_enableClipboardHistoryButton bind:NSValueBinding toObject:_genieSettingsController withKeyPath:@"content.enableClipboardHistory" options:nil];
	[_launchAtLoginButton bind:NSValueBinding toObject:_genieSettingsController withKeyPath:@"content.disableLaunchAtLogin" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];

	NSDictionary* views = @{
		@"variablesLabel":    variablesLabel,
		@"variables":         _variablesTable.view,
		@"launchAtLogin":     _launchAtLoginButton,
		@"clipboardHistory":  _enableClipboardHistoryButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[variablesLabel]-[variables]-|"                          options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[launchAtLogin]-(>=20)-|"                                  options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[clipboardHistory]-(>=20)-|"                               options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[variables]-[launchAtLogin]-[clipboardHistory]-(>=20)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[_variablesTable.arrayController bind:NSContentArrayBinding toObject:GenieManager.sharedInstance withKeyPath:@"variables" options:nil];

	NSArray<NSTableColumn*>* tableColumns = _variablesTable.tableView.tableColumns;
	tableColumns[0].title = @"";
	tableColumns[1].title = @"Variable Name";
	tableColumns[2].title = @"Value";

	NSTabViewItem* tabViewItem = [[NSTabViewItem alloc] initWithIdentifier:@"General"];
	tabViewItem.label = @"General";
	tabViewItem.view = contentView;
	tabViewItem.initialFirstResponder = _variablesTable.tableView;
	return tabViewItem;
}

- (NSTabViewItem*)changesTabViewItem
{
	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	WKWebViewConfiguration* webConfig = [[WKWebViewConfiguration alloc] init];
	webConfig.suppressesIncrementalRendering = YES;

	WKWebView* webView = [[WKWebView alloc] initWithFrame:NSZeroRect configuration:webConfig];

	NSDictionary* views = @{ @"webView": webView };
	GenieAddAutoLayoutViewsToSuperview(views, contentView);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[webView(<=600,==600@100)]-(>=20)-|" options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[webView]-|" options:0 metrics:nil views:views]];
	[contentView addConstraint:[NSLayoutConstraint constraintWithItem:webView attribute:NSLayoutAttributeCenterX relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeCenterX multiplier:1 constant:0]];

	if(NSURL* url = [[NSBundle mainBundle] URLForResource:@"Changes" withExtension:@"html"])
		[webView loadFileURL:url allowingReadAccessToURL:[NSURL fileURLWithPath:[url.path stringByDeletingLastPathComponent]]];

	NSTabViewItem* tabViewItem = [[NSTabViewItem alloc] initWithIdentifier:@"Changes"];
	tabViewItem.label = @"Changes";
	tabViewItem.view = contentView;
	tabViewItem.initialFirstResponder = webView;
	return tabViewItem;
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
			case kGenieItemKindSqlite:
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
			case kGenieItemKindSqlite:
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
	return [super validateMenuItem:aMenuItem];
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

	NSArray<NSTableColumn*>* tableColumns = _catalogViewController.outlineView.tableColumns;
	for(NSUInteger i = 0; i < tableColumns.count; ++i)
	{
		if(tableColumns[i].isEditable && [tableColumns[i].dataCell isKindOfClass:[NSTextFieldCell class]])
		{
			NSInteger rowIndex = _catalogViewController.outlineView.selectedRow;
			if(rowIndex != -1)
				[_catalogViewController.outlineView editColumn:i row:rowIndex withEvent:nil select:YES];
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
@end
