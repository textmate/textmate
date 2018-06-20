#import "GeniePreferences.h"
#import "GenieTableViewController.h"
#import "ViewControllers.h"
#import "AddAutoLayoutViews.h"
#import "DryRunController.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <MenuBuilder/MenuBuilder.h>
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

@interface GeneralSettingsViewController ()
{
	GenieUserDefaultsProxy* _genieSettings;
	NSObjectController* _genieSettingsController;

	GenieTableViewController* _variablesTable;

	NSButton* _launchAtLoginButton;
	NSButton* _enableClipboardHistoryButton;
}
@property (nonatomic) NSResponder* initialFirstResponder;
@end

@implementation GeneralSettingsViewController
- (instancetype)init
{
	if(self = [super init])
	{
		self.title = @"General";

		_genieSettings           = [[GenieUserDefaultsProxy alloc] init];
		_genieSettingsController = [[NSObjectController alloc] initWithContent:_genieSettings];
	}
	return self;
}

- (void)loadView
{
	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	NSTextField* variablesLabel = [NSTextField labelWithString:@"Variables:"];
	_variablesTable = [[GenieTableViewController alloc] initWithColumnNames:@[ @"disabled", @"name", @"value" ] visibleRows:5 showHeaderView:YES prototype:@{ @"name": @"variable", @"value": @"value" }];

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
	GenieSetupKeyViewLoop(@[ contentView, _variablesTable.view, _launchAtLoginButton, _enableClipboardHistoryButton ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[variablesLabel]-[variables]-|"                          options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[launchAtLogin]-(>=20)-|"                                  options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[clipboardHistory]-(>=20)-|"                               options:0 metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[variables]-[launchAtLogin]-[clipboardHistory]-(>=20)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[_variablesTable.arrayController bind:NSContentArrayBinding toObject:GenieManager.sharedInstance withKeyPath:@"variables" options:nil];

	NSArray<NSTableColumn*>* tableColumns = _variablesTable.tableView.tableColumns;
	tableColumns[0].title = @"";
	tableColumns[1].title = @"Variable Name";
	tableColumns[2].title = @"Value";

	self.initialFirstResponder = _variablesTable.tableView;
	self.view = contentView;
}
@end

@interface ChangesViewController ()
@property (nonatomic) NSResponder* initialFirstResponder;
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

	self.initialFirstResponder = webView;
	self.view = contentView;
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
			[NSString stringWithFormat:@"%lu", kGenieItemKindSqlite]:              [[SqliteProperties alloc] initWithTreeController:_treeController],
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

	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView]|"                           options:0 metrics:nil views:leftViews]];
	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[add(==21)]-(-1)-[remove(==21)]-(>=0)-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:leftViews]];
	[leftContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[scrollView]-[add(==21)]-|"              options:0 metrics:nil views:leftViews]];

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
		{ @"Sqlite Search",         .tag = kGenieItemKindSqlite,          .indent = 1 },
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

	NSDictionary* rightViews = @{
		@"actionLabel":     actionLabel,
		@"action":          popUpButton,
		@"leftSeparator":   leftSeparator,
		@"rightSeparator":  rightSeparator,
		@"container":       _containerView,
		@"advanced":        _advancedButton,
	};

	NSView* rightContentView = [[NSView alloc] initWithFrame:NSZeroRect];
	GenieAddAutoLayoutViewsToSuperview(rightViews, rightContentView);

	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[leftSeparator(>=10)]-[actionLabel]"               options:NSLayoutFormatAlignAllCenterY metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[actionLabel]-[action]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[action]-[rightSeparator(==leftSeparator)]-|"        options:0 metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[container]|"                                       options:0 metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=20)-[advanced]-|"                               options:0 metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[action]-[container][advanced]-|"                  options:0 metrics:nil views:rightViews]];
	[rightContentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[leftSeparator(==rightSeparator,==1)]"               options:0 metrics:nil views:rightViews]];

	NSLayoutConstraint* separatorConstraint = [NSLayoutConstraint constraintWithItem:leftSeparator attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:rightSeparator attribute:NSLayoutAttributeTop multiplier:1 constant:0];
	[rightContentView addConstraint:separatorConstraint];

	[popUpButton bind:NSSelectedTagBinding toObject:_treeController withKeyPath:@"selection.kind" options:nil];

	[self bind:@"countOfAdvancedKeys" toObject:_treeController withKeyPath:@"selection.countOfAdvancedKeys" options:nil];

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

	_splitView = splitView;

	self.initialFirstResponder = _outlineView;
	self.view = _splitView;

	// Select the proper view controller for currently selected item
	self.selectedItemKind = _selectedItemKind;
}

- (void)viewWillAppear
{
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
	else if(aMenuItem.action == @selector(delete:))
		return _removeButton.isEnabled;
	return [super respondsToSelector:@selector(validateMenuItem:)] ? [super validateMenuItem:aMenuItem] : YES;
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
