#import "BundlesPreferences.h"
#import <BundlesManager/BundlesManager.h>
#import <OakFoundation/OakFoundation.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakScopeBarView.h>

static NSUserInterfaceItemIdentifier const kTableColumnIdentifierInstalled   = @"Installed";
static NSUserInterfaceItemIdentifier const kTableColumnIdentifierBundleName  = @"BundleName";
static NSUserInterfaceItemIdentifier const kTableColumnIdentifierWebLink     = @"WebLink";
static NSUserInterfaceItemIdentifier const kTableColumnIdentifierUpdated     = @"Updated";
static NSUserInterfaceItemIdentifier const kTableColumnIdentifierDescription = @"Description";

static NSMutableSet* BundlesBeingInstalled = [NSMutableSet set];

@interface Bundle (BundlesInstallPreferences)
@property (nonatomic) NSControlStateValue installedCellState;
@end

@implementation Bundle (BundlesInstallPreferences)
+ (NSSet*)keyPathsForValuesAffectingInstalledCellState
{
	return [NSSet setWithObjects:@"installed", nil];
}

- (NSControlStateValue)installedCellState
{
	return [BundlesBeingInstalled containsObject:self] ? NSControlStateValueMixed : (self.isInstalled ? NSControlStateValueOn : NSControlStateValueOff);
}

- (void)setInstalledCellState:(NSControlStateValue)newValue
{
	BundlesManager* manager = BundlesManager.sharedInstance;
	if(newValue == NSControlStateValueOff)
	{
		[manager uninstallBundle:self];
		manager.activityText = [NSString stringWithFormat:@"Uninstalled ‘%@’.", self.name];
	}
	else if(![BundlesBeingInstalled containsObject:self])
	{
		[BundlesBeingInstalled addObject:self];

		manager.isBusy = YES;
		manager.activityText = [NSString stringWithFormat:@"Installing ‘%@’…", self.name];

		[manager installBundles:@[ self ] completionHandler:^(NSArray<Bundle*>* bundles){
			[self willChangeValueForKey:@"installedCellState"];
			[BundlesBeingInstalled removeObject:self];
			[self didChangeValueForKey:@"installedCellState"];

			if(!self.installed)
				manager.activityText = [NSString stringWithFormat:@"Error installing ‘%@’.", self.name];
			else if(bundles.count == 1)
				manager.activityText = [NSString stringWithFormat:@"Installed ‘%@’.", self.name];
			else if(bundles.count == 2)
				manager.activityText = [NSString stringWithFormat:@"Installed ‘%@’ and one dependency.", self.name];
			else
				manager.activityText = [NSString stringWithFormat:@"Installed ‘%@’ and %ld dependencies.", self.name, bundles.count-1];
			manager.isBusy = NO;
		}];
	}
}
@end

@interface BundlesPreferences () <NSTableViewDelegate>
{
	NSMutableSet*              _enabledCategories;
	NSArrayController*         _arrayController;
	OakScopeBarViewController* _scopeBar;
	NSSearchField*             _searchField;
	NSTableView*               _bundlesTableView;
}
@property (nonatomic) NSUInteger selectedIndex;
@end

@implementation BundlesPreferences
- (NSImage*)toolbarItemImage { return [NSWorkspace.sharedWorkspace iconForFileType:@"tmbundle"]; }
- (NSView*)initialKeyView    { return _bundlesTableView; }

- (id)init
{
	if(self = [self initWithNibName:nil bundle:nil])
	{
		self.identifier = @"Bundles";
		self.title      = @"Bundles";

		_enabledCategories = [NSMutableSet set];
		_selectedIndex     = NSNotFound;

		_scopeBar = [[OakScopeBarViewController alloc] init];
		_scopeBar.allowsEmptySelection = YES;
		_scopeBar.controlSize = NSControlSizeSmall;
	}
	return self;
}

- (NSTableColumn*)columnWithIdentifier:(NSUserInterfaceItemIdentifier)identifier title:(NSString*)title editable:(BOOL)editable width:(CGFloat)width resizingMask:(NSTableColumnResizingOptions)resizingMask
{
	NSTableColumn* tableColumn = [[NSTableColumn alloc] initWithIdentifier:identifier];

	tableColumn.title        = title;
	tableColumn.editable     = editable;
	tableColumn.width        = width;
	tableColumn.resizingMask = resizingMask;

	if(resizingMask == NSTableColumnNoResizing)
	{
		tableColumn.minWidth = width;
		tableColumn.maxWidth = width;
	}

	return tableColumn;
}

- (void)loadView
{
	NSMutableSet* categories = [NSMutableSet set];
	for(Bundle* bundle in BundlesManager.sharedInstance.bundles)
	{
		if(NSString* category = bundle.category)
			[categories addObject:category];
	}
	_scopeBar.labels = [[categories allObjects] sortedArrayUsingSelector:@selector(localizedCompare:)];

	_searchField = [[NSSearchField alloc] initWithFrame:NSZeroRect];
	_searchField.controlSize = NSControlSizeSmall;
	_searchField.font        = [NSFont systemFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	_searchField.action      = @selector(filterStringDidChange:);
	[_searchField.cell setScrollable:YES];
	[_searchField.cell setSendsSearchStringImmediately:YES];

	_arrayController = [[NSArrayController alloc] init];
	_arrayController.avoidsEmptySelection = NO;
	_arrayController.sortDescriptors = @[
		[NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)],
		[NSSortDescriptor sortDescriptorWithKey:@"installed" ascending:YES],
		[NSSortDescriptor sortDescriptorWithKey:@"downloadLastUpdated" ascending:YES],
		[NSSortDescriptor sortDescriptorWithKey:@"textSummary" ascending:YES selector:@selector(localizedCompare:)]
	];

	NSTableColumn* installedTableColumn   = [self columnWithIdentifier:kTableColumnIdentifierInstalled   title:@""            editable:YES width:16  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* bundleTableColumn      = [self columnWithIdentifier:kTableColumnIdentifierBundleName  title:@"Bundle"      editable:NO  width:140 resizingMask:NSTableColumnUserResizingMask];
	NSTableColumn* linkTableColumn        = [self columnWithIdentifier:kTableColumnIdentifierWebLink     title:@""            editable:NO  width:16  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* updatedTableColumn     = [self columnWithIdentifier:kTableColumnIdentifierUpdated     title:@"Updated"     editable:NO  width:90  resizingMask:NSTableColumnNoResizing];
	NSTableColumn* descriptionTableColumn = [self columnWithIdentifier:kTableColumnIdentifierDescription title:@"Description" editable:NO  width:140 resizingMask:NSTableColumnAutoresizingMask];

	NSButtonCell* installedCell = [[NSButtonCell alloc] init];
	installedCell.buttonType       = NSButtonTypeSwitch;
	installedCell.allowsMixedState = YES;
	installedCell.controlSize      = NSControlSizeSmall;
	installedCell.title            = @"";
	installedTableColumn.dataCell = installedCell;

	NSButtonCell* linkCell = [[NSButtonCell alloc] init];
	linkCell.buttonType  = NSButtonTypeMomentaryChange;
	linkCell.bezelStyle  = NSBezelStyleInline;
	linkCell.bordered    = NO;
	linkCell.controlSize = NSControlSizeSmall;
	linkCell.title       = @"";
	linkCell.action      = @selector(didClickBundleLink:);
	linkCell.target      = self;
	linkTableColumn.dataCell = linkCell;

	NSDateFormatter* updatedFormatter = [[NSDateFormatter alloc] init];
	updatedFormatter.dateStyle = NSDateFormatterMediumStyle;

	NSTextFieldCell* updatedCell = [[NSTextFieldCell alloc] initTextCell:@""];
	updatedCell.alignment = NSTextAlignmentRight;
	updatedCell.formatter = updatedFormatter;
	updatedTableColumn.dataCell = updatedCell;

	_bundlesTableView = [[NSTableView alloc] initWithFrame:NSZeroRect];
	_bundlesTableView.allowsColumnReordering  = NO;
	_bundlesTableView.columnAutoresizingStyle = NSTableViewLastColumnOnlyAutoresizingStyle;
	_bundlesTableView.delegate                = self;

	for(NSTableColumn* tableColumn in @[ installedTableColumn, bundleTableColumn, linkTableColumn, updatedTableColumn, descriptionTableColumn ])
		[_bundlesTableView addTableColumn:tableColumn];
	[_bundlesTableView setIndicatorImage:[NSImage imageNamed:@"NSAscendingSortIndicator"] inTableColumn:bundleTableColumn];

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.autohidesScrollers    = YES;
	scrollView.borderType            = NSBezelBorder;
	scrollView.documentView          = _bundlesTableView;

	NSButton* updateBundlesCheckbox = [NSButton checkboxWithTitle:@"Check for and install updates automatically" target:nil action:nil];

	NSView* view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 622, 454)];

	NSDictionary* views = @{
		@"scopeBar":      _scopeBar.view,
		@"search":        _searchField,
		@"scrollView":    scrollView,
		@"updateBundles": updateBundlesCheckbox,
	};

	OakAddAutoLayoutViewsToSuperview(views.allValues, view);

	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-8-[scopeBar]-(>=8)-[search(>=50,<=100,==100@250)]-8-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scrollView(>=50)]-|" options:0 metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[updateBundles]-(>=8)-|" options:0 metrics:nil views:views]];
	[view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-8-[search]-8-[scrollView(>=50)]-[updateBundles]-|" options:0 metrics:nil views:views]];

	// ============
	// = Bindings =
	// ============

	[_arrayController bind:NSContentBinding toObject:BundlesManager.sharedInstance withKeyPath:@"bundles" options:nil];
	[_scopeBar bind:NSValueBinding toObject:self withKeyPath:@"selectedIndex" options:nil];

	[_bundlesTableView bind:NSContentBinding          toObject:_arrayController withKeyPath:@"arrangedObjects" options:nil];
	[_bundlesTableView bind:NSSelectionIndexesBinding toObject:_arrayController withKeyPath:@"selectionIndexes" options:nil];

	[installedTableColumn   bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.installedCellState" options:nil];
	[bundleTableColumn      bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.name" options:nil];
	[updatedTableColumn     bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.downloadLastUpdated" options:nil];
	[descriptionTableColumn bind:NSValueBinding toObject:_arrayController withKeyPath:@"arrangedObjects.textSummary" options:nil];

	[updateBundlesCheckbox bind:NSValueBinding toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:@"values.disableBundleUpdates" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];

	self.view = view;
}

- (void)setSelectedIndex:(NSUInteger)newSelectedIndex
{
	_selectedIndex = newSelectedIndex;
	[_enabledCategories removeAllObjects];
	if(_selectedIndex < _scopeBar.labels.count)
		[_enabledCategories addObject:_scopeBar.labels[_selectedIndex]];
	[self filterStringDidChange:self];
}

- (void)filterStringDidChange:(id)sender
{
	NSMutableArray* predicates = [NSMutableArray array];
	if(OakNotEmptyString(_searchField.stringValue))
		[predicates addObject:[NSPredicate predicateWithFormat:@"name CONTAINS[cd] %@", _searchField.stringValue]];
	if(_enabledCategories.count)
		[predicates addObject:[NSPredicate predicateWithFormat:@"category IN %@", _enabledCategories]];
	_arrayController.filterPredicate = [NSCompoundPredicate andPredicateWithSubpredicates:predicates];
	[_arrayController rearrangeObjects];
}

// ========================
// = NSTableView Delegate =
// ========================

- (void)tableView:(NSTableView*)aTableView didClickTableColumn:(NSTableColumn*)aTableColumn
{
	NSDictionary* map = @{
		kTableColumnIdentifierInstalled:   @"installed",
		kTableColumnIdentifierBundleName:  @"name",
		kTableColumnIdentifierUpdated:     @"downloadLastUpdated",
		kTableColumnIdentifierDescription: @"textSummary"
	};

	NSString* key = map[aTableColumn.identifier];
	if(!key)
		return;

	NSMutableArray* descriptors = [_arrayController.sortDescriptors mutableCopy];

	NSInteger i = 0;
	while(i < descriptors.count && ![_arrayController.sortDescriptors[i].key isEqualToString:key])
		++i;

	if(i == descriptors.count)
		return;

	NSSortDescriptor* descriptor = descriptors[i];
	descriptor = i == 0 || !descriptor.ascending ? [descriptor reversedSortDescriptor] : descriptor;
	[descriptors removeObjectAtIndex:i];
	[descriptors insertObject:descriptor atIndex:0];

	_arrayController.sortDescriptors = descriptors;

	for(NSTableColumn* tableColumn in [_bundlesTableView tableColumns])
		[aTableView setIndicatorImage:nil inTableColumn:tableColumn];
	[aTableView setIndicatorImage:[NSImage imageNamed:(descriptor.ascending ? @"NSAscendingSortIndicator" : @"NSDescendingSortIndicator")] inTableColumn:aTableColumn];
}

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aTableColumn.identifier isEqualToString:kTableColumnIdentifierWebLink])
	{
		Bundle* bundle = _arrayController.arrangedObjects[rowIndex];
		BOOL enabled = bundle.htmlURL ? YES : NO;
		[aCell setEnabled:enabled];
		[aCell setImage:enabled ? [NSImage imageNamed:@"NSFollowLinkFreestandingTemplate"] : nil];
	}
	else if([aTableColumn.identifier isEqualToString:kTableColumnIdentifierInstalled])
	{
		Bundle* bundle = _arrayController.arrangedObjects[rowIndex];
		[aCell setEnabled:!bundle.isMandatory || !bundle.isInstalled];
	}
}

- (BOOL)tableView:(NSTableView*)aTableView shouldEditTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if([aTableColumn.identifier isEqualToString:kTableColumnIdentifierInstalled])
	{
		Bundle* bundle = _arrayController.arrangedObjects[rowIndex];
		return ![BundlesBeingInstalled containsObject:bundle];
	}
	return NO;
}

- (BOOL)tableView:(NSTableView*)aTableView shouldSelectRow:(NSInteger)rowIndex
{
	NSInteger clickedColumn = aTableView.clickedColumn;
	return clickedColumn != [aTableView columnWithIdentifier:kTableColumnIdentifierInstalled] && clickedColumn != [aTableView columnWithIdentifier:kTableColumnIdentifierWebLink];
}

- (void)didClickBundleLink:(NSTableView*)aTableView
{
	NSInteger rowIndex = aTableView.clickedRow;
	Bundle* bundle = _arrayController.arrangedObjects[rowIndex];
	if(bundle.htmlURL)
		[NSWorkspace.sharedWorkspace openURL:bundle.htmlURL];
}
@end
