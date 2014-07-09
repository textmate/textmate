#import "BundleItemChooser.h"
#import "ui/OakBundleItemCell.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ranker.h>
#import <text/ctype.h>
#import <ns/ns.h>

static std::vector<bundles::item_ptr> relevant_items_in_scope (scope::context_t const& scope, bool hasSelection, int mask = bundles::kItemTypeMenuTypes)
{
	std::map<std::string, bundles::item_ptr, text::less_t> sorted;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope, mask, oak::uuid_t(), false))
		sorted.emplace(full_name_with_selection(item, hasSelection), item);

	std::vector<bundles::item_ptr> res;
	std::transform(sorted.begin(), sorted.end(), back_inserter(res), [](std::pair<std::string, bundles::item_ptr> const& p){ return p.second; });
	return res;
}

@interface BundleItemChooserItem : NSObject
@property (nonatomic) id name;
@property (nonatomic) NSString* uuid;
@property (nonatomic) bundles::item_ptr item;
@end

@implementation BundleItemChooserItem
- (id)objectForKey:(id)aKey     { return [self valueForKey:aKey]; }
- (BOOL)isEqual:(id)anotherItem { return [anotherItem isKindOfClass:[BundleItemChooserItem class]] && [self.uuid isEqualToString:((BundleItemChooserItem*)anotherItem).uuid]; }
@end

@interface BundleItemChooser () <NSToolbarDelegate>
@property (nonatomic) OakKeyEquivalentView* keyEquivalentView;
@property (nonatomic) NSPopUpButton* actionsPopUpButton;
@property (nonatomic) NSBox* topDivider;
@property (nonatomic) NSBox* bottomDivider;
@property (nonatomic) NSButton* selectButton;
@property (nonatomic) NSButton* editButton;
@property (nonatomic) NSArray* layoutConstraints;

@property (nonatomic) NSString* keyEquivalentString;
@property (nonatomic) BOOL keyEquivalentInput;
@property (nonatomic) BOOL searchAllScopes;
@end

@implementation BundleItemChooser
+ (instancetype)sharedInstance
{
	static id sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		self.window.title = @"Select Bundle Item";
		[self.window setContentBorderThickness:31 forEdge:NSMinYEdge];

		NSCell* cell = [OakBundleItemCell new];
		[[self.tableView tableColumnWithIdentifier:@"name"] setDataCell:cell];

		self.actionsPopUpButton = OakCreateActionPopUpButton(YES /* bordered */);
		NSMenu* actionMenu = self.actionsPopUpButton.menu;
		[actionMenu addItemWithTitle:@"Placeholder" action:NULL keyEquivalent:@""];
		[actionMenu addItemWithTitle:@"Search by Key Equivalent" action:@selector(toggleKeyEquivalentInput:) keyEquivalent:@"1"];
		[actionMenu addItem:[NSMenuItem separatorItem]];
		[actionMenu addItemWithTitle:@"Search All Scopes" action:@selector(toggleSearchAllScopes:) keyEquivalent:@"2"];

		self.topDivider          = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);
		self.bottomDivider       = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);

		self.selectButton        = OakCreateButton(@"Select");
		self.selectButton.target = self;
		self.selectButton.action = @selector(accept:);

		self.editButton          = OakCreateButton(@"Edit");
		self.editButton.target   = self;
		self.editButton.action   = @selector(editItem:);

		for(NSView* view in @[ self.searchField, self.actionsPopUpButton, self.topDivider, self.scrollView, self.bottomDivider, self.editButton, self.selectButton ])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self.window.contentView addSubview:view];
		}

		[self setupLayoutConstraints];
		self.window.defaultButtonCell = self.selectButton.cell;
	}
	return self;
}

- (void)setupLayoutConstraints
{
	NSDictionary* views = @{
		@"searchField"        : self.keyEquivalentInput ? self.keyEquivalentView : self.searchField,
		@"actions"            : self.actionsPopUpButton,
		@"topDivider"         : self.topDivider,
		@"scrollView"         : self.scrollView,
		@"bottomDivider"      : self.bottomDivider,
		@"edit"               : self.editButton,
		@"select"             : self.selectButton,
	};

	NSMutableArray* constraints = [NSMutableArray array];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-[actions]-(8)-|"        options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"     options:0 metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[edit]-[select]-|"                                options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(8)-[searchField]-(8)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[select]-(5)-|" options:0 metrics:nil views:views]];

	[self.window.contentView addConstraints:constraints];
	self.layoutConstraints = constraints;
}

- (OakKeyEquivalentView*)keyEquivalentView
{
	if(!_keyEquivalentView)
	{
		_keyEquivalentView = [[OakKeyEquivalentView alloc] initWithFrame:NSZeroRect];
		[_keyEquivalentView setTranslatesAutoresizingMaskIntoConstraints:NO];
		[_keyEquivalentView bind:NSValueBinding toObject:self withKeyPath:@"keyEquivalentString" options:nil];
	}
	return _keyEquivalentView;
}

- (void)toggleKeyEquivalentInput:(id)sender
{
	self.keyEquivalentInput = !self.keyEquivalentInput;
}

- (void)setKeyEquivalentInput:(BOOL)flag
{
	if(_keyEquivalentInput == flag)
		return;

	_keyEquivalentInput = flag;

	NSView* contentView = self.window.contentView;
	[contentView removeConstraints:self.layoutConstraints];
	self.layoutConstraints = nil;

	if(flag)
	{
		[self.searchField removeFromSuperview];
		[contentView addSubview:self.keyEquivalentView];
	}
	else
	{
		[self.keyEquivalentView removeFromSuperview];
		[contentView addSubview:self.searchField];
	}

	[self setupLayoutConstraints];
	[self.window recalculateKeyViewLoop];
	[self.window makeFirstResponder:self.keyEquivalentInput ? self.keyEquivalentView : self.searchField];

	self.keyEquivalentView.eventString = nil;
	self.keyEquivalentView.recording   = self.keyEquivalentInput;

	[self updateItems:self];
}

- (void)toggleSearchAllScopes:(id)sender
{
	self.searchAllScopes = !self.searchAllScopes;
	[self updateItems:self];
}

- (void)setScope:(scope::context_t)aScope
{
	_scope = aScope;
	[self updateItems:self];
}

- (void)setKeyEquivalentString:(NSString*)aString
{
	if([_keyEquivalentString isEqualToString:aString])
		return;

	_keyEquivalentString = aString;
	[self updateItems:self];
}

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(OakBundleItemCell*)cell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if(![aTableColumn.identifier isEqualToString:@"name"])
		return;

	BundleItemChooserItem* entry = self.items[rowIndex];
	cell.keyEquivalent = [NSString stringWithCxxString:key_equivalent(entry.item)];
	cell.tabTrigger    = [NSString stringWithCxxString:entry.item->value_for_field(bundles::kFieldTabTrigger)];
}

- (void)updateItems:(id)sender
{
	std::string const filter = to_s(self.filterString);

	std::multimap<double, BundleItemChooserItem*> rankedItems;
	for(auto const& item : relevant_items_in_scope(self.searchAllScopes ? scope::wildcard : self.scope, self.hasSelection))
	{
		std::string const fullName = full_name_with_selection(item, self.hasSelection);

		if(self.keyEquivalentInput && OakNotEmptyString(self.keyEquivalentString))
		{
			if(key_equivalent(item) == to_s(self.keyEquivalentString))
			{
				BundleItemChooserItem* entry = [BundleItemChooserItem new];
				entry.name = [NSString stringWithCxxString:fullName];
				entry.uuid = [NSString stringWithCxxString:item->uuid()];
				entry.item = item;
				rankedItems.emplace(rankedItems.size(), entry);
			}
		}
		else if(!self.keyEquivalentInput && OakNotEmptyString(self.filterString))
		{
			std::vector< std::pair<size_t, size_t> > ranges;
			if(double rank = oak::rank(filter, fullName, &ranges))
			{
				BundleItemChooserItem* entry = [BundleItemChooserItem new];
				entry.name = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
				entry.uuid = [NSString stringWithCxxString:item->uuid()];
				entry.item = item;
				rankedItems.emplace(-rank, entry);
			}
		}
		else
		{
			BundleItemChooserItem* entry = [BundleItemChooserItem new];
			entry.name = [NSString stringWithCxxString:fullName];
			entry.uuid = [NSString stringWithCxxString:item->uuid()];
			entry.item = item;
			rankedItems.emplace(rankedItems.size(), entry);
		}
	}

	NSMutableArray* res = [NSMutableArray array];
	for(auto const& pair : rankedItems)
		[res addObject:pair.second];
	self.items = res;

	self.window.title = [NSString stringWithFormat:@"Select Bundle Item (%@)", self.itemCountTextField.stringValue];
}

- (void)updateStatusText:(id)sender
{
	if(self.tableView.selectedRow != -1)
	{
		BundleItemChooserItem* item = self.items[self.tableView.selectedRow];
		self.statusTextField.stringValue = item.uuid;
	}
	else
	{
		self.statusTextField.stringValue = @"";
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(toggleKeyEquivalentInput:))
		aMenuItem.title = self.keyEquivalentInput ? @"Search by Title" : @"Search by Key Equivalent";
	else if(aMenuItem.action == @selector(toggleSearchAllScopes:))
		aMenuItem.state = self.searchAllScopes ? NSOnState : NSOffState;
	return YES;
}

- (IBAction)editItem:(id)sender
{
	[self.window orderOut:self];
	if(self.editAction)
		[NSApp sendAction:self.editAction to:self.target from:self];
	[self.window close];
}
@end
