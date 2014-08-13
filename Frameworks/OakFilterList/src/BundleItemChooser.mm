#import "BundleItemChooser.h"
#import "OakAbbreviations.h"
#import "ui/OakBundleItemCell.h"
#import "ui/TableView.h"
#import "ui/TableViewAction.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ranker.h>
#import <text/ctype.h>
#import <ns/ns.h>

static NSUInteger const kBundleItemTitleField          = 0;
static NSUInteger const kBundleItemKeyEquivalentField  = 1;
static NSUInteger const kBundleItemTabTriggerField     = 2;
static NSUInteger const kBundleItemSemanticClassField  = 3;
static NSUInteger const kBundleItemScopeSelectorField  = 4;

static NSUInteger const kSearchSourceActionItems      = (1 << 0);
static NSUInteger const kSearchSourceSettingsItems    = (1 << 1);
static NSUInteger const kSearchSourceGrammarItems     = (1 << 2);
static NSUInteger const kSearchSourceThemeItems       = (1 << 3);
static NSUInteger const kSearchSourceDragCommandItems = (1 << 4);

static void* kRecordingBinding = &kRecordingBinding;

static std::vector<bundles::item_ptr> relevant_items_in_scope (scope::context_t const& scope, bool hasSelection, NSUInteger sourceMask)
{
	int mask = 0;
	if(sourceMask & kSearchSourceActionItems)
		mask |= bundles::kItemTypeCommand|bundles::kItemTypeMacro|bundles::kItemTypeSnippet;
	if(sourceMask & kSearchSourceSettingsItems)
		mask |= bundles::kItemTypeSettings;
	if(sourceMask & kSearchSourceGrammarItems)
		mask |= bundles::kItemTypeGrammar;
	if(sourceMask & kSearchSourceThemeItems)
		mask |= bundles::kItemTypeTheme;
	if(sourceMask & kSearchSourceDragCommandItems)
		mask |= bundles::kItemTypeDragCommand;

	auto allItems = bundles::query(bundles::kFieldAny, NULL_STR, scope, mask, oak::uuid_t(), false);
	if(sourceMask == kSearchSourceSettingsItems)
		return allItems;

	std::map<std::string, bundles::item_ptr, text::less_t> sorted;
	for(auto const& item : allItems)
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

@property (nonatomic) NSUInteger searchSource;
@property (nonatomic) NSUInteger bundleItemField;

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
		_bundleItemField = kBundleItemTitleField;
		_searchSource    = kSearchSourceActionItems;

		self.window.title = @"Select Bundle Item";
		[self.window setContentBorderThickness:31 forEdge:NSMinYEdge];

		NSCell* cell = [OakBundleItemCell new];
		[[self.tableView tableColumnWithIdentifier:@"name"] setDataCell:cell];

		self.actionsPopUpButton = OakCreateActionPopUpButton(YES /* bordered */);
		NSMenu* actionMenu = self.actionsPopUpButton.menu;
		[actionMenu addItemWithTitle:@"Placeholder" action:NULL keyEquivalent:@""];

		struct { NSString* title; NSUInteger tag; } const fields[] =
		{
			{ @"Title",          kBundleItemTitleField         },
			{ @"Key Equivalent", kBundleItemKeyEquivalentField },
			{ @"Tab Trigger",    kBundleItemTabTriggerField    },
			{ @"Semantic Class", kBundleItemSemanticClassField },
			{ @"Scope Selector", kBundleItemScopeSelectorField },
		};

		struct { NSString* title; NSUInteger tag; } const sources[] =
		{
			{ @"Actions",           kSearchSourceActionItems   },
			{ @"Settings",          kSearchSourceSettingsItems },
			{ @"Language Grammars", kSearchSourceGrammarItems  },
			{ @"Themes",            kSearchSourceThemeItems    },
		};

		char key = 0;

		[actionMenu addItemWithTitle:@"Search" action:@selector(nop:) keyEquivalent:@""];
		for(auto&& info : fields)
		{
			NSMenuItem* item = [actionMenu addItemWithTitle:info.title action:@selector(takeBundleItemFieldFrom:) keyEquivalent:key < 2 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
			[item setIndentationLevel:1];
			[item setTag:info.tag];
		}

		[actionMenu addItem:[NSMenuItem separatorItem]];
		[actionMenu addItemWithTitle:@"Sources" action:@selector(nop:) keyEquivalent:@""];
		for(auto&& info : sources)
		{
			NSMenuItem* item = [actionMenu addItemWithTitle:info.title action:@selector(takeSearchSourceFrom:) keyEquivalent:@""];
			[item setIndentationLevel:1];
			[item setTag:info.tag];
		}

		[actionMenu addItem:[NSMenuItem separatorItem]];
		[actionMenu addItemWithTitle:@"Search All Scopes" action:@selector(toggleSearchAllScopes:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];

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

- (void)dealloc
{
	[_keyEquivalentView removeObserver:self forKeyPath:@"recording" context:kRecordingBinding];
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kRecordingBinding)
	{
		NSNumber* isRecording = change[NSKeyValueChangeNewKey];
		[(OakInactiveTableView*)self.tableView setDrawAsHighlighted:![isRecording boolValue]];
	}
	else
	{
		[super observeValueForKeyPath:keyPath ofObject:object change:change context:context];

		if(_keyEquivalentView && !_keyEquivalentView.recording && [keyPath isEqualToString:@"firstResponder"])
		{
			BOOL oldIsKeyEquivalentView = change[NSKeyValueChangeOldKey] == _keyEquivalentView;
			BOOL newIsKeyEquivalentView = change[NSKeyValueChangeNewKey] == _keyEquivalentView;
			if(oldIsKeyEquivalentView != newIsKeyEquivalentView)
				[(OakInactiveTableView*)self.tableView setDrawAsHighlighted:newIsKeyEquivalentView];
		}
	}
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

	// The auto-calculated key-view loop is sometimes wrong, my theory is that it’s because of delayed layout
	[self performSelector:@selector(delayedRecalculateKeyViewLoop:) withObject:self afterDelay:0];
}

- (void)delayedRecalculateKeyViewLoop:(id)sender
{
	[self.window recalculateKeyViewLoop];
}

- (void)showWindow:(id)sender
{
	self.bundleItemField = kBundleItemTitleField;
	[super showWindow:sender];
}

- (void)keyDown:(NSEvent*)anEvent
{
	NSUInteger res = OakPerformTableViewActionFromKeyEvent(self.tableView, anEvent);
	if(res == OakMoveAcceptReturn)
		[self accept:self];
	else if(res == OakMoveCancelReturn)
		[self cancel:self];
}

- (OakKeyEquivalentView*)keyEquivalentView
{
	if(!_keyEquivalentView)
	{
		_keyEquivalentView = [[OakKeyEquivalentView alloc] initWithFrame:NSZeroRect];
		[_keyEquivalentView setTranslatesAutoresizingMaskIntoConstraints:NO];
		[_keyEquivalentView bind:NSValueBinding toObject:self withKeyPath:@"keyEquivalentString" options:nil];
		[_keyEquivalentView addObserver:self forKeyPath:@"recording" options:NSKeyValueObservingOptionNew context:kRecordingBinding];

		if(nil != &NSAccessibilitySharedFocusElementsAttribute)
			[_keyEquivalentView accessibilitySetOverrideValue:@[ self.tableView ] forAttribute:NSAccessibilitySharedFocusElementsAttribute];
	}
	return _keyEquivalentView;
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

- (void)setBundleItemField:(NSUInteger)newBundleItemField
{
	if(_bundleItemField == newBundleItemField)
		return;

	_bundleItemField = newBundleItemField;
	self.keyEquivalentInput = _bundleItemField == kBundleItemKeyEquivalentField;
	self.filterString = nil;
	[self updateItems:self];
}

- (void)setSearchSource:(NSUInteger)newSearchSource
{
	if(_searchSource == newSearchSource)
		return;

	_searchSource = newSearchSource;
	[self updateItems:self];
}

- (void)takeBundleItemFieldFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.bundleItemField = [sender tag];
}

- (void)takeSearchSourceFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.searchSource = self.searchSource == [sender tag] ? kSearchSourceActionItems : (self.searchSource ^ [sender tag]);
}

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(OakBundleItemCell*)cell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if(![aTableColumn.identifier isEqualToString:@"name"])
		return;

	BundleItemChooserItem* entry = self.items[rowIndex];
	cell.keyEquivalentString = [NSString stringWithCxxString:key_equivalent(entry.item)];
	cell.tabTriggerString    = [NSString stringWithCxxString:entry.item->value_for_field(bundles::kFieldTabTrigger)];
}

- (void)updateItems:(id)sender
{
	std::string const filter = to_s(self.keyEquivalentInput ? self.keyEquivalentString : self.filterString);

	std::vector<oak::uuid_t> uuids;
	for(NSString* uuid in [[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] stringsForAbbreviation:self.filterString])
		uuids.push_back(to_s(uuid));

	std::multimap<double, BundleItemChooserItem*> rankedItems;
	for(auto const& item : relevant_items_in_scope(self.searchAllScopes ? scope::wildcard : self.scope, self.hasSelection, self.searchSource))
	{
		std::string const fullName = full_name_with_selection(item, self.hasSelection);

		id title     = [NSString stringWithCxxString:fullName];
		bool include = filter == NULL_STR || filter.empty();
		double rank  = rankedItems.size();

		if(!include)
		{
			switch(_bundleItemField)
			{
				case kBundleItemTitleField:
				{
					std::vector< std::pair<size_t, size_t> > ranges;
					if(double score = oak::rank(filter, fullName, &ranges))
					{
						size_t rankIndex = std::find(uuids.begin(), uuids.end(), item->uuid()) - uuids.begin();
						if(rankIndex != uuids.size())
							score = uuids.size() - rankIndex;

						title   = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
						include = true;
						rank    = -score;
					}
				}
				break;

				case kBundleItemKeyEquivalentField:
				{
					include = key_equivalent(item) == filter;
				}
				break;

				case kBundleItemTabTriggerField:
				{
					std::string const tabTrigger = item->value_for_field(bundles::kFieldTabTrigger);
					include = tabTrigger.find(filter) != std::string::npos;
				}
				break;

				case kBundleItemSemanticClassField:
				{
					std::string const semanticClass = item->value_for_field(bundles::kFieldSemanticClass);
					include = semanticClass.find(filter) != std::string::npos;
				}
				break;

				case kBundleItemScopeSelectorField:
				{
					std::string const scopeSelector = to_s(item->scope_selector());
					include = scopeSelector.find(filter) != std::string::npos;
				}
				break;
			}
		}

		if(include)
		{
			BundleItemChooserItem* entry = [BundleItemChooserItem new];
			entry.name = title;
			entry.uuid = [NSString stringWithCxxString:item->uuid()];
			entry.item = item;
			rankedItems.emplace(rank, entry);
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
	if(aMenuItem.action == @selector(takeBundleItemFieldFrom:))
		aMenuItem.state = self.bundleItemField == aMenuItem.tag ? NSOnState : NSOffState;
	else if(aMenuItem.action == @selector(takeSearchSourceFrom:))
		aMenuItem.state = (self.searchSource & aMenuItem.tag) ? NSOnState : NSOffState;
	else if(aMenuItem.action == @selector(toggleSearchAllScopes:))
		aMenuItem.state = self.searchAllScopes ? NSOnState : NSOffState;

	return YES;
}

- (void)accept:(id)sender
{
	if(!self.keyEquivalentInput && OakNotEmptyString(self.filterString) && (self.tableView.selectedRow > 0 || [self.filterString length] > 1))
	{
		BundleItemChooserItem* item = self.items[self.tableView.selectedRow];
		[[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] learnAbbreviation:self.filterString forString:item.uuid];
	}
	[super accept:sender];
}

- (IBAction)editItem:(id)sender
{
	[self.window orderOut:self];
	if(self.editAction)
		[NSApp sendAction:self.editAction to:self.target from:self];
	[self.window close];
}
@end
