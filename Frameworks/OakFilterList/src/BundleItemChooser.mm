#import "BundleItemChooser.h"
#import "OakAbbreviations.h"
#import "ui/OakBundleItemCell.h"
#import "ui/TableView.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <text/ranker.h>
#import <text/case.h>
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
static NSUInteger const kSearchSourceMenuItems        = (1 << 5);

static void* kRecordingBinding = &kRecordingBinding;

static NSString* OakMenuItemIdentifier (NSMenuItem* menuItem)
{
	if(!menuItem.action)
		return nil;

	NSString* str = NSStringFromSelector(menuItem.action);
	return menuItem.tag ? [str stringByAppendingFormat:@"%ld", menuItem.tag] : str;
}

static std::string key_equivalent_for_menu_item (NSMenuItem* menuItem)
{
	if(OakIsEmptyString([menuItem keyEquivalent]))
		return NULL_STR;

	static struct { NSUInteger flag; std::string symbol; } const EventFlags[] =
	{
		{ NSNumericPadKeyMask, "#" },
		{ NSControlKeyMask,    "^" },
		{ NSAlternateKeyMask,  "~" },
		{ NSShiftKeyMask,      "$" },
		{ NSCommandKeyMask,    "@" },
	};

	std::string key  = to_s([menuItem keyEquivalent]);
	NSUInteger flags = [menuItem keyEquivalentModifierMask];

	if(flags & NSShiftKeyMask)
	{
		std::string const upCased = text::uppercase(key);
		if(key != upCased)
		{
			flags &= ~NSShiftKeyMask;
			key = upCased;
		}
	}

	std::string modifiers = "";
	for(auto const& record : EventFlags)
		modifiers += (flags & record.flag) ? record.symbol : "";
	return modifiers + key;
}

namespace
{
	struct menu_item_t
	{
		std::string title;
		NSMenuItem* menu_item;
	};
}

template <typename _OutputIter>
_OutputIter copy_menu_items (NSMenu* menu, _OutputIter out, NSArray* parentNames = @[ @"Main Menu" ])
{
	for(NSMenuItem* item in [menu itemArray])
	{
		if(id target = [NSApp targetForAction:[item action]])
		{
			if(![target respondsToSelector:@selector(validateMenuItem:)] || [target validateMenuItem:item])
			{
				NSString* title = [item title];
				if([item state] == NSOnState)
				{
					if([[[item onStateImage] name] isEqualToString:@"NSMenuItemBullet"])
							title = [title stringByAppendingString:@" (•)"];
					else	title = [title stringByAppendingString:@" (✓)"];
				}
				title = [title stringByAppendingFormat:@" — %@", [parentNames componentsJoinedByString:@" » "]];
				*out++ = { to_s(title), item };
			}
		}

		if(NSMenu* submenu = [item submenu])
			out = copy_menu_items(submenu, out, [parentNames arrayByAddingObject:[item title]]);
	}
	return out;
}

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
+ (instancetype)bundleChooserItemWithItem:(bundles::item_ptr)anItem title:(id)aTitle;
@property (nonatomic) id name;
@property (nonatomic) NSString* uuid;
@property (nonatomic) bundles::item_ptr item;
@property (nonatomic) NSMenuItem* menuItem;
@end

@implementation BundleItemChooserItem
- (id)objectForKey:(id)aKey     { return [self valueForKey:aKey]; }
- (BOOL)isEqual:(id)anotherItem { return [anotherItem isKindOfClass:[BundleItemChooserItem class]] && [self.uuid isEqualToString:((BundleItemChooserItem*)anotherItem).uuid]; }

+ (instancetype)bundleChooserItemWithItem:(bundles::item_ptr)anItem title:(id)aTitle
{
	BundleItemChooserItem* res = [self new];
	res.item = anItem;
	res.uuid = [NSString stringWithCxxString:anItem->uuid()];
	res.name = aTitle;
	return res;
}
@end

@interface BundleItemChooser () <NSToolbarDelegate>
@property (nonatomic) OakKeyEquivalentView* keyEquivalentView;
@property (nonatomic) NSPopUpButton* actionsPopUpButton;
@property (nonatomic) NSBox* aboveScopeBarDark;
@property (nonatomic) NSBox* aboveScopeBarLight;
@property (nonatomic) OakScopeBarView* scopeBar;
@property (nonatomic) NSBox* topDivider;
@property (nonatomic) NSBox* bottomDivider;
@property (nonatomic) NSButton* selectButton;
@property (nonatomic) NSButton* editButton;
@property (nonatomic) NSArray* layoutConstraints;
@property (nonatomic) NSArray* sourceListLabels;

@property (nonatomic) NSUInteger sourceIndex;
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
		_sourceListLabels = @[ @"Actions", @"Settings", @"Other" ];
		_bundleItemField  = kBundleItemTitleField;
		_searchSource     = kSearchSourceActionItems|kSearchSourceMenuItems;

		self.window.title = @"Select Bundle Item";
		[self.window setContentBorderThickness:31 forEdge:NSMinYEdge];

		NSCell* cell = [OakBundleItemCell new];
		cell.lineBreakMode = NSLineBreakByTruncatingMiddle;
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

		char key = 0;

		[actionMenu addItemWithTitle:@"Search" action:@selector(nop:) keyEquivalent:@""];
		for(auto&& info : fields)
		{
			NSMenuItem* item = [actionMenu addItemWithTitle:info.title action:@selector(takeBundleItemFieldFrom:) keyEquivalent:key < 2 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
			[item setIndentationLevel:1];
			[item setTag:info.tag];
		}

		[actionMenu addItem:[NSMenuItem separatorItem]];
		[actionMenu addItemWithTitle:@"Search All Scopes" action:@selector(toggleSearchAllScopes:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];

		self.aboveScopeBarDark  = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);
		self.aboveScopeBarLight = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1], [NSColor colorWithCalibratedWhite:0.912 alpha:1]);

		self.scopeBar = [OakScopeBarView new];
		self.scopeBar.labels = _sourceListLabels;

		self.topDivider          = OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1]),
		self.bottomDivider       = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);

		self.selectButton        = OakCreateButton(@"Select", NSTexturedRoundedBezelStyle);
		self.selectButton.target = self;
		self.selectButton.action = @selector(accept:);

		self.editButton          = OakCreateButton(@"Edit", NSTexturedRoundedBezelStyle);
		self.editButton.target   = self;
		self.editButton.action   = @selector(editItem:);

		for(NSView* view in [self.allViews allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self.window.contentView addSubview:view];
		}

		[self setupLayoutConstraints];
		self.window.defaultButtonCell = self.selectButton.cell;

		[self.scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];
	}
	return self;
}

- (void)dealloc
{
	[_keyEquivalentView removeObserver:self forKeyPath:@"recording" context:kRecordingBinding];
	[_scopeBar unbind:NSValueBinding];
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

- (NSDictionary*)allViews
{
	return @{
		@"searchField"        : self.keyEquivalentInput ? self.keyEquivalentView : self.searchField,
		@"actions"            : self.actionsPopUpButton,
		@"aboveScopeBarDark"  : self.aboveScopeBarDark,
		@"aboveScopeBarLight" : self.aboveScopeBarLight,
		@"scopeBar"           : self.scopeBar,
		@"topDivider"         : self.topDivider,
		@"scrollView"         : self.scrollView,
		@"bottomDivider"      : self.bottomDivider,
		@"status"             : self.statusTextField,
		@"edit"               : self.editButton,
		@"select"             : self.selectButton,
	};
}

- (void)setupLayoutConstraints
{
	NSDictionary* views = self.allViews;

	NSMutableArray* constraints = [NSMutableArray array];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-[actions]-(8)-|"        options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[aboveScopeBarDark(==aboveScopeBarLight)]|"      options:0 metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"     options:0 metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(24)-[status]-[edit]-[select]-|"                options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	[constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[aboveScopeBarDark][aboveScopeBarLight]-(3)-[scopeBar]-(4)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[select]-(5)-|" options:0 metrics:nil views:views]];

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
	if([self.tableView numberOfRows] > 0)
	{
		[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
		[self.tableView scrollRowToVisible:0];
	}
	[super showWindow:sender];
}

- (void)setSourceIndex:(NSUInteger)newSourceIndex
{
	switch(_sourceIndex = newSourceIndex)
	{
		case 0: self.searchSource = kSearchSourceActionItems|kSearchSourceMenuItems;   break;
		case 1: self.searchSource = kSearchSourceSettingsItems;                        break;
		case 2: self.searchSource = kSearchSourceGrammarItems|kSearchSourceThemeItems; break;
	}
}

- (void)keyDown:(NSEvent*)anEvent
{
	NSUInteger res = OakPerformTableViewActionFromKeyEvent(self.tableView, anEvent);
	if(res == OakMoveAcceptReturn)
		[self performDefaultButtonClick:self];
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

- (void)tableView:(NSTableView*)aTableView willDisplayCell:(OakBundleItemCell*)cell forTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)rowIndex
{
	if(![aTableColumn.identifier isEqualToString:@"name"])
		return;

	BundleItemChooserItem* entry = self.items[rowIndex];
	if(entry.item)
	{
		cell.keyEquivalentString = [NSString stringWithCxxString:key_equivalent(entry.item)];
		cell.tabTriggerString    = [NSString stringWithCxxString:entry.item->value_for_field(bundles::kFieldTabTrigger)];
	}
	else if(entry.menuItem)
	{
		cell.keyEquivalentString = [NSString stringWithCxxString:key_equivalent_for_menu_item(entry.menuItem)];
		cell.tabTriggerString    = nil;
	}
}

- (void)updateItems:(id)sender
{
	std::string const filter = to_s(self.keyEquivalentInput ? self.keyEquivalentString : self.filterString);
	std::set<std::string> previousSettings, previousVariables;

	std::vector<std::string> identifiers;
	for(NSString* identifier in [[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] stringsForAbbreviation:self.filterString])
		identifiers.push_back(to_s(identifier));

	std::multimap<double, BundleItemChooserItem*> rankedItems;
	for(auto const& item : relevant_items_in_scope(self.searchAllScopes ? scope::wildcard : self.scope, self.hasSelection, self.searchSource))
	{
		bool include = filter == NULL_STR || filter.empty();
		if(!include)
		{
			switch(_bundleItemField)
			{
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

			if(!include && _bundleItemField != kBundleItemTitleField)
				continue;
		}

		if(item->kind() != bundles::kItemTypeSettings)
		{
			std::string const fullName = full_name_with_selection(item, self.hasSelection);
			id title    = [NSString stringWithCxxString:fullName];
			double rank = rankedItems.size();

			if(!include && _bundleItemField == kBundleItemTitleField)
			{
				std::vector< std::pair<size_t, size_t> > ranges;
				double score = oak::rank(filter, fullName, &ranges);
				if(!score)
					continue;

				size_t rankIndex = std::find(identifiers.begin(), identifiers.end(), to_s(item->uuid())) - identifiers.begin();
				if(rankIndex != identifiers.size())
					score = identifiers.size() - rankIndex;

				title = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
				rank  = -score;
			}

			rankedItems.emplace(rank, [BundleItemChooserItem bundleChooserItemWithItem:item title:title]);
		}
		else
		{
			std::string itemNameSuffix = full_name_with_selection(item, self.hasSelection);
			std::string::size_type pos = itemNameSuffix.rfind(" — ");
			if(pos != std::string::npos)
				itemNameSuffix = itemNameSuffix.substr(pos + strlen(" — ")) + " » " + itemNameSuffix.substr(0, pos);

			plist::dictionary_t settings;
			if(plist::get_key_path(item->plist(), bundles::kFieldSettingName, settings))
			{
				for(auto const& pair : settings)
				{
					if(pair.first != "shellVariables")
					{
						std::vector< std::pair<size_t, size_t> > ranges;
						std::string const fullName = pair.first + " — " + itemNameSuffix;
						if(!include && _bundleItemField == kBundleItemTitleField)
						{
							if(!oak::rank(filter, fullName, &ranges))
								continue;
						}

						NSMutableAttributedString* title = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
						if(!self.searchAllScopes)
						{
							if(!previousSettings.insert(pair.first).second)
								[title addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, [[NSString stringWithCxxString:pair.first] length])];
						}

						rankedItems.emplace(rankedItems.size(), [BundleItemChooserItem bundleChooserItemWithItem:item title:title]);
					}
					else
					{
						auto const shellVariables = shell_variables(item);

						bool eclipsed = false;
						if(!self.searchAllScopes)
						{
							for(auto const& pair : shellVariables)
								eclipsed = !previousVariables.insert(pair.first).second || eclipsed;
						}

						for(auto const& pair : shellVariables)
						{
							std::vector< std::pair<size_t, size_t> > ranges;
							std::string const fullName = pair.first + " — " + itemNameSuffix + " » shellVariables";
							if(!include && _bundleItemField == kBundleItemTitleField)
							{
								if(!oak::rank(filter, fullName, &ranges))
									continue;
							}

							NSMutableAttributedString* title = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
							if(eclipsed)
								[title addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, [[NSString stringWithCxxString:pair.first] length])];

							rankedItems.emplace(rankedItems.size(), [BundleItemChooserItem bundleChooserItemWithItem:item title:title]);
						}
					}
				}
			}
		}
	}

	if(self.searchSource & kSearchSourceMenuItems)
	{
		std::vector<menu_item_t> menuItems;
		copy_menu_items([NSApp mainMenu], back_inserter(menuItems));
		for(auto const& record : menuItems)
		{
			id title     = nil;
			bool include = filter == NULL_STR || filter.empty();
			double rank  = rankedItems.size();

			if(!include)
			{
				switch(_bundleItemField)
				{
					case kBundleItemTitleField:
					{
						std::vector< std::pair<size_t, size_t> > ranges;
						if(double score = oak::rank(filter, record.title, &ranges))
						{
							size_t rankIndex = std::find(identifiers.begin(), identifiers.end(), to_s(OakMenuItemIdentifier(record.menu_item))) - identifiers.begin();
							if(rankIndex != identifiers.size())
								score = identifiers.size() - rankIndex;

							title   = CreateAttributedStringWithMarkedUpRanges(record.title, ranges);
							include = true;
							rank    = -score;
						}
					}
					break;

					case kBundleItemKeyEquivalentField:
					{
						include = key_equivalent_for_menu_item(record.menu_item) == filter;
					}
					break;
				}
			}

			if(include)
			{
				BundleItemChooserItem* entry = [BundleItemChooserItem new];
				entry.name     = title ?: [NSString stringWithCxxString:record.title];
				entry.menuItem = record.menu_item;
				rankedItems.emplace(rank, entry);
			}
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
	NSString* status = nil;
	if(self.tableView.selectedRow != -1)
	{
		if(BundleItemChooserItem* selected = self.items[self.tableView.selectedRow])
		{
			if(bundles::item_ptr item = selected.item)
			{
				if(_bundleItemField == kBundleItemSemanticClassField)
					status = [NSString stringWithCxxString:item->value_for_field(bundles::kFieldSemanticClass)];
				else if(_bundleItemField == kBundleItemScopeSelectorField)
					status = [NSString stringWithCxxString:to_s(item->scope_selector())];
			}
		}
	}
	self.statusTextField.stringValue = status ?: @"";

	// Our super class will ask for updated status text each time selection changes
	// so we use this to update enabled state for action buttons
	// FIXME Since ‘canEdit’ depends on ‘editAction’ we must update ‘enabled’ when ‘editAction’ changes.
	self.selectButton.enabled     = self.canAccept;
	self.editButton.enabled       = self.canEdit;
	self.window.defaultButtonCell = !self.canAccept && self.canEdit ? self.editButton.cell : self.selectButton.cell;
	self.tableView.doubleAction   = !self.canAccept && self.canEdit ? @selector(editItem:) : @selector(accept:);
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(takeBundleItemFieldFrom:))
		aMenuItem.state = self.bundleItemField == aMenuItem.tag ? NSOnState : NSOffState;
	else if(aMenuItem.action == @selector(toggleSearchAllScopes:))
		aMenuItem.state = self.searchAllScopes ? NSOnState : NSOffState;

	return YES;
}

- (BOOL)canAccept
{
	BundleItemChooserItem* item = self.tableView.selectedRow != -1 ? self.items[self.tableView.selectedRow] : nil;
	return item.menuItem || item.item && item.item->kind() != bundles::kItemTypeSettings;
}

- (BOOL)canEdit
{
	BundleItemChooserItem* item = self.tableView.selectedRow != -1 ? self.items[self.tableView.selectedRow] : nil;
	return item.uuid && self.editAction;
}

- (void)accept:(id)sender
{
	if(!self.keyEquivalentInput && OakNotEmptyString(self.filterString) && (self.tableView.selectedRow > 0 || [self.filterString length] > 1))
	{
		BundleItemChooserItem* item = self.items[self.tableView.selectedRow];
		if(item.uuid)
			[[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] learnAbbreviation:self.filterString forString:item.uuid];
		else if(NSMenuItem* menuItem = item.menuItem)
			[[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] learnAbbreviation:self.filterString forString:OakMenuItemIdentifier(menuItem)];
	}

	if(self.tableView.selectedRow != -1)
	{
		BundleItemChooserItem* item = self.items[self.tableView.selectedRow];
		if(NSMenuItem* menuItem = item.menuItem)
		{
			[self.window orderOut:self];
			if(menuItem.action)
				[NSApp sendAction:menuItem.action to:menuItem.target from:menuItem];
			[self.window close];

			return;
		}
	}

	[super accept:sender];
}

- (IBAction)editItem:(id)sender
{
	if(![self canEdit])
		return NSBeep();

	[self.window orderOut:self];
	[NSApp sendAction:self.editAction to:self.target from:self];
	[self.window close];
}

- (IBAction)selectNextTab:(id)sender     { self.sourceIndex = (self.sourceIndex + 1) % self.sourceListLabels.count; }
- (IBAction)selectPreviousTab:(id)sender { self.sourceIndex = (self.sourceIndex + self.sourceListLabels.count - 1) % self.sourceListLabels.count; }
@end
