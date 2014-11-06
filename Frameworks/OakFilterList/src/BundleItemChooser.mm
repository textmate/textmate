#import "BundleItemChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <bundles/bundles.h>
#import <settings/settings.h>
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
	return bundles::query(bundles::kFieldAny, NULL_STR, scope, mask, oak::uuid_t(), false);
}

@interface BundleItemChooserItem : NSObject
+ (instancetype)bundleChooserItemWithItem:(bundles::item_ptr)anItem title:(id)aTitle;
@property (nonatomic) id name;
@property (nonatomic) NSString* uuid;
@property (nonatomic) NSString* path;
@property (nonatomic) bundles::item_ptr item;
@property (nonatomic) NSMenuItem* menuItem;
@end

@implementation BundleItemChooserItem
- (id)objectForKey:(id)aKey     { return [self valueForKey:aKey]; }
- (BOOL)isEqual:(id)anotherItem { return [anotherItem isKindOfClass:[BundleItemChooserItem class]] && [self.uuid isEqualToString:((BundleItemChooserItem*)anotherItem).uuid]; }

- (NSComparisonResult)localizedCompare:(BundleItemChooserItem*)rhs
{
	NSString* lhsString = [_name isKindOfClass:[NSAttributedString class]]    ? [_name string]    : _name;
	NSString* rhsString = [rhs.name isKindOfClass:[NSAttributedString class]] ? [rhs.name string] : rhs.name;
	return !!_menuItem == !!rhs.menuItem ? [lhsString localizedCompare:rhsString] : (_menuItem ? NSOrderedDescending : NSOrderedAscending);
}

+ (instancetype)bundleChooserItemWithItem:(bundles::item_ptr)anItem title:(id)aTitle
{
	BundleItemChooserItem* res = [self new];
	res.item = anItem;
	res.uuid = [NSString stringWithCxxString:anItem->uuid()];
	res.name = aTitle;
	return res;
}
@end

@interface BundleItemTableCellView : NSTableCellView
@property (nonatomic) NSTextField* shortcutTextField;
@end

@implementation BundleItemTableCellView
- (id)init
{
	if((self = [super init]))
	{
		NSImageView* imageView = [NSImageView new];
		NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		NSTextField* shortcutTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);

		[shortcutTextField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[shortcutTextField setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSDictionary* views = @{ @"icon" : imageView, @"text" : textField, @"shortcut" : shortcutTextField };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[icon(==16)]-(4)-[text]-[shortcut]-(8)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:imageView attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:self attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];

		self.imageView         = imageView;
		self.textField         = textField;
		self.shortcutTextField = shortcutTextField;
	}
	return self;
}

- (void)setObjectValue:(BundleItemChooserItem*)bundleItem
{
	std::map<bundles::kind_t, NSString*> const map = {
		{ bundles::kItemTypeCommand,     @"Command"      },
		{ bundles::kItemTypeDragCommand, @"Drag Command" },
		{ bundles::kItemTypeSnippet,     @"Snippet"      },
		{ bundles::kItemTypeSettings,    @"Settings"     },
		{ bundles::kItemTypeGrammar,     @"Grammar"      },
		{ bundles::kItemTypeProxy,       @"Proxy"        },
		{ bundles::kItemTypeTheme,       @"Theme"        },
		{ bundles::kItemTypeMacro,       @"Macro"        },
	};

	NSImage* image = nil;
	if(bundleItem.item)
	{
		auto it = map.find(bundleItem.item->kind());
		if(it != map.end())
			image = [NSImage imageNamed:it->second inSameBundleAsClass:NSClassFromString(@"BundleEditor")];
	}
	else if(bundleItem.menuItem)
	{
		image = [NSImage imageNamed:@"MenuItem" inSameBundleAsClass:NSClassFromString(@"BundleEditor")];
	}
	else if(bundleItem.path)
	{
		image = [OakFileIconImage fileIconImageWithPath:bundleItem.path size:NSMakeSize(16, 16)];
	}

	self.imageView.image       = image;
	self.textField.objectValue = bundleItem.name;

	std::string shortcut = NULL_STR;
	if(bundleItem.item)
		shortcut = key_equivalent(bundleItem.item);
	else if(bundleItem.menuItem)
		shortcut = key_equivalent_for_menu_item(bundleItem.menuItem);

	id str = nil;
	if(shortcut != NULL_STR)
	{
		self.shortcutTextField.font = [NSFont controlContentFontOfSize:0];

		size_t keyStartsAt = 0;
		std::string const glyphString = ns::glyphs_for_event_string(shortcut, &keyStartsAt);
		NSString* modifiers = [NSString stringWithCxxString:glyphString.substr(0, keyStartsAt)];
		NSString* key       = [NSString stringWithCxxString:glyphString.substr(keyStartsAt)];

		NSDictionary* fontAttr = @{ NSFontAttributeName : self.shortcutTextField.font };
		CGFloat curWidth = std::max<CGFloat>(1, [key sizeWithAttributes:fontAttr].width);
		CGFloat maxWidth = std::max<CGFloat>(1, [@"⌫" sizeWithAttributes:fontAttr].width);

		if(curWidth < maxWidth)
		{
			CGFloat width = std::max<CGFloat>(1, [modifiers sizeWithAttributes:fontAttr].width);

			NSMutableAttributedString* aStr = [[NSMutableAttributedString alloc] initWithString:[NSString stringWithFormat:@"%@%@\t", modifiers, key] attributes:fontAttr];
			NSMutableParagraphStyle* pStyle = [NSMutableParagraphStyle new];
			[pStyle setTabStops:@[ [[NSTextTab alloc] initWithType:NSLeftTabStopType location:width + maxWidth] ]];
			[aStr addAttributes:@{ NSParagraphStyleAttributeName : pStyle } range:NSMakeRange(0, [aStr length])];

			str = aStr;
		}
		else
		{
			str = [modifiers stringByAppendingString:key];
		}
	}
	else if(bundleItem.item)
	{
		std::string tabTrigger = bundleItem.item->value_for_field(bundles::kFieldTabTrigger);
		if(tabTrigger != NULL_STR)
		{
			str = [NSString stringWithCxxString:tabTrigger + "⇥"];
			self.shortcutTextField.font = [NSFont controlContentFontOfSize:10];
		}
	}
	self.shortcutTextField.objectValue = str;
}
@end

@interface BundleItemChooser () <NSToolbarDelegate>
@property (nonatomic) OakKeyEquivalentView* keyEquivalentView;
@property (nonatomic) NSPopUpButton* actionsPopUpButton;
@property (nonatomic) NSView* aboveScopeBarDark;
@property (nonatomic) NSView* aboveScopeBarLight;
@property (nonatomic) OakScopeBarView* scopeBar;
@property (nonatomic) NSView* topDivider;
@property (nonatomic) NSView* bottomDivider;
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

		OakAddAutoLayoutViewsToSuperview([self.allViews allValues], self.window.contentView);

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
		self.drawTableViewAsHighlighted = ![isRecording boolValue];
	}
	else
	{
		[super observeValueForKeyPath:keyPath ofObject:object change:change context:context];

		if(_keyEquivalentView && !_keyEquivalentView.recording && [keyPath isEqualToString:@"firstResponder"])
		{
			BOOL oldIsKeyEquivalentView = change[NSKeyValueChangeOldKey] == _keyEquivalentView;
			BOOL newIsKeyEquivalentView = change[NSKeyValueChangeNewKey] == _keyEquivalentView;
			if(oldIsKeyEquivalentView != newIsKeyEquivalentView)
				self.drawTableViewAsHighlighted = newIsKeyEquivalentView;
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

	OakSetupKeyViewLoop(@[ (self.keyEquivalentInput ? self.keyEquivalentView : self.searchField), self.actionsPopUpButton, self.scopeBar.buttons, self.editButton, self.selectButton ]);
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

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	NSString* identifier = aTableColumn.identifier;
	NSTableCellView* res = [aTableView makeViewWithIdentifier:identifier owner:self];
	if(!res)
	{
		res = [BundleItemTableCellView new];
		res.identifier = identifier;
	}

	res.objectValue = self.items[row];
	return res;
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
						double rank = rankedItems.size();

						std::vector< std::pair<size_t, size_t> > ranges;
						std::string const fullName = pair.first + " — " + itemNameSuffix;
						if(!include && _bundleItemField == kBundleItemTitleField)
						{
							double score = oak::rank(filter, fullName, &ranges);
							if(!score)
								continue;
							if(self.searchAllScopes)
								rank = score;
						}

						NSMutableAttributedString* title = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
						if(!self.searchAllScopes)
						{
							if(!previousSettings.insert(pair.first).second)
								[title addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, [[NSString stringWithCxxString:pair.first] length])];
						}

						rankedItems.emplace(rank, [BundleItemChooserItem bundleChooserItemWithItem:item title:title]);
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
							double rank = rankedItems.size();

							std::vector< std::pair<size_t, size_t> > ranges;
							std::string const fullName = pair.first + " — " + itemNameSuffix + " » shellVariables";
							if(!include && _bundleItemField == kBundleItemTitleField)
							{
								double score = oak::rank(filter, fullName, &ranges);
								if(!score)
									continue;
								if(self.searchAllScopes)
									rank = score;
							}

							NSMutableAttributedString* title = CreateAttributedStringWithMarkedUpRanges(fullName, ranges);
							if(eclipsed)
								[title addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, [[NSString stringWithCxxString:pair.first] length])];

							rankedItems.emplace(rank, [BundleItemChooserItem bundleChooserItemWithItem:item title:title]);
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

	if(self.searchSource & kSearchSourceSettingsItems)
	{
		for(auto const& info : settings_info_for_path(to_s(self.path), self.searchAllScopes ? scope::wildcard : self.scope.right, to_s(self.directory)))
		{
			std::string const base = path::name(info.path);
			std::string const title = info.variable + " — " + (base == "Default.tmProperties" || base == "Global.tmProperties" ? base : path::with_tilde(info.path)) + (info.section == NULL_STR ? "" : " » " + info.section);
			std::vector< std::pair<size_t, size_t> > ranges;

			bool include = filter == NULL_STR || filter.empty();
			double rank  = rankedItems.size();

			if(!include)
			{
				if(_bundleItemField != kBundleItemTitleField)
					continue;

				double score = oak::rank(filter, title, &ranges);
				if(!score)
					continue;
				if(self.searchAllScopes)
					rank = score;
			}

			BundleItemChooserItem* item = [BundleItemChooserItem new];
			item.name = CreateAttributedStringWithMarkedUpRanges(title, ranges);
			item.path = [NSString stringWithCxxString:info.path];
			rankedItems.emplace(rankedItems.size(), item);
		}
	}

	NSMutableArray* res = [NSMutableArray array];
	for(auto const& pair : rankedItems)
		[res addObject:pair.second];

	BOOL shouldSort = !(self.searchSource & kSearchSourceSettingsItems) && (filter == NULL_STR || filter.empty());
	self.items = shouldSort ? [res sortedArrayUsingSelector:@selector(localizedCompare:)] : res;

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
	return item.uuid && self.editAction || item.path;
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
