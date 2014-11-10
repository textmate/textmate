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
		std::string name;
		std::string path;
		NSMenuItem* menu_item;
	};
}

template <typename _OutputIter>
_OutputIter copy_menu_items (NSMenu* menu, _OutputIter out, NSArray* parentNames = @[ ])
{
	for(NSMenuItem* item in [menu itemArray])
	{
		if([item action] == @selector(performBundleItemWithUUIDStringFrom:) || [item action] == @selector(takeThemeUUIDFrom:))
			continue;

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
				*out++ = { to_s(title), to_s([parentNames componentsJoinedByString:@" ▸ "]), item };
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
		[imageView setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[imageView setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
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

- (NSArray*)unfilteredItems
{
	auto OakSetNonEmptyString = [](NSMutableDictionary* dict, NSString* key, std::string const& str) {
		if(!str.empty() && str != NULL_STR)
			dict[key] = [NSString stringWithCxxString:str];
	};

	NSMutableArray* items = [NSMutableArray new];
	std::set<std::string> previousSettings, previousVariables;

	for(auto const& item : relevant_items_in_scope(self.searchAllScopes ? scope::wildcard : self.scope, self.hasSelection, self.searchSource))
	{
		std::string const name = name_with_selection(item, self.hasSelection);
		std::string const path = menu_path(item);
		NSString* const uuid   = [NSString stringWithCxxString:item->uuid()];

		if(item->kind() != bundles::kItemTypeSettings)
		{
			std::string suffix;
			if(item->kind() == bundles::kItemTypeGrammar)
				suffix = " ▸ Language Grammars";
			else if(item->kind() == bundles::kItemTypeTheme)
				suffix = " ▸ Themes";

			NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
				@"name" : [NSString stringWithCxxString:name],
				@"path" : [NSString stringWithCxxString:path + suffix],
				@"uuid" : uuid
			}];
			OakSetNonEmptyString(dict, @"scopeSelector", to_s(item->scope_selector()));
			OakSetNonEmptyString(dict, @"keyEquivalent", key_equivalent(item));
			OakSetNonEmptyString(dict, @"tabTrigger",    item->value_for_field(bundles::kFieldTabTrigger));
			OakSetNonEmptyString(dict, @"semanticClass", item->value_for_field(bundles::kFieldSemanticClass));
			[items addObject:dict];
		}
		else
		{
			plist::dictionary_t settings;
			if(plist::get_key_path(item->plist(), bundles::kFieldSettingName, settings))
			{
				for(auto const& pair : settings)
				{
					if(pair.first != "shellVariables")
					{
						NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
							@"name"     : [NSString stringWithCxxString:pair.first],
							@"value"    : [NSString stringWithCxxString:to_s(pair.second)],
							@"path"     : [NSString stringWithCxxString:path + " ▸ " + name],
							@"uuid"     : uuid,
							@"eclipsed" : @(!self.searchAllScopes && !previousSettings.insert(pair.first).second)
						}];
						OakSetNonEmptyString(dict, @"scopeSelector", to_s(item->scope_selector()));
						[items addObject:dict];
					}
					else
					{
						auto const shellVariables = shell_variables(item);

						BOOL eclipsed = false;
						if(!self.searchAllScopes)
						{
							for(auto const& pair : shellVariables)
								eclipsed = !previousVariables.insert(pair.first).second || eclipsed;
						}

						for(auto const& pair : shellVariables)
						{
							NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
								@"name"     : [NSString stringWithCxxString:pair.first],
								@"value"    : [NSString stringWithCxxString:pair.second],
								@"path"     : [NSString stringWithCxxString:path + " ▸ " + name + " ▸ " + "shellVariables"],
								@"uuid"     : uuid,
								@"eclipsed" : @(eclipsed)
							}];
							OakSetNonEmptyString(dict, @"scopeSelector", to_s(item->scope_selector()));
							[items addObject:dict];
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
			NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
				@"name"     : [NSString stringWithCxxString:record.name],
				@"path"     : [NSString stringWithCxxString:record.path],
				@"menuItem" : record.menu_item
			}];
			OakSetNonEmptyString(dict, @"keyEquivalent", key_equivalent_for_menu_item(record.menu_item));
			[items addObject:dict];
		}
	}

	if(self.searchSource & kSearchSourceSettingsItems)
	{
		for(auto const& info : settings_info_for_path(to_s(self.path), self.searchAllScopes ? scope::wildcard : self.scope.right, to_s(self.directory)))
		{
			std::string const name = info.variable;
			std::string const path = (path::name(info.path) == "Default.tmProperties" ? "TextMate.app ▸ Default.tmProperties" : path::with_tilde(info.path)) + (info.section == NULL_STR ? "" : " ▸ " + info.section);

			[items addObject:@{
				@"name" : [NSString stringWithCxxString:name],
				@"path" : [NSString stringWithCxxString:path],
				@"file" : [NSString stringWithCxxString:info.path]
			}];
		}
	}

	return items;
}

- (void)updateItems:(id)sender
{
	auto OakContainsString = [](NSString* haystack, NSString* needle) -> BOOL {
		return haystack && needle && [haystack rangeOfString:needle].location != NSNotFound;
	};

	NSArray* identifiers = [[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] stringsForAbbreviation:self.filterString];
	NSString* filter = self.keyEquivalentInput ? self.keyEquivalentString : self.filterString;

	NSArray* items = [self unfilteredItems];
	if(!(self.searchSource & kSearchSourceSettingsItems) && (_bundleItemField != kBundleItemKeyEquivalentField || OakIsEmptyString(filter)))
		items = [items sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)], [NSSortDescriptor sortDescriptorWithKey:@"path" ascending:YES selector:@selector(localizedCompare:)] ]];

	std::multimap<double, BundleItemChooserItem*> rankedItems;
	for(NSDictionary* item in items)
	{
		std::vector<std::pair<size_t, size_t>> cover;
		NSString* title = item[@"uuid"] && (self.searchSource & (kSearchSourceActionItems|kSearchSourceMenuItems)) ? [NSString stringWithCxxString:bundles::lookup(to_s((NSString*)item[@"uuid"]))->full_name()] : [NSString stringWithFormat:@"%@ — %@", item[@"name"], item[@"path"]];

		double rank = (items.count - rankedItems.size()) / (double)items.count;
		if(OakNotEmptyString(filter))
		{
			if(_bundleItemField == kBundleItemTitleField && (self.searchSource & (kSearchSourceActionItems|kSearchSourceMenuItems)))
				rank = oak::rank(to_s(filter), to_s(title), &cover);
			else if(_bundleItemField == kBundleItemTitleField)
			{
				NSRange r = [title rangeOfString:filter options:NSCaseInsensitiveSearch];
				if(r.location != NSNotFound)
						cover.emplace_back(to_s([title substringToIndex:r.location]).size(), to_s([title substringToIndex:NSMaxRange(r)]).size());
				else	rank = 0;
			}
			else if(_bundleItemField == kBundleItemKeyEquivalentField)
				rank = [item[@"keyEquivalent"] isEqualToString:filter] ? rank : 0;
			else if(_bundleItemField == kBundleItemTabTriggerField)
				rank = OakContainsString(item[@"tabTrigger"], filter) ? rank : 0;
			else if(_bundleItemField == kBundleItemSemanticClassField)
				rank = OakContainsString(item[@"semanticClass"], filter) ? rank : 0;
			else if(_bundleItemField == kBundleItemScopeSelectorField)
				rank = OakContainsString(item[@"scopeSelector"], filter) ? rank : 0;
		}

		if(rank > 0)
		{
			if(_bundleItemField == kBundleItemTitleField)
			{
				NSUInteger i = [identifiers indexOfObject:(item[@"uuid"] ?: OakMenuItemIdentifier(item[@"menuItem"]))];
				if(i != NSNotFound)
					rank = 1 + (identifiers.count - i) / identifiers.count;
			}

			BundleItemChooserItem* entry = [BundleItemChooserItem new];
			entry.name = CreateAttributedStringWithMarkedUpRanges(to_s(title), cover);

			if([item[@"eclipsed"] boolValue])
			{
				NSMutableAttributedString* str = [entry.name mutableCopy];
				[str addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, [item[@"name"] length])];
				entry.name = str;
			}

			entry.uuid = item[@"uuid"];
			if(NSString* uuid = item[@"uuid"])
				entry.item = bundles::lookup(to_s(uuid));
			else if(item[@"menuItem"])
				entry.menuItem = item[@"menuItem"];
			else if(item[@"file"])
				entry.path = item[@"file"];

			rankedItems.emplace(2 - rank, entry);
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
	return item.uuid && self.editAction || item.path;
}

- (void)accept:(id)sender
{
	if(_bundleItemField == kBundleItemTitleField && OakNotEmptyString(self.filterString) && (self.tableView.selectedRow > 0 || [self.filterString length] > 1))
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
