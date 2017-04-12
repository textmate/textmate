#import "BundleItemChooser.h"
#import "OakAbbreviations.h"
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakKeyEquivalentView.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/NSColor Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <bundles/bundles.h>
#import <settings/settings.h>
#import <text/ranker.h>
#import <text/case.h>
#import <text/ctype.h>
#import <regexp/format_string.h>
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
static NSUInteger const kSearchSourceKeyBindingItems  = (1 << 6);

static NSString* OakMenuItemIdentifier (NSMenuItem* menuItem)
{
	if(!menuItem.action)
		return nil;

	NSString* str = NSStringFromSelector(menuItem.action);
	return menuItem.tag ? [str stringByAppendingFormat:@"%ld", menuItem.tag] : str;
}

// ==============
// = ActionItem =
// ==============

@interface ActionItem : NSObject
@property (nonatomic, getter = isMatched) BOOL matched;
@property (nonatomic, readonly) double rank;

@property (nonatomic) NSImage* icon;
@property (nonatomic) NSAttributedString* name;
@property (nonatomic) NSAttributedString* path;
@property (nonatomic) NSString* keyEquivalent;
@property (nonatomic) NSString* tabTrigger;

@property (nonatomic) NSString* itemName;
@property (nonatomic) NSString* location;

@property (nonatomic) NSString* uuid; // Bundle item
@property (nonatomic) NSString* file; // Settings or key binding item
@property (nonatomic) NSString* line; // Line in settings file

@property (nonatomic) NSMenuItem* menuItem;
@property (nonatomic) SEL action;
@property (nonatomic) NSString* value; // Settings items and settings files
@property (nonatomic) NSString* scopeSelector;
@property (nonatomic) NSString* semanticClass;
@property (nonatomic) BOOL eclipsed; // Settings or key binding item
@end

@implementation ActionItem
- (void)reset
{
	_matched = NO;
	_rank    = 0;
	_name    = nil;
	_path    = nil;
}

- (void)updateRankUsingFilter:(std::string const&)filter bundleItemField:(NSUInteger)bundleItemField searchSource:(NSUInteger)searchSource bindings:(NSArray<NSString*>*)identifiers defaultRank:(double)rank
{
	[self reset];

	std::vector<std::pair<size_t, size_t>> cover_path, cover_name;
	std::string name = to_s(_itemName);
	std::string path = to_s(_location);

	if(filter != NULL_STR && !filter.empty())
	{
		auto OakContainsString = [](NSString* haystack, NSString* needle) -> BOOL {
			return haystack && needle && [haystack rangeOfString:needle].location != NSNotFound;
		};

		if(bundleItemField == kBundleItemTitleField)
		{
			std::vector<std::pair<size_t, size_t>> cover;
			if(searchSource & (kSearchSourceActionItems|kSearchSourceMenuItems|kSearchSourceKeyBindingItems))
			{
				if(rank = oak::rank(filter, name, &cover))
						rank += 1;
				else	rank = oak::rank(filter, path + " " + name, &cover);
			}
			else
			{
				auto is_substr = [&cover](std::string const& needle, std::string const& haystack) -> BOOL {
					NSString* str = to_ns(haystack);
					NSRange r = [str rangeOfString:to_ns(needle) options:NSCaseInsensitiveSearch];
					if(r.location != NSNotFound)
						cover.emplace_back(to_s([str substringToIndex:r.location]).size(), to_s([str substringToIndex:NSMaxRange(r)]).size());
					return r.location != NSNotFound;
				};

				if(is_substr(filter, name))
					rank += 1;
				else if(!is_substr(filter, path + " " + name))
					rank = 0;
			}

			for(auto pair : cover)
			{
				if(rank > 1)
					cover_name.push_back(pair);
				else if(pair.first < path.size())
					cover_path.emplace_back(pair.first, std::min(pair.second, path.size()));
				else if(path.size() + 1 < pair.second)
					cover_name.emplace_back(std::max(pair.first, path.size() + 1) - path.size() - 1, pair.second - path.size() - 1);
			}
		}
		else if(bundleItemField == kBundleItemKeyEquivalentField)
			rank = [_keyEquivalent isEqualToString:to_ns(filter)] ? rank : 0;
		else if(bundleItemField == kBundleItemTabTriggerField)
			rank = OakContainsString(_tabTrigger, to_ns(filter)) ? rank : 0;
		else if(bundleItemField == kBundleItemSemanticClassField)
			rank = OakContainsString(_semanticClass, to_ns(filter)) ? rank : 0;
		else if(bundleItemField == kBundleItemScopeSelectorField)
			rank = OakContainsString(_scopeSelector, to_ns(filter)) ? rank : 0;
	}

	if(_matched = (rank > 0 ? YES : NO))
	{
		if(bundleItemField == kBundleItemTitleField)
		{
			NSUInteger i = [identifiers indexOfObject:(_uuid ?: NSStringFromSelector(_action) ?: OakMenuItemIdentifier(_menuItem))];
			if(i != NSNotFound)
				rank = 2 + (identifiers.count - i) / identifiers.count;
		}

		if(NSString* value = _value)
			name += " = " + to_s(value);

		NSMutableAttributedString* str = CreateAttributedStringWithMarkedUpRanges(name, cover_name, NSLineBreakByTruncatingTail);
		if(_eclipsed)
			[str addAttribute:NSStrikethroughStyleAttributeName value:@(NSUnderlineStyleSingle|NSUnderlinePatternSolid) range:NSMakeRange(0, str.string.length)];

		self.name = str;
		self.path = CreateAttributedStringWithMarkedUpRanges(path, cover_path, NSLineBreakByTruncatingHead);
		_rank = 3 - rank;
	}
}
@end

// ==============

static void* kRecordingBinding = &kRecordingBinding;

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
		{
			if(submenu.delegate && [submenu.delegate respondsToSelector:@selector(menuNeedsUpdate:)])
			{
				if([@[ @"Spelling" ] containsObject:submenu.title])
					[submenu.delegate menuNeedsUpdate:submenu];
			}
			out = copy_menu_items(submenu, out, [parentNames arrayByAddingObject:[item title]]);
		}
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

@interface BundleItemTableCellView : NSTableCellView
@property (nonatomic) NSTextField* contextTextField;
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

		NSTextField* textField = OakCreateLabel(@"", [NSFont systemFontOfSize:13]);
		NSTextField* contextTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:10]);

		NSTextField* shortcutTextField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:13]);
		[shortcutTextField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[shortcutTextField setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		NSDictionary* views = @{ @"icon" : imageView, @"name" : textField, @"context" : contextTextField, @"shortcut" : shortcutTextField };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(4)-[icon]-(4)-[name]-(4)-[shortcut]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[context]-(8)-|" options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[name]-(2)-[context]-(5)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:self attribute:NSLayoutAttributeCenterY relatedBy:NSLayoutRelationEqual toItem:imageView attribute:NSLayoutAttributeCenterY multiplier:1 constant:0]];
		[self addConstraint:[NSLayoutConstraint constraintWithItem:textField attribute:NSLayoutAttributeBaseline relatedBy:NSLayoutRelationEqual toItem:shortcutTextField attribute:NSLayoutAttributeBaseline multiplier:1 constant:0]];

		self.imageView         = imageView;
		self.textField         = textField;
		self.contextTextField  = contextTextField;
		self.shortcutTextField = shortcutTextField;
	}
	return self;
}

- (void)setObjectValue:(ActionItem*)item
{
	[super setObjectValue:item];

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
	if(NSString* uuid = item.uuid)
	{
		if(bundles::item_ptr bundleItem = bundles::lookup(to_s(uuid)))
		{
			auto it = map.find(bundleItem->kind());
			if(it != map.end())
				image = [NSImage imageNamed:it->second inSameBundleAsClass:NSClassFromString(@"BundleEditor")];
		}
	}
	else if(item.menuItem)
	{
		image = [NSImage imageNamed:@"MenuItem" inSameBundleAsClass:NSClassFromString(@"BundleEditor")];
	}
	else if(NSString* path = item.file)
	{
		if(path::is_child(to_s(path), oak::application_t::path()))
				image = [NSImage imageNamed:NSImageNameApplicationIcon];
		else	image = [OakFileIconImage fileIconImageWithPath:path size:NSMakeSize(32, 32)];
	}
	else
	{
		image = [NSImage imageNamed:@"Variables" inSameBundleAsClass:NSClassFromString(@"VariablesPreferences")];
	}

	image = [image copy];
	[image setSize:NSMakeSize(32, 32)];

	self.imageView.image              = image;
	self.textField.objectValue        = item.name;
	self.contextTextField.objectValue = item.path;

	id str = @"";
	if(NSString* keyEquivalent = item.keyEquivalent)
	{
		self.shortcutTextField.font = [NSFont controlContentFontOfSize:0];
		str = OakAttributedStringForEventString(keyEquivalent, self.shortcutTextField.font);
	}
	else if(NSString* tabTrigger = item.tabTrigger)
	{
		self.shortcutTextField.font = [NSFont controlContentFontOfSize:10];
		str = [tabTrigger stringByAppendingString:@"⇥"];
	}
	self.shortcutTextField.objectValue = str;
}

// FIXME Copy/paste from OakChooser.mm (OakFileTableCellView)
- (NSAttributedString*)selectedStringForString:(id)aString
{
	NSMutableAttributedString* str = [aString isKindOfClass:[NSString class]] ? [[NSMutableAttributedString alloc] initWithString:aString attributes:nil] : [aString mutableCopy];
	[str enumerateAttributesInRange:NSMakeRange(0, str.length) options:NSAttributedStringEnumerationLongestEffectiveRangeNotRequired usingBlock:^(NSDictionary* attrs, NSRange range, BOOL *stop){
		if(attrs[NSBackgroundColorAttributeName] != nil)
			[str addAttribute:NSBackgroundColorAttributeName value:[NSColor tmMatchedTextSelectedBackgroundColor] range:range];
		if(attrs[NSUnderlineColorAttributeName] != nil)
			[str addAttribute:NSUnderlineColorAttributeName value:[NSColor tmMatchedTextSelectedUnderlineColor] range:range];
	}];
	return str;
}

// FIXME Copy/paste from OakChooser.mm (OakFileTableCellView)
- (void)setBackgroundStyle:(NSBackgroundStyle)backgroundStyle
{
	[super setBackgroundStyle:backgroundStyle];
	if(backgroundStyle == NSBackgroundStyleDark)
	{
		self.textField.objectValue        = [self selectedStringForString:[self valueForKeyPath:@"objectValue.name"]];
		self.contextTextField.textColor   = [NSColor colorWithCalibratedWhite:0.9 alpha:1];
		self.contextTextField.objectValue = [self selectedStringForString:[self valueForKeyPath:@"objectValue.path"]];
	}
	else
	{
		self.textField.objectValue        = [self valueForKeyPath:@"objectValue.name"];
		self.contextTextField.textColor   = [NSColor colorWithCalibratedWhite:0.5 alpha:1];
		self.contextTextField.objectValue = [self valueForKeyPath:@"objectValue.path"];
	}
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityChildrenAttribute])
			return @[ self.textField.cell, self.imageView.cell, self.contextTextField.cell, self.shortcutTextField.cell ];
	else	return [super accessibilityAttributeValue:attribute];
}
@end

@interface BundleItemChooser () <NSToolbarDelegate>
{
	NSArray<ActionItem*>* _unfilteredItems;
}
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
@property (nonatomic) id eventMonitor;
@end

@implementation BundleItemChooser
+ (instancetype)sharedInstance
{
	static BundleItemChooser* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		self.tableView.rowHeight = 38;

		_sourceListLabels = @[ @"Actions", @"Settings", @"Other" ];
		_bundleItemField  = kBundleItemTitleField;
		_searchSource     = kSearchSourceActionItems|kSearchSourceMenuItems|kSearchSourceKeyBindingItems;

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
		[actionMenu addItemWithTitle:@"Search All Scopes" action:@selector(toggleSearchAllScopes:) keyEquivalent:key < 9 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];

		self.aboveScopeBarDark  = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);
		self.aboveScopeBarLight = OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1], [NSColor colorWithCalibratedWhite:0.912 alpha:1]);

		self.scopeBar = [OakScopeBarView new];
		self.scopeBar.labels = _sourceListLabels;

		self.topDivider          = OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1]),
		self.bottomDivider       = OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]);

		self.selectButton                  = OakCreateButton(@"Select");
		self.selectButton.font             = [NSFont messageFontOfSize:[NSFont smallSystemFontSize]];
		self.selectButton.cell.controlSize = NSSmallControlSize;
		self.selectButton.target           = self;
		self.selectButton.action           = @selector(accept:);

		self.editButton                  = OakCreateButton(@"Edit");
		self.editButton.font             = [NSFont messageFontOfSize:[NSFont smallSystemFontSize]];
		self.editButton.cell.controlSize = NSSmallControlSize;
		self.editButton.target           = self;
		self.editButton.action           = @selector(editItem:);

		OakAddAutoLayoutViewsToSuperview([self.allViews allValues], self.window.contentView);
		[self setupLayoutConstraints];

		[self.scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeKeyStatus:) name:NSWindowDidBecomeKeyNotification object:self.window];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidChangeKeyStatus:) name:NSWindowDidResignKeyNotification object:self.window];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[_keyEquivalentView removeObserver:self forKeyPath:@"recording" context:kRecordingBinding];
	[_scopeBar unbind:NSValueBinding];
}

- (void)windowDidChangeKeyStatus:(NSNotification*)aNotification
{
	auto updateDefaultButton = ^NSEvent*(NSEvent* event){
		BOOL isKeyWindow = NSApp.keyWindow == self.window;
		BOOL optionDown  = ([event modifierFlags] & NSDeviceIndependentModifierFlagsMask) == NSAlternateKeyMask;
		self.window.defaultButtonCell = self.canEdit && (!self.canAccept || (optionDown && isKeyWindow)) ? self.editButton.cell : self.selectButton.cell;
		return event;
	};

	updateDefaultButton([NSApp currentEvent]);
	if(NSApp.keyWindow == self.window)
	{
		_eventMonitor = [NSEvent addLocalMonitorForEventsMatchingMask:NSFlagsChangedMask handler:updateDefaultButton];
	}
	else if(_eventMonitor)
	{
		[NSEvent removeMonitor:_eventMonitor];
		_eventMonitor = nil;
	}
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

	NSView* contentView = self.window.contentView;
	[constraints addObject:[NSLayoutConstraint constraintWithItem:_aboveScopeBarLight attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
	[constraints addObject:[NSLayoutConstraint constraintWithItem:_topDivider         attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];
	[constraints addObject:[NSLayoutConstraint constraintWithItem:_bottomDivider      attribute:NSLayoutAttributeLeft relatedBy:NSLayoutRelationEqual toItem:contentView attribute:NSLayoutAttributeLeft multiplier:1.0 constant:0.0]];

	[contentView addConstraints:constraints];
	self.layoutConstraints = constraints;

	OakSetupKeyViewLoop(@[ (self.keyEquivalentInput ? self.keyEquivalentView : self.searchField), self.actionsPopUpButton, self.scopeBar, self.editButton, self.selectButton ]);
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

- (void)windowWillClose:(NSNotification*)aNotification
{
	_unfilteredItems = nil;
	self.items = @[ ];
}

- (void)setSourceIndex:(NSUInteger)newSourceIndex
{
	switch(_sourceIndex = newSourceIndex)
	{
		case 0: self.searchSource = kSearchSourceActionItems|kSearchSourceMenuItems|kSearchSourceKeyBindingItems; break;
		case 1: self.searchSource = kSearchSourceSettingsItems;                                                   break;
		case 2: self.searchSource = kSearchSourceGrammarItems|kSearchSourceThemeItems;                            break;
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

		if(nil != &NSAccessibilitySharedFocusElementsAttribute) // MAC_OS_X_VERSION_10_10
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
	_unfilteredItems = nil;
	[self updateItems:self];
}

- (void)setScope:(scope::context_t)aScope
{
	_scope = aScope;
	_unfilteredItems = nil;
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
	_unfilteredItems = nil;
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

- (NSArray<ActionItem*>*)unfilteredItems
{
	if(_unfilteredItems == nil)
	{
		auto format = [](plist::any_t const& plist) -> std::string {
			return format_string::replace(to_s(plist, plist::kPreferSingleQuotedStrings|plist::kSingleLine), "\\A\\s+|\\s+\\z|(\\s+)", "${1:+ }");
		};

		NSMutableArray<ActionItem*>* items = [NSMutableArray new];
		std::set<std::string> previousSettings, previousVariables;

		for(auto const& bundleItem : relevant_items_in_scope(self.searchAllScopes ? scope::wildcard : self.scope, self.hasSelection, self.searchSource))
		{
			std::string const name = name_with_selection(bundleItem, self.hasSelection);
			std::string const path = menu_path(bundleItem);
			NSString* const uuid   = [NSString stringWithCxxString:bundleItem->uuid()];

			if(bundleItem->kind() != bundles::kItemTypeSettings)
			{
				std::string suffix;
				if(bundleItem->kind() == bundles::kItemTypeGrammar)
					suffix = " ▸ Language Grammars";
				else if(bundleItem->kind() == bundles::kItemTypeTheme)
					suffix = " ▸ Themes";

				ActionItem* item = [[ActionItem alloc] init];
				item.itemName      = to_ns(name);
				item.location      = to_ns(path + suffix);
				item.uuid          = uuid;
				item.scopeSelector = to_ns(to_s(bundleItem->scope_selector()));
				item.keyEquivalent = to_ns(key_equivalent(bundleItem));
				item.tabTrigger    = to_ns(bundleItem->value_for_field(bundles::kFieldTabTrigger));
				item.semanticClass = to_ns(text::join(bundleItem->values_for_field(bundles::kFieldSemanticClass), ", "));
				[items addObject:item];
			}
			else
			{
				plist::dictionary_t settings;
				if(plist::get_key_path(bundleItem->plist(), bundles::kFieldSettingName, settings))
				{
					for(auto const& pair : settings)
					{
						if(pair.first != "shellVariables")
						{
							ActionItem* item = [[ActionItem alloc] init];
							item.itemName      = to_ns(pair.first);
							item.value         = to_ns(format(pair.second));
							item.location      = to_ns(path + " ▸ " + name);
							item.uuid          = uuid;
							item.eclipsed      = !self.searchAllScopes && !previousSettings.insert(pair.first).second ? YES : NO;
							item.scopeSelector = to_ns(to_s(bundleItem->scope_selector()));
							[items addObject:item];
						}
						else
						{
							auto const shellVariables = shell_variables(bundleItem);

							BOOL eclipsed = NO;
							if(!self.searchAllScopes)
							{
								for(auto const& pair : shellVariables)
									eclipsed = !previousVariables.insert(pair.first).second || eclipsed;
							}

							for(auto const& pair : shellVariables)
							{
								ActionItem* item = [[ActionItem alloc] init];
								item.itemName      = to_ns(pair.first);
								item.value         = to_ns(format(pair.second));
								item.location      = to_ns(path + " ▸ " + name + " ▸ " + "shellVariables");
								item.uuid          = uuid;
								item.eclipsed      = eclipsed;
								item.scopeSelector = to_ns(to_s(bundleItem->scope_selector()));
								[items addObject:item];
							}
						}
					}
				}
			}
		}

		std::set<std::pair<std::string, std::string>> seen;
		if(self.searchSource & kSearchSourceMenuItems)
		{
			std::vector<menu_item_t> menuItems;
			copy_menu_items([NSApp mainMenu], back_inserter(menuItems));
			for(auto const& record : menuItems)
			{
				ActionItem* item = [[ActionItem alloc] init];
				item.itemName = to_ns(record.name);
				item.location = to_ns(record.path);
				item.menuItem = record.menu_item;

				std::string const keyEquivalent = key_equivalent_for_menu_item(record.menu_item);
				if(!keyEquivalent.empty())
					seen.emplace(keyEquivalent, sel_getName(record.menu_item.action));

				item.keyEquivalent = to_ns(keyEquivalent);
				[items addObject:item];
			}
		}

		if(self.searchSource & kSearchSourceKeyBindingItems)
		{
			static std::string const KeyBindingLocations[] =
			{
				oak::application_t::support("KeyBindings.dict"),
				oak::application_t::path("Contents/Resources/KeyBindings.dict"),
				path::join(path::home(), "Library/KeyBindings/DefaultKeyBinding.dict"),
				"/Library/KeyBindings/DefaultKeyBinding.dict",
				"/System/Library/Frameworks/AppKit.framework/Resources/StandardKeyBinding.dict",
			};

			std::set<std::string> keysSeen;
			for(auto const& path : KeyBindingLocations)
			{
				std::string displayPath = path::is_child(path, oak::application_t::path()) ? "TextMate.app ▸ " + path::name(path) : path::with_tilde(path);
				for(auto const& pair : plist::load(path))
				{
					std::string key = ns::normalize_event_string(pair.first);
					std::string name, action;
					if(std::string const* sel = boost::get<std::string>(&pair.second))
					{
						if(*sel == "noop:" || !seen.emplace(key, *sel).second)
							continue;

						action = *sel;
						name = format_string::replace(*sel, "[a-z](?=[A-Z])", "$0 ");
						name = format_string::replace(name, "(.+):\\z", "${1:/capitalize}");
						name = format_string::replace(name, "\\bsub Word\\b", "Sub-word");

						if(![NSApp targetForAction:NSSelectorFromString([NSString stringWithCxxString:*sel])])
							name += " (unknown action)";
					}
					else
					{
						name = format(pair.second);
					}

					ActionItem* item = [[ActionItem alloc] init];
					item.itemName      = to_ns(name);
					item.location      = to_ns(displayPath);
					item.file          = to_ns(path);
					item.keyEquivalent = to_ns(key);
					item.eclipsed      = !keysSeen.insert(key).second ? YES : NO;
					item.action        = NSSelectorFromString(to_ns(action));
					[items addObject:item];
				}
			}
		}

		if(self.searchSource & kSearchSourceSettingsItems)
		{
			for(auto const& info : settings_info_for_path(to_s(self.path), self.searchAllScopes ? scope::wildcard : self.scope.right, to_s(self.directory)))
			{
				std::string const name = info.variable;
				std::string const path = info.path == NULL_STR ? "TextMate.app ▸ Preferences" : (path::is_child(info.path, oak::application_t::path()) ? "TextMate.app ▸ " + path::name(info.path) : path::with_tilde(info.path)) + (info.section == NULL_STR ? "" : " ▸ " + info.section);

				ActionItem* item = [[ActionItem alloc] init];
				item.itemName = to_ns(name);
				item.value    = to_ns(format(info.value));
				item.location = to_ns(path);
				item.file     = to_ns(info.path);
				item.line     = [NSString stringWithFormat:@"%zu", info.line_number];
				[items addObject:item];
			}
		}

		_unfilteredItems = items;
	}
	return _unfilteredItems;
}

- (void)updateItems:(id)sender
{
	NSArray<NSString*>* identifiers = [[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] stringsForAbbreviation:self.filterString];
	NSString* filter = self.keyEquivalentInput ? self.keyEquivalentString : self.filterString;

	NSArray<ActionItem*>* items = [self unfilteredItems];

	BOOL preserveOrder = (self.searchSource & kSearchSourceSettingsItems) || (_bundleItemField == kBundleItemKeyEquivalentField && OakNotEmptyString(filter));
	[items enumerateObjectsWithOptions:NSEnumerationConcurrent usingBlock:^(ActionItem* item, NSUInteger idx, BOOL* stop){
		double rank = preserveOrder ? (items.count - idx) / (double)items.count : 1;
		[item updateRankUsingFilter:to_s(filter) bundleItemField:_bundleItemField searchSource:_searchSource bindings:identifiers defaultRank:rank];
	}];

	NSArray* res = [items filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"isMatched == YES"]];
	res = [res sortedArrayUsingDescriptors:@[
		[NSSortDescriptor sortDescriptorWithKey:@"rank" ascending:YES],
		[NSSortDescriptor sortDescriptorWithKey:@"itemName" ascending:YES selector:@selector(localizedCompare:)],
		[NSSortDescriptor sortDescriptorWithKey:@"location" ascending:YES selector:@selector(localizedCompare:)]
	]];

	self.items = res;

	self.window.title = [NSString stringWithFormat:@"Select Bundle Item (%@)", self.itemCountTextField.stringValue];
}

- (void)updateStatusText:(id)sender
{
	NSString* status = nil;
	if(self.tableView.selectedRow != -1)
	{
		if(ActionItem* item = self.items[self.tableView.selectedRow])
			status = item.semanticClass ?: item.scopeSelector ?: NSStringFromSelector(item.action);
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
	ActionItem* item = self.tableView.selectedRow != -1 ? self.items[self.tableView.selectedRow] : nil;
	return item.menuItem || item.action || item.uuid && bundles::lookup(to_s(item.uuid))->kind() != bundles::kItemTypeSettings;
}

- (BOOL)canEdit
{
	ActionItem* item = self.tableView.selectedRow != -1 ? self.items[self.tableView.selectedRow] : nil;
	return item.uuid && self.editAction || item.file;
}

- (void)accept:(id)sender
{
	if(_bundleItemField == kBundleItemTitleField && OakNotEmptyString(self.filterString) && (self.tableView.selectedRow > 0 || [self.filterString length] > 1))
	{
		ActionItem* item = self.items[self.tableView.selectedRow];
		if(NSString* identifier = item.uuid ?: NSStringFromSelector(item.action) ?: OakMenuItemIdentifier(item.menuItem))
			[[OakAbbreviations abbreviationsForName:@"OakBundleItemChooserBindings"] learnAbbreviation:self.filterString forString:identifier];
	}

	if(self.tableView.selectedRow != -1)
	{
		ActionItem* item = self.items[self.tableView.selectedRow];

		SEL action = NULL;
		id target = nil, sender = self;

		if(NSMenuItem* menuItem = item.menuItem)
		{
			target = menuItem.target;
			action = menuItem.action;
			sender = menuItem;
		}
		else
		{
			action = item.action;
		}

		if(action)
		{
			[self.window orderOut:self];
			[NSApp sendAction:action to:target from:sender];
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
