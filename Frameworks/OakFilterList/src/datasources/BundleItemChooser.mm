#import "BundleItemChooser.h"
#import "../OakAbbreviations.h"
#import "../highlight_ranges.h"
#import <bundles/bundles.h>
#import <ns/ns.h>
#import <scope/scope.h>
#import <oak/CocoaSTL.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/OakSubmenuController.h>
#import <text/case.h>
#import <text/ctype.h>
#import <text/ranker.h>
#import "OakBundleItemCell.h"
#import <OakAppKit/OakKeyEquivalentView.h>
#import <MGScopeBar/MGScopeBar.h>

OAK_DEBUG_VAR(FilterList_BundleItemChooser);

@interface BundleItemChooserItem : NSObject
{
	NSUInteger index;
	oak::uuid_t uuid;
}
@property (nonatomic, assign) NSUInteger index;
- (NSString*)uuid;
- (void)setUuid:(oak::uuid_t const&)other;
@end

@implementation BundleItemChooserItem
@synthesize index;

- (NSString*)uuid
{
	return [NSString stringWithCxxString:uuid];
}

- (void)setUuid:(oak::uuid_t const&)other
{
	uuid = other;
}

- (id)objectForKey:(id)anId
{
	ASSERT([anId isEqual:@"uuid"]);
	return [self uuid];
}

- (BOOL)isEqual:(id)anotherItem
{
	return [anotherItem isKindOfClass:[BundleItemChooserItem class]] && ((BundleItemChooserItem*)anotherItem)->uuid == uuid;
}
@end

@interface OakScopeBar : MGScopeBar
{
}
@end

@implementation OakScopeBar
- (void)drawRect:(NSRect)aRect
{
	[super drawRect:aRect];

	NSRect lineRect = [self bounds];
	lineRect.origin.y += 1;
	lineRect.size.height = 1;
	[[NSColor colorWithCalibratedWhite:0.59 alpha:1.0] set];
	NSRectFill(lineRect);

	lineRect = [self bounds];
	lineRect.size.height = 1;
	[[NSColor darkGrayColor] set];
	NSRectFill(lineRect);
}
@end

@interface BundleItemChooserViewController : NSViewController <MGScopeBarDelegate>
{
	NSSearchField* searchField;
	OakKeyEquivalentView* keyEquivField;
	NSSegmentedControl* sourceSelector;
	MGScopeBar* scopeBar;
	BundleItemChooser* itemChooser;
}
@property (nonatomic, assign) BundleItemChooser* itemChooser;
@end

@interface BundleItemChooserView : NSView
{
	BundleItemChooserViewController* viewController;
}
@property (nonatomic, assign) BundleItemChooserViewController* viewController;
@end

@implementation BundleItemChooserView
@synthesize viewController;

- (void)viewDidMoveToWindow
{
	[self.window makeFirstResponder:[self.viewController valueForKey:self.viewController.itemChooser.keyEquivalentSearch ? @"keyEquivField" : @"searchField"]];
	self.viewController = nil;
}
@end

@implementation BundleItemChooserViewController
@synthesize itemChooser;

static NSString* const TitleSearchMode         = @"TitleSearchMode";
static NSString* const KeyEquivalentSearchMode = @"KeyEquivalentSearchMode";

static NSString* const CurrentScope = @"CurrentScope";
static NSString* const AllScopes    = @"AllScopes";

- (id)initWithBundleItemChooser:(BundleItemChooser*)chooser
{
	if((self = [super init]))
	{
		self.itemChooser = chooser;

		static const CGFloat initialViewWidth = 1200;

		searchField                  = [[[NSSearchField alloc] initWithFrame:NSMakeRect(7, 8, initialViewWidth-14, 22)] autorelease];
		searchField.action           = @selector(didChangeFilterString:);
		searchField.target           = self;
		searchField.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[searchField.cell setScrollable:YES];

		keyEquivField                      = [[[OakKeyEquivalentView alloc] initWithFrame:searchField.frame] autorelease];
		keyEquivField.autoresizingMask     = NSViewWidthSizable|NSViewMinYMargin;
		keyEquivField.disableGlobalHotkeys = NO;
		[keyEquivField setHidden:YES];

		scopeBar                  = [[[OakScopeBar alloc] initWithFrame:NSMakeRect(0, NSMaxY(searchField.frame) + 6, initialViewWidth, 25)] autorelease];
		scopeBar.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;

		self.view = [[[BundleItemChooserView alloc] initWithFrame:NSMakeRect(0, 0, initialViewWidth, NSMaxY(scopeBar.frame))] autorelease];
		((BundleItemChooserView*)self.view).viewController = self;
		self.view.autoresizingMask = NSViewWidthSizable;
		[self.view addSubview:searchField];
		[self.view addSubview:keyEquivField];

		[self.view addSubview:scopeBar];

		BOOL searchAllScopes     = itemChooser.searchAllScopes;
		BOOL keyEquivalentSearch = itemChooser.keyEquivalentSearch;
		search::type searchType  = itemChooser.searchType;
		NSString* filterString   = itemChooser.filterString;

		scopeBar.delegate = self;
		[scopeBar reloadData];
		[scopeBar adjustSubviews];

		[scopeBar setSelected:searchAllScopes forItem:AllScopes inGroup:0];
		[scopeBar setSelected:keyEquivalentSearch forItem:KeyEquivalentSearchMode inGroup:1];
		switch(searchType)
		{
			case search::actions:  [scopeBar setSelected:YES forItem:@"Actions" inGroup:2];  break;
			case search::grammars: [scopeBar setSelected:YES forItem:@"Grammars" inGroup:2]; break;
			case search::themes:   [scopeBar setSelected:YES forItem:@"Themes" inGroup:2];   break;
		}

		itemChooser.filterString = filterString;

		if(itemChooser.keyEquivalentSearch)
				keyEquivField.eventString = filterString;
		else	searchField.stringValue = filterString;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(viewFrameDidChange:) name:NSViewFrameDidChangeNotification object:self.view];
		[keyEquivField bind:@"value" toObject:chooser withKeyPath:@"filterString" options:nil];
	}
	return self;
}

- (void)dealloc
{
	searchField.target = nil;
	searchField.action = NULL;
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	[keyEquivField unbind:@"value"];
	self.itemChooser = nil;
	[super dealloc];
}

- (int)numberOfGroupsInScopeBar:(MGScopeBar*)theScopeBar
{
	return 3;
}

- (NSArray*)scopeBar:(MGScopeBar*)theScopeBar itemIdentifiersForGroup:(int)groupNumber
{
	if(groupNumber == 0)
		return @[ CurrentScope, AllScopes ];
	else if(groupNumber == 1)
		return @[ TitleSearchMode, KeyEquivalentSearchMode ];
	else if(groupNumber == 2)
		return @[ @"Actions", @"Grammars", @"Themes" ];
	return nil;
}

- (NSString*)scopeBar:(MGScopeBar*)theScopeBar labelForGroup:(int)groupNumber
{
	return nil;
}

- (MGScopeBarGroupSelectionMode)scopeBar:(MGScopeBar*)theScopeBar selectionModeForGroup:(int)groupNumber
{
	return MGRadioSelectionMode;
}

- (NSString*)scopeBar:(MGScopeBar*)theScopeBar titleOfItem:(NSString*)identifier inGroup:(int)groupNumber
{
	if(identifier == TitleSearchMode)               return @"Title";
	else if(identifier == KeyEquivalentSearchMode)  return @"Key Equivalent";
	else if(identifier == CurrentScope)             return @"Current Scope";
	else if(identifier == AllScopes)                return @"All Scopes";
	else                                            return identifier;
}

- (void)scopeBar:(MGScopeBar*)theScopeBar selectedStateChanged:(BOOL)selected forItem:(NSString*)identifier inGroup:(int)groupNumber
{
	if(!selected)
		return;

	if(groupNumber == 1)
	{
		if(identifier == TitleSearchMode)
		{
			[searchField setHidden:NO];
			[keyEquivField setHidden:YES];
			[self.view.window makeFirstResponder:searchField];
			itemChooser.keyEquivalentSearch = NO;
		}
		else if(identifier == KeyEquivalentSearchMode)
		{
			[searchField setHidden:YES];
			[keyEquivField setHidden:NO];
			[self.view.window makeFirstResponder:keyEquivField];
			itemChooser.keyEquivalentSearch = YES;
			keyEquivField.recording = YES;
		}
	}
	else if(groupNumber == 0)
	{
		itemChooser.searchAllScopes = identifier == AllScopes;
	}
	else if(groupNumber == 2)
	{
		if([identifier isEqualToString:@"Actions"])       itemChooser.searchType = search::actions;
		else if([identifier isEqualToString:@"Grammars"]) itemChooser.searchType = search::grammars;
		else if([identifier isEqualToString:@"Themes"])   itemChooser.searchType = search::themes;
	}
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(!self.view.window.isKeyWindow)
	{
		[aMenu addItemWithTitle:@"No Sources" action:@selector(nop:) keyEquivalent:@""];
		return;
	}

	int groupCount = [self numberOfGroupsInScopeBar:scopeBar];
	int key = 0;
	for(NSUInteger groupIndex = 0; groupIndex < groupCount; ++groupIndex)
	{
		NSArray* identifiers = [self scopeBar:scopeBar itemIdentifiersForGroup:groupIndex];
		for(NSUInteger index = 0; index < identifiers.count; ++index)
		{
			NSString* identifier   = [identifiers objectAtIndex:index];
			NSString* label        = [self scopeBar:scopeBar titleOfItem:identifier inGroup:groupIndex];
			NSMenuItem* item       = [aMenu addItemWithTitle:label action:@selector(takeSelectedItemFrom:) keyEquivalent:++key < 10 ? [NSString stringWithFormat:@"%c", '0' + (key % 10)] : @""];
			item.representedObject = identifier;
			item.tag               = groupIndex;
			if([[scopeBar.selectedItems objectAtIndex:groupIndex] containsObject:identifier])
				item.state = NSOnState;
		}
		if(groupIndex+1 < groupCount)
			[aMenu addItem:[NSMenuItem separatorItem]];
	}
}

- (void)takeSelectedItemFrom:(NSMenuItem*)item
{
	NSString* identifier  = [[OakSubmenuController sharedInstance] representedObjectForSender:item];
	NSUInteger groupIndex = [[OakSubmenuController sharedInstance] tagForSender:item];
	[scopeBar setSelected:YES forItem:identifier inGroup:groupIndex];
}

- (void)didChangeFilterString:(NSSearchField*)sender { itemChooser.filterString = sender.stringValue; }
- (void)setSearchFieldDelegate:(id)aDelegate         { searchField.delegate = aDelegate; }

- (void)viewFrameDidChange:(NSNotification*)notification
{
	// Distribute source segment widths
	for(NSUInteger index = 0; index < sourceSelector.segmentCount; ++index)
		[sourceSelector setWidth:(sourceSelector.superview.frame.size.width / sourceSelector.segmentCount) forSegment:index];
}
@end

@implementation BundleItemChooser
@synthesize keyEquivalentSearch, searchAllScopes, searchType;

static std::vector<bundles::item_ptr> relevant_items_in_scope (search::type searchType, scope::context_t const& scope)
{
	int kindMask = 0;
	if(searchType == search::actions)
		kindMask = bundles::kItemTypeMenuTypes;
	else if(searchType == search::grammars)
		kindMask = bundles::kItemTypeGrammar;
	else if(searchType == search::themes)
		kindMask = bundles::kItemTypeTheme;

	std::multimap<std::string, bundles::item_ptr, text::less_t> sorted;
	citerate(item, bundles::query(bundles::kFieldAny, NULL_STR, scope, kindMask, oak::uuid_t(), false))
		sorted.insert(std::make_pair((*item)->full_name(), *item));

	std::vector<bundles::item_ptr> res;
	std::transform(sorted.begin(), sorted.end(), back_inserter(res), [](std::pair<std::string, bundles::item_ptr> const& p){ return p.second; });
	return res;
}

- (id)initWithScope:(scope::context_t const&)aScope
{
	D(DBF_FilterList_BundleItemChooser, bug("scope: ‘%s’, ‘%s’\n", to_s(aScope.left).c_str(), to_s(aScope.right).c_str()););
	if(self = [super init])
	{
		scope           = aScope;
		self.searchType = search::actions;
	}
	return self;
}

+ (id)bundleItemChooserForScope:(scope::context_t const&)aScope
{
	return [[[self alloc] initWithScope:aScope] autorelease];
}

- (void)dealloc
{
	[viewController release];
	[super dealloc];
}

- (NSString*)title
{
	return @"Select Bundle Item";
}

- (NSTextFieldCell*)itemDataCell
{
	return [[[OakBundleItemCell alloc] initTextCell:@""] autorelease];
}

- (void)setSearchType:(search::type)newType
{
	if(newType != searchType || all_items.empty())
	{
		searchType = newType;
		all_items = relevant_items_in_scope(searchType, scope::wildcard);

		citerate(item, relevant_items_in_scope(searchType, scope))
			items_filtered_by_scope.insert((*item)->uuid());
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (void)setKeyEquivalentSearch:(BOOL)flag
{
	if(keyEquivalentSearch != flag)
	{
		keyEquivalentSearch = flag;
		self.filterString = nil;
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (void)setSearchAllScopes:(BOOL)flag
{
	if(searchAllScopes != flag)
	{
		searchAllScopes = flag;
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (NSString*)filterString
{
	return [NSString stringWithCxxString:originalFilterString];
}

- (void)setFilterString:(NSString*)string
{
	originalFilterString = string.UTF8String ?: "";
	std::string const& newFilterString = string.lowercaseString.UTF8String ?: "";
	if(newFilterString != filterString)
	{
		filterString = newFilterString;
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (NSViewController*)viewController
{
	if(!viewController)
		viewController = [[BundleItemChooserViewController alloc] initWithBundleItemChooser:self];
	return viewController;
}

- (NSButtonCell*)accessoryButton
{
	NSButtonCell* button = [[NSButtonCell new] autorelease];
	[button setButtonType:NSSwitchButton];
	[button setBezelStyle:NSSmallSquareBezelStyle];
	[button setImagePosition:NSImageOnly];
	[button setBordered:NO];
	[button setImage:[NSImage imageNamed:NSImageNameFollowLinkFreestandingTemplate]];
	[button setAlternateImage:[NSImage imageNamed:NSImageNameFollowLinkFreestandingTemplate]];
	return button;
}

- (NSArray*)items
{
	std::multimap<double, BundleItemChooserItem*> rankedItems;

	if(keyEquivalentSearch)
	{
		for(size_t index = 0; index < all_items.size(); ++index)
		{
			if(!searchAllScopes && items_filtered_by_scope.find(all_items[index]->uuid()) == items_filtered_by_scope.end())
				continue;

			if(!filterString.empty() && all_items[index]->value_for_field(bundles::kFieldKeyEquivalent) != originalFilterString)
				continue;

			BundleItemChooserItem* item = [[BundleItemChooserItem new] autorelease];
			[item setIndex:index];
			[item setUuid:all_items[index]->uuid()];
			rankedItems.insert(std::make_pair(0, item));
		}
	}
	else
	{
		NSArray* bestMatches = [[OakAbbreviations abbreviationsForName:@"BundleItemChooserBindings"] stringsForAbbreviation:[NSString stringWithCxxString:filterString]];
		for(size_t index = 0; index < all_items.size(); ++index)
		{
			if(!searchAllScopes && items_filtered_by_scope.find(all_items[index]->uuid()) == items_filtered_by_scope.end())
				continue;

			double rank = 1;
			NSUInteger bestMatchIndex = [bestMatches indexOfObject:[NSString stringWithCxxString:all_items[index]->uuid()]];
			if(bestMatchIndex != NSNotFound)
			{
				rank = -((double)(bestMatches.count - bestMatchIndex) - 1); // since we go from 0-1 with 0 = best, <0 means “even better” :)
			}
			else if(!filterString.empty())
			{
				double nameRank    = oak::rank(filterString, all_items[index]->full_name());
				double triggerRank = oak::rank(filterString, all_items[index]->value_for_field(bundles::kFieldTabTrigger));
				if(nameRank == 0 && triggerRank == 0)
					continue;
				rank = 1 - std::max(nameRank, triggerRank);
			}

			BundleItemChooserItem* item = [[BundleItemChooserItem new] autorelease];
			[item setIndex:index];
			[item setUuid:all_items[index]->uuid()];
			rankedItems.insert(std::make_pair(rank, item));
		}
	}

	NSMutableArray* items = [NSMutableArray array];
	iterate(it, rankedItems)
		[items addObject:it->second];

	return items;
}

- (NSAttributedString*)displayStringForItem:(id)item
{
	NSUInteger index = [item index];
	std::string const& itemName = all_items[index]->full_name();

	std::vector< std::pair<size_t, size_t> > ranges;
	double nameRank    = oak::rank(filterString, all_items[index]->full_name());
	double triggerRank = oak::rank(filterString, all_items[index]->value_for_field(bundles::kFieldTabTrigger));
	if(nameRank > triggerRank)
		oak::rank(text::lowercase(filterString), itemName, &ranges);
	return AttributedStringWithMarkedUpRanges(itemName, ranges);
}

- (void)makeItemsBestFitForCurrentSearch:(NSArray*)theItems
{
	if(!keyEquivalentSearch)
	{
		for(BundleItemChooserItem* item in theItems)
			[[OakAbbreviations abbreviationsForName:@"BundleItemChooserBindings"] learnAbbreviation:self.filterString forString:[item uuid]];
	}
}

- (void)willDisplayCell:(NSTextFieldCell*)aCell forItem:(id)anItem
{
	NSUInteger index = [anItem index];
	[(OakBundleItemCell*)aCell setKeyEquivalent:[NSString stringWithCxxString:all_items[index]->value_for_field(bundles::kFieldKeyEquivalent)]];

	std::string const& tabTrigger = all_items[index]->value_for_field(bundles::kFieldTabTrigger);
	if(tabTrigger != NULL_STR)
	{
		double nameRank    = oak::rank(filterString, all_items[index]->full_name());
		double triggerRank = oak::rank(filterString, tabTrigger);
		if(triggerRank > nameRank)
		{
			std::vector< std::pair<size_t, size_t> > ranges;
			oak::rank(text::lowercase(filterString), tabTrigger, &ranges);
			[(OakBundleItemCell*)aCell setAttributedTabTrigger:AttributedStringWithMarkedUpRanges(tabTrigger, ranges)];
		}
		else
		{
			[(OakBundleItemCell*)aCell setTabTrigger:[NSString stringWithCxxString:tabTrigger]];
		}
	}
	else
	{
		[(OakBundleItemCell*)aCell setTabTrigger:nil];
	}
}
@end
