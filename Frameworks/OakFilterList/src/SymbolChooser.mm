#import "SymbolChooser.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakFoundation.h>
#import <text/ranker.h>
#import <ns/ns.h>

@interface SymbolChooserItem : NSObject
@property (nonatomic) NSString* path;
@property (nonatomic) NSString* identifier;
@property (nonatomic) NSString* selectionString;
@property (nonatomic) NSAttributedString* name;
@property (nonatomic) NSString* infoString;
@end

@implementation SymbolChooserItem
- (id)objectForKey:(id)key { return [self valueForKey:key]; }
@end

static SymbolChooserItem* CreateItem (OakDocument* document, text::pos_t const& pos, NSString* symbol, std::vector< std::pair<size_t, size_t> > const& ranges)
{
	SymbolChooserItem* res = [SymbolChooserItem new];
	res.path            = document.path;
	res.identifier      = document.identifier.UUIDString;
	res.selectionString = [NSString stringWithCxxString:pos];
	res.name            = CreateAttributedStringWithMarkedUpRanges(to_s(symbol), ranges, NSLineBreakByTruncatingTail);
	res.infoString      = [document.displayName stringByAppendingFormat:@":%@", res.selectionString];
	return res;
}

@implementation SymbolChooser
+ (instancetype)sharedInstance
{
	static SymbolChooser* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		self.window.title = @"Jump to Symbol";

		NSDictionary* titlebarViews = @{
			@"searchField": self.searchField,
		};

		NSView* titlebarView = [[NSView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview(titlebarViews.allValues, titlebarView);

		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField]-(8)-|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(4)-[searchField]-(8)-|" options:0 metrics:nil views:titlebarViews]];
		[self addTitlebarAccessoryView:titlebarView];

		NSDictionary* footerViews = @{
			@"dividerView":        [self makeDividerView],
			@"statusTextField":    self.statusTextField,
			@"itemCountTextField": self.itemCountTextField,
		};

		NSView* footerView = self.footerView;
		OakAddAutoLayoutViewsToSuperview(footerViews.allValues, footerView);

		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[dividerView]|"                                 options:0 metrics:nil views:footerViews]];
		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[statusTextField]-[itemCountTextField]-|"      options:NSLayoutFormatAlignAllCenterY metrics:nil views:footerViews]];
		[footerView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[dividerView(==1)]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:footerViews]];

		[self updateScrollViewInsets];

		OakSetupKeyViewLoop(@[ self.searchField ]);
	}
	return self;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self setTMDocument:nil];
}

- (void)setTMDocument:(OakDocument*)aDocument
{
	if(_TMDocument = aDocument)
		[self updateItems:self];
	NSString* title = @"Jump to Symbol";
	self.window.title = _TMDocument ? [title stringByAppendingFormat:@" — %@", _TMDocument.displayName] : title;
}

- (void)setSelectionString:(NSString*)aString
{
	_selectionString = aString;

	std::map<text::pos_t, SymbolChooserItem*> symbolItems;
	for(SymbolChooserItem* item in self.items)
	{
		text::selection_t sel(to_s(item.selectionString));
		text::pos_t pos = sel.last().min();
		symbolItems.emplace(pos, item);
	}

	SymbolChooserItem* item = nil;
	for(text::range_t const& range : text::selection_t(to_s(_selectionString)))
	{
		auto it = symbolItems.upper_bound(range.min());
		if(it != symbolItems.begin())
			item = (--it)->second;
	}

	if(item)
	{
		NSInteger row = [self.items indexOfObject:item];
		[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[self.tableView scrollRowToVisible:row];
	}
}

- (void)updateItems:(id)sender
{
	NSMutableArray* res = [NSMutableArray array];
	if(_TMDocument)
	{
		if(OakIsEmptyString(self.filterString))
		{
			[_TMDocument enumerateSymbolsUsingBlock:^(text::pos_t const& pos, NSString* symbol){
				if(![symbol isEqualToString:@"-"])
					[res addObject:CreateItem(_TMDocument, pos, symbol, std::vector< std::pair<size_t, size_t> >())];
			}];
		}
		else
		{
			std::string const filter = oak::normalize_filter(to_s(self.filterString));

			__block NSString* sectionName = nil;
			__block std::multimap<double, SymbolChooserItem*> rankedItems;

			[_TMDocument enumerateSymbolsUsingBlock:^(text::pos_t const& pos, NSString* symbol){
				if([symbol isEqualToString:@"-"])
					return;

				BOOL indented = [symbol hasPrefix:@"\u2003"];
				if(!indented)
					sectionName = symbol;

				std::vector< std::pair<size_t, size_t> > ranges;
				if(double rank = oak::rank(filter, to_s(symbol), &ranges))
					rankedItems.emplace(1 - rank, CreateItem(_TMDocument, pos, indented && sectionName ? [NSString stringWithFormat:@"%@ — %@", symbol, sectionName] : symbol, ranges));
			}];

			for(auto const& pair : rankedItems)
				[res addObject:pair.second];
		}
	}
	self.items = res;
}

- (void)updateStatusText:(id)sender
{
	if(self.items.count != 0)
	{
		SymbolChooserItem* item = self.items[self.tableView.selectedRow == -1 ? 0 : self.tableView.selectedRow];
		self.statusTextField.stringValue = item.infoString;
	}
	else
	{
		self.statusTextField.stringValue = @"";
	}
}
@end
