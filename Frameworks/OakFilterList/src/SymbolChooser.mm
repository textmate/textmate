#import "SymbolChooser.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
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

static SymbolChooserItem* CreateItem (document::document_ptr const& document, text::pos_t const& pos, std::string const& candidate, std::vector< std::pair<size_t, size_t> > const& ranges)
{
	SymbolChooserItem* res = [SymbolChooserItem new];
	res.path            = [NSString stringWithCxxString:document->path()];
	res.identifier      = [NSString stringWithCxxString:document->identifier()];
	res.selectionString = [NSString stringWithCxxString:pos];
	res.name            = CreateAttributedStringWithMarkedUpRanges(candidate, ranges);
	res.infoString      = [NSString stringWithCxxString:document->display_name() + ":" + (std::string)pos];
	return res;
}

@implementation SymbolChooser
+ (instancetype)sharedInstance
{
	static id sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		self.window.title = @"Go to Symbol";

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"topDivider"         : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"scrollView"         : self.scrollView,
			@"bottomDivider"      : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"statusTextField"    : self.statusTextField,
			@"itemCountTextField" : self.itemCountTextField,
		};

		NSView* contentView = self.window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField(>=50)]-(8)-|"                      options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"         options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[statusTextField]-[itemCountTextField]-|"           options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:views]];
	}
	return self;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self setDocument:document::document_ptr()];
}

- (void)setDocument:(document::document_ptr)aDocument
{
	struct callback_t : document::open_callback_t
	{
		callback_t (SymbolChooser* self) : _self(self) { }

		void show_error (std::string const& path, document::document_ptr document, std::string const& message, oak::uuid_t const& filter)
		{
			fprintf(stderr, "%s: %s\n", path.c_str(), message.c_str());
		}

		void show_document (std::string const& path, document::document_ptr document)
		{
			[_self updateItems:_self];
		}

	private:
		SymbolChooser* _self;
	};

	if(_document)
		_document->close();
	if((_document = aDocument) && _document->try_open(std::make_shared<callback_t>(self)))
		[self updateItems:self];

	self.window.title = _document ? [NSString stringWithFormat:@"Go to Symbol — %@", [NSString stringWithCxxString:_document->display_name()]] : @"Go to Symbol";
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

	NSInteger row = item ? [self.items indexOfObject:item] : 0;
	[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
	[self.tableView scrollRowToVisible:row];
}

- (void)updateItems:(id)sender
{
	NSMutableArray* res = [NSMutableArray array];
	if(_document)
	{
		std::string filter = to_s(self.filterString);
		if(filter == NULL_STR || filter == "")
		{
			for(auto const& pair : _document->symbols())
			{
				if(pair.second != "-")
					[res addObject:CreateItem(_document, pair.first, pair.second, std::vector< std::pair<size_t, size_t> >())];
			}
		}
		else
		{
			std::string sectionName = NULL_STR;
			std::multimap<double, SymbolChooserItem*> rankedItems;
			for(auto const& pair : _document->symbols())
			{
				if(pair.second == "-")
					continue;

				bool indented = pair.second.find("\u2003") == 0;
				if(!indented)
					sectionName = pair.second;

				std::vector< std::pair<size_t, size_t> > ranges;
				if(double rank = oak::rank(filter, pair.second, &ranges))
					rankedItems.emplace(1 - rank, CreateItem(_document, pair.first, indented && sectionName != NULL_STR ? (pair.second + " — " + sectionName) : pair.second, ranges));
			}

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
