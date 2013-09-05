#import "SymbolChooser.h"
#import "SymbolList.h"
#import <OakFoundation/NSString Additions.h>
#import <text/ranker.h>
#import <text/case.h>
#import "../highlight_ranges.h"
#import <ns/ns.h>

OAK_DEBUG_VAR(FilterList_SymbolChooser);

@interface SymbolChooserViewController : NSViewController
{
	NSSearchField* searchField;
	SymbolChooser* symbolChooser;
}
@end

@implementation SymbolChooserViewController
- (id)initWithSymbolChooser:(SymbolChooser*)chooser
{
	if((self = [super init]))
	{
		symbolChooser = chooser;

		searchField                  = [[NSSearchField alloc] initWithFrame:NSMakeRect(10, 10, 180, 22)];
		searchField.target           = symbolChooser;
		searchField.action           = @selector(search:);
		searchField.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[searchField.cell setScrollable:YES];

		self.view                  = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 200, NSMaxY(searchField.frame) + 8)];
		self.view.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[self.view addSubview:searchField];
	}
	return self;
}

- (void)dealloc
{
	searchField.delegate = nil;
	searchField.target   = nil;
}

- (void)setSearchFieldDelegate:(id)aDelegate
{
	searchField.delegate = aDelegate;
}
@end

@interface SymbolChooser ()
{
	OBJC_WATCH_LEAKS(SymbolChooser);
	document::document_ptr _document;
	SymbolChooserViewController* _viewController;
}
@property (nonatomic) NSArray* items;
@property (nonatomic) NSArray* selectedItems;
@property (nonatomic) NSString* filterString;
@property (nonatomic) BOOL needsReload;
@end

@implementation SymbolChooser
+ (id)symbolChooserForDocument:(document::document_ptr)aDocument
{
	SymbolChooser* res = [SymbolChooser new];
	res.document = aDocument;
	return res;
}

- (void)dealloc
{
	self.document = document::document_ptr();
}

- (void)updateItemsArray
{
	self.selectedItems = nil;
	self.items = SymbolListForDocument(_document, to_s(_filterString));
	[self updateSelectedItemsArray];
}

- (void)updateSelectedItemsArray
{
	if(!_selectionString || [_selectionString isEqualToString:@""])
	{
		self.selectedItems = nil;
		return;
	}

	NSMutableArray* res = [NSMutableArray array];

	std::map<text::pos_t, FileChooserSymbolItem*> symbolItems;
	for(FileChooserSymbolItem* item in _items)
	{
		text::selection_t sel(to_s(item.selectionString));
		text::pos_t pos = sel.last().min();
		symbolItems.emplace(pos, item);
	}

	for(text::range_t const& range : text::selection_t(to_s(_selectionString)))
	{
		auto it = symbolItems.upper_bound(range.min());
		if(it != symbolItems.begin())
			[res addObject:(--it)->second];
	}

	self.selectedItems = res;
}

- (void)delayedReload:(id)sender
{
	[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	self.needsReload = NO;
}

- (void)setNeedsReload:(BOOL)flag
{
	if(_needsReload == flag)
		return;
	if(_needsReload = flag)
		[self performSelector:@selector(delayedReload:) withObject:self afterDelay:0];
}

- (void)setItems:(NSArray*)anArray
{
	_items = anArray;
	self.needsReload = YES;
}

- (void)setSelectedItems:(NSArray*)anArray
{
	_selectedItems = anArray;
	self.needsReload = YES;
}

// ==================
// = Action methods =
// ==================

- (IBAction)search:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(stringValue)]);
	self.filterString = [[sender stringValue] lowercaseString];
}

// ========================
// = FilterListDataSource =
// ========================

- (NSString*)title
{
	return @"Go to Symbol";
}

- (NSViewController*)viewController
{
	if(!_viewController)
		_viewController = [[SymbolChooserViewController alloc] initWithSymbolChooser:self];
	return _viewController;
}

- (NSAttributedString*)displayStringForItem:(id)item
{
	return [item displayString];
}

- (NSAttributedString*)infoStringForItem:(id)item
{
	return [item infoString];
}

// =======
// = API =
// =======

- (void)setDocument:(document::document_ptr const&)aDocument
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
			[_self updateItemsArray];
		}

	private:
		SymbolChooser* _self;
	};

	if(_document)
		_document->close();

	if(_document = aDocument)
	{
		if(_document->try_open(std::make_shared<callback_t>(self)))
			[self updateItemsArray];
	}
}

- (void)setSelectionString:(NSString*)aString
{
	if(_selectionString != aString && ![_selectionString isEqualToString:aString])
	{
		_selectionString = aString;
		[self updateSelectedItemsArray];
	}
}

- (void)setFilterString:(NSString*)aString
{
	if(_filterString != aString && ![_filterString isEqualToString:aString])
	{
		_filterString = aString;
		[self updateItemsArray];
	}
}
@end
