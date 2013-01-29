#import "SymbolChooser.h"
#import "SymbolList.h"
#import <DocumentWindow/DocumentController.h>
#import <OakFoundation/NSString Additions.h>
#import <OakTextView/OakDocumentView.h>
#import <text/ranker.h>
#import <text/case.h>
#import "../highlight_ranges.h"

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
- (void)updateSymbols;
@end

@implementation SymbolChooser
- (NSViewController*)viewController
{
	if(!viewController)
		viewController = [[SymbolChooserViewController alloc] initWithSymbolChooser:self];
	return viewController;
}

- (id)initWithDocumentView:(OakDocumentView *)aDocumentView
{
	if(self = [super init])
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
				[_self updateSymbols];
			}

		private:
			SymbolChooser* _self;
		};

		document = [aDocumentView document];
		documentView = aDocumentView;
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(updateSymbols) name:FLDataSourceItemsDidChangeNotification object:documentView];
		if(document->try_open(document::open_callback_ptr((document::open_callback_t*)new callback_t(self))))
			[self updateSymbols];
	}
	return self;
}

+ (id)symbolChooserForDocumentView:(OakDocumentView *)aDocumentView
{
	return [[SymbolChooser alloc] initWithDocumentView:aDocumentView];
}

- (NSString*)title
{
	return @"Go to Symbol";
}

- (NSString*)filterString
{
	return [NSString stringWithCxxString:filterString];
}

- (void)updateSymbols
{
	[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
}

- (IBAction)search:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(stringValue)]);
	std::string const& newFilterString = [[[sender stringValue] lowercaseString] UTF8String];
	if(newFilterString != filterString)
	{
		filterString = newFilterString;
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (NSArray*)items
{
	DocumentController *controller = [documentView window].delegate;
	if ([controller selectedDocument] != document)
	{
		[controller showSymbolChooser:nil];
		[[documentView window] makeKeyWindow];
	}
	_items = SymbolListForDocument(document, filterString);
	return _items;
}

- (NSAttributedString*)displayStringForItem:(id)item
{
	return [item displayString];
}

- (NSAttributedString*)infoStringForItem:(id)item
{
	return [item infoString];
}

- (NSArray*)selectedItems
{
	text::selection_t sel([[documentView textView].selectionString UTF8String]);
	ng::buffer_t const& buf = document->buffer();
	size_t min = buf.convert(sel.last().min());
	size_t max = buf.convert(sel.last().max());
	BOOL past_selection = NO;
	NSUInteger last_index;
	NSUInteger index = 0;
	NSMutableIndexSet* indexesToSelect = [NSMutableIndexSet indexSet];
	for (id item in _items)
	{
		text::selection_t pos([[item selectionString] UTF8String]);
		size_t start = buf.convert(pos.last().min());
		size_t end = buf.convert(pos.last().max());
		last_index = index;
		index = [_items indexOfObject:item];
		if (start > min)
		{
			if (!past_selection && last_index != NSNotFound)
				[indexesToSelect addIndex:last_index];
			past_selection = YES;
			if (end <= max)
			{
				if(index != NSNotFound)
					[indexesToSelect addIndex:index];
			}
		}
	}
	if (!past_selection)
		[indexesToSelect addIndex:index];
	return [_items objectsAtIndexes:indexesToSelect];
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self name:FLDataSourceItemsDidChangeNotification object:documentView];
	document->close();
}
@end
