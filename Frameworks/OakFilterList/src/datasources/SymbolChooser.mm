#import "SymbolChooser.h"
#import "SymbolList.h"
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
	return SymbolListForDocument(document, filterString);
}

- (NSAttributedString*)displayStringForItem:(id)item
{
	return [item displayString];
}

- (NSAttributedString*)infoStringForItem:(id)item
{
	return [item infoString];
}

- (void)dealloc
{
	document->close();
}
@end
