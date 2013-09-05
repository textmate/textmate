#import "SymbolList.h"
#import "../highlight_ranges.h"
#import <OakFoundation/NSString Additions.h>
#import <text/ranker.h>

@implementation FileChooserSymbolItem
- (id)initWithPath:(NSString*)aPath selectionString:(NSString*)aSelectionString identifier:(NSString*)anIdentifier displayString:(NSAttributedString*)aDisplayString infoString:(NSAttributedString*)anInfoString
{
	if((self = [super init]))
	{
		self.path            = aPath;
		self.identifier      = anIdentifier;
		self.selectionString = aSelectionString;
		self.displayString   = aDisplayString;
		self.infoString      = anInfoString;
	}
	return self;
}

- (id)objectForKey:(id)key { return [self valueForKey:key]; }
@end

static FileChooserSymbolItem* CreateItem (document::document_ptr const& document, text::pos_t const& pos, std::string const& candidate, std::vector< std::pair<size_t, size_t> > const& ranges)
{
	NSString* path              = [NSString stringWithCxxString:document->path()];
	NSString* selection         = [NSString stringWithCxxString:pos];
	NSString* identifier        = [NSString stringWithCxxString:document->identifier()];
	NSAttributedString* display = AttributedStringWithMarkedUpRanges(candidate, ranges);
	NSAttributedString* info    = [[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:document->display_name() + ":" + (std::string)pos]];
	return [[FileChooserSymbolItem alloc] initWithPath:path selectionString:selection identifier:identifier displayString:display infoString:info];
}

NSArray* SymbolListForDocument (document::document_ptr const& document, std::string const& filter)
{
	NSMutableArray* res = [NSMutableArray array];
	if(filter == NULL_STR || filter == "")
	{
		citerate(it, document->symbols())
		{
			if(it->second != "-")
				[res addObject:CreateItem(document, it->first, it->second, std::vector< std::pair<size_t, size_t> >())];
		}
		return res;
	}

	std::string sectionName = NULL_STR;
	std::multimap<double, FileChooserSymbolItem*> rankedItems;
	citerate(it, document->symbols())
	{
		if(it->second == "-")
			continue;

		bool indented = it->second.find("\u2003") == 0;
		if(!indented)
			sectionName = it->second;

		std::vector< std::pair<size_t, size_t> > ranges;
		if(double rank = oak::rank(filter, it->second, &ranges))
			rankedItems.emplace(1 - rank, CreateItem(document, it->first, indented && sectionName != NULL_STR ? (it->second + " — " + sectionName) : it->second, ranges));
	}

	iterate(pair, rankedItems)
		[res addObject:pair->second];

	return res;
}
