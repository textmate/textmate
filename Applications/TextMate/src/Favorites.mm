#import "Favorites.h"
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <text/ranker.h>
#import <oak/CocoaSTL.h>
#import <io/entries.h>
#import <text/case.h>
#import <io/path.h>
#import <ns/ns.h>

// ===================
// = View Controller =
// ===================

@interface FavoritesViewController : NSViewController
{
	NSSearchField* searchField;
	FavoritesDataSource* favoritesDataSource;
}
@end

@implementation FavoritesViewController
- (id)initWithFavoritesDataSource:(FavoritesDataSource*)aDataSource
{
	if(self = [super init])
	{
		favoritesDataSource          = aDataSource;

		searchField                  = [[[NSSearchField alloc] initWithFrame:NSMakeRect(10, 10, 180, 22)] autorelease];
		searchField.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		searchField.target           = favoritesDataSource;
		searchField.action           = @selector(search:);
		[searchField.cell setScrollable:YES];

		self.view                  = [[[NSView alloc] initWithFrame:NSMakeRect(0, 0, 200, NSMaxY(searchField.frame) + 8)] autorelease];
		self.view.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[self.view addSubview:searchField];
	}
	return self;
}

- (void)dealloc
{
	searchField.target = nil;
	searchField.action = NULL;
	[super dealloc];
}

- (void)setSearchFieldDelegate:(id)aDelegate
{
	searchField.delegate = aDelegate;
}
@end

// ===============
// = Data Source =
// ===============

@implementation FavoritesDataSource
- (NSViewController*)viewController
{
	if(!viewController)
		viewController = [[FavoritesViewController alloc] initWithFavoritesDataSource:self];
	return viewController;
}

- (id)initWithCxxPath:(std::string const&)aPath
{
	if(self = [super init])
	{
		favoritesPath = aPath;
		filterString  = "";

		citerate(entry, path::entries(favoritesPath))
		{
			if((*entry)->d_type == DT_LNK)
			{
				std::string const& path = path::resolve(path::join(favoritesPath, (*entry)->d_name));
				if(strncmp("[DIR] ", (*entry)->d_name, 6) == 0)
				{
					citerate(subentry, path::entries(path))
					{
						if((*subentry)->d_type == DT_DIR)
							favorites.insert(std::make_pair(text::format("%s — %s", (*subentry)->d_name, (*entry)->d_name + 6), path::join(path, (*subentry)->d_name)));
					}
				}
				else
				{
					favorites.insert(std::make_pair((*entry)->d_name, path));
				}
			}
		}
	}
	return self;
}

+ (FavoritesDataSource*)favoritesDataSource
{
	return [[[self alloc] initWithCxxPath:oak::application_t::support("Favorites")] autorelease];
}

- (NSString*)title
{
	return @"Open Favorite";
}

- (NSString*)filterString
{
	return [NSString stringWithCxxString:filterString];
}

- (IBAction)search:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(stringValue)]);
	NSString* objCStr = [[sender stringValue] lowercaseString] ?: @"";
	std::string const newFilterString = to_s(objCStr);
	if(newFilterString != filterString)
	{
		filterString = newFilterString;
		[[NSNotificationCenter defaultCenter] postNotificationName:FLDataSourceItemsDidChangeNotification object:self];
	}
}

- (NSArray*)items
{
	std::multimap< double, std::pair<std::string, std::string> > ranked;
	iterate(pair, favorites)
	{
		if(filterString == "")
		{
			ranked.insert(std::make_pair(ranked.size(), *pair));
		}
		else
		{
			double rank = oak::rank(filterString, pair->first);
			if(rank > 0)
				ranked.insert(std::make_pair(-rank, *pair));
		}
	}

	NSMutableArray* items = [NSMutableArray array];
	iterate(pair, ranked)
	{
		[items addObject:
			[NSDictionary dictionaryWithObjectsAndKeys:
				[NSString stringWithCxxString:pair->second.first],  @"title",
				[NSString stringWithCxxString:pair->second.second], @"path",
			nil]
		];
	}
	return items;
}

- (NSAttributedString*)displayStringForItem:(id)anItem
{
	std::string str = to_s((NSString*)[anItem objectForKey:@"title"]);
	return [[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:str]] autorelease];
}

- (NSAttributedString*)infoStringForItem:(id)anItem
{
	std::string str = path::with_tilde(to_s((NSString*)[anItem objectForKey:@"path"]));
	return [[[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:str]] autorelease];
}

- (void)dealloc
{
	[viewController release];
	[super dealloc];
}
@end
