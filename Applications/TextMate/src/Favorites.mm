#import "Favorites.h"
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <text/ranker.h>
#import <io/entries.h>
#import <text/case.h>
#import <io/path.h>
#import <ns/ns.h>

// ===================
// = View Controller =
// ===================

@interface FavoritesViewController : NSViewController
@property (nonatomic, retain) NSSearchField* searchField;
@property (nonatomic, retain) FavoritesDataSource* favoritesDataSource;
@end

@implementation FavoritesViewController
- (id)initWithFavoritesDataSource:(FavoritesDataSource*)aDataSource
{
	if(self = [super init])
	{
		self.favoritesDataSource          = aDataSource;

		self.searchField                  = [[NSSearchField alloc] initWithFrame:NSMakeRect(10, 10, 180, 22)];
		self.searchField.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		self.searchField.target           = self.favoritesDataSource;
		self.searchField.action           = @selector(search:);
		[self.searchField.cell setScrollable:YES];

		self.view                  = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 200, NSMaxY(self.searchField.frame) + 8)];
		self.view.autoresizingMask = NSViewWidthSizable|NSViewMinYMargin;
		[self.view addSubview:self.searchField];
	}
	return self;
}

- (void)dealloc
{
	self.searchField.target = nil;
	self.searchField.action = NULL;
}

- (void)setSearchFieldDelegate:(id)aDelegate
{
	self.searchField.delegate = aDelegate;
}
@end

// ===============
// = Data Source =
// ===============

@implementation FavoritesDataSource
{
	std::string favoritesPath;
	std::multimap<std::string, std::string, text::less_t> favorites;
	std::string filterString;
	FavoritesViewController* viewController;
}

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
							favorites.emplace(text::format("%s — %s", (*subentry)->d_name, (*entry)->d_name + 6), path::join(path, (*subentry)->d_name));
					}
				}
				else
				{
					favorites.emplace((*entry)->d_name, path);
				}
			}
		}
	}
	return self;
}

+ (FavoritesDataSource*)favoritesDataSource
{
	return [[self alloc] initWithCxxPath:oak::application_t::support("Favorites")];
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
			ranked.emplace(ranked.size(), *pair);
		}
		else
		{
			double rank = oak::rank(filterString, pair->first);
			if(rank > 0)
				ranked.emplace(-rank, *pair);
		}
	}

	NSMutableArray* items = [NSMutableArray array];
	iterate(pair, ranked)
	{
		[items addObject:@{
			@"title" : [NSString stringWithCxxString:pair->second.first],
			@"path"  : [NSString stringWithCxxString:pair->second.second],
		}];
	}
	return items;
}

- (NSAttributedString*)displayStringForItem:(id)anItem
{
	std::string str = to_s((NSString*)[anItem objectForKey:@"title"]);
	return [[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:str]];
}

- (NSAttributedString*)infoStringForItem:(id)anItem
{
	std::string str = path::with_tilde(to_s((NSString*)[anItem objectForKey:@"path"]));
	return [[NSAttributedString alloc] initWithString:[NSString stringWithCxxString:str]];
}
@end
