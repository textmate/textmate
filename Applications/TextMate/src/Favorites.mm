#import "Favorites.h"
#import <OakFilterList/OakAbbreviations.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakScopeBarView.h>
#import <OakAppKit/OakSound.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <text/ranker.h>
#import <io/entries.h>
#import <text/case.h>
#import <text/ctype.h>
#import <io/path.h>
#import <ns/ns.h>
#import <kvdb/kvdb.h>

static NSString* const kUserDefaultsOpenProjectSourceIndex = @"openProjectSourceIndex";

static NSUInteger const kOakSourceIndexRecentProjects = 0;
static NSUInteger const kOakSourceIndexFavorites      = 1;

@interface FavoriteChooser ()
{
	std::multimap<std::string, std::string, text::less_t> favorites;
}
@property (nonatomic) NSInteger sourceIndex;
@end

@implementation FavoriteChooser
+ (instancetype)sharedInstance
{
	static id sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsOpenProjectSourceIndex : @0,
	}];
}

- (KVDB*)sharedProjectStateDB
{
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	return [KVDB sharedDBUsingFile:@"project-state.db" inDirectory:appSupport];
}

- (id)init
{
	if((self = [super init]))
	{
		_sourceIndex = NSNotFound;

		self.window.title = @"Open Favorite";
		self.window.frameAutosaveName = @"Open Favorite";
		self.tableView.allowsMultipleSelection = YES;

		OakScopeBarView* scopeBar = [OakScopeBarView new];
		scopeBar.labels = @[ @"Recent Projects", @"Favorites" ];

		NSDictionary* views = @{
			@"searchField"        : self.searchField,
			@"aboveScopeBarDark"  : OakCreateHorizontalLine([NSColor grayColor], [NSColor lightGrayColor]),
			@"aboveScopeBarLight" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.797 alpha:1.000], [NSColor colorWithCalibratedWhite:0.912 alpha:1.000]),
			@"scopeBar"           : scopeBar,
			@"topDivider"         : OakCreateHorizontalLine([NSColor darkGrayColor], [NSColor colorWithCalibratedWhite:0.551 alpha:1.000]),
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
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[aboveScopeBarDark(==aboveScopeBarLight)]|"          options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|"                             options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[scrollView(==topDivider,==bottomDivider)]|"         options:0 metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[statusTextField]-[itemCountTextField]-|"           options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[aboveScopeBarDark][aboveScopeBarLight]-(3)-[scopeBar]-(4)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:views]];

		self.sourceIndex = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsOpenProjectSourceIndex];
		[scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];
	}
	return self;
}

- (void)setSourceIndex:(NSInteger)newIndex
{
	if(_sourceIndex == newIndex)
		return;

	_sourceIndex = newIndex;
	[self loadItems:self];
	[[NSUserDefaults standardUserDefaults] setInteger:newIndex forKey:kUserDefaultsOpenProjectSourceIndex];
}

- (void)scanFavoritesDirectory:(id)sender
{
	favorites.clear();

	std::string const favoritesPath = oak::application_t::support("Favorites");
	for(auto const& entry : path::entries(favoritesPath))
	{
		if(entry->d_type == DT_LNK)
		{
			std::string const path = path::resolve(path::join(favoritesPath, entry->d_name));
			if(strncmp("[DIR] ", entry->d_name, 6) == 0)
			{
				for(auto const& subentry : path::entries(path))
				{
					if(subentry->d_type == DT_DIR)
						favorites.emplace(text::format("%s — %s", subentry->d_name, entry->d_name + 6), path::join(path, subentry->d_name));
				}
			}
			else
			{
				favorites.emplace(entry->d_name, path);
			}
		}
	}
}

- (void)loadItems:(id)sender
{
	if(_sourceIndex == kOakSourceIndexRecentProjects)
	{
		favorites.clear();
		for(id pair in [[self sharedProjectStateDB] allObjects])
		{
			if(access([pair[@"key"] fileSystemRepresentation], F_OK) == 0)
			{
				std::string const path = to_s((NSString*)pair[@"key"]);
				favorites.emplace(path::name(path), path);
			}
		}
	}
	else if(_sourceIndex == kOakSourceIndexFavorites)
	{
		[self scanFavoritesDirectory:self];
	}
	[self updateItems:self];
}

- (void)showWindow:(id)sender
{
	if(![self.window isVisible])
	{
		self.filterString = @"";
		[self loadItems:self];
		if([self.tableView numberOfRows])
			[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
	}
	[super showWindow:sender];
}

- (void)updateItems:(id)sender
{
	std::string const filter = to_s(self.filterString);

	std::vector<std::string> bindings;
	for(NSString* str in [[OakAbbreviations abbreviationsForName:@"OakFavoriteChooserBindings"] stringsForAbbreviation:self.filterString])
		bindings.push_back(to_s(str));

	std::multimap<double, NSDictionary*> ranked;
	for(auto const& pair : favorites)
	{
		if(filter == NULL_STR || filter == "")
		{
			ranked.emplace(ranked.size(), @{
				@"name" : [NSString stringWithCxxString:pair.first],
				@"info" : [NSString stringWithCxxString:path::with_tilde(pair.second)],
				@"path" : [NSString stringWithCxxString:pair.second],
			});
		}
		else
		{
			std::vector<std::pair<size_t, size_t>> ranges;
			double rank = oak::rank(filter, pair.first, &ranges);
			if(rank > 0)
			{
				size_t bindingIndex = std::find(bindings.begin(), bindings.end(), pair.second) - bindings.begin();
				if(bindingIndex != bindings.size())
						rank = -1.0 * (bindings.size() - bindingIndex);
				else	rank = -rank;

				ranked.emplace(rank, @{
					@"name" : CreateAttributedStringWithMarkedUpRanges(pair.first, ranges),
					@"info" : [NSString stringWithCxxString:path::with_tilde(pair.second)],
					@"path" : [NSString stringWithCxxString:pair.second],
				});
			}
		}
	}

	NSMutableArray* res = [NSMutableArray array];
	for(auto const& pair : ranked)
		[res addObject:pair.second];
	self.items = res;
}

- (void)updateStatusText:(id)sender
{
	if(self.tableView.selectedRow != -1)
	{
		NSDictionary* item = self.items[self.tableView.selectedRow];
		self.statusTextField.stringValue = [item objectForKey:@"info"];
	}
	else
	{
		self.statusTextField.stringValue = @"";
	}
}

- (void)accept:(id)sender
{
	if(self.filterString)
	{
		for(NSDictionary* item in self.selectedItems)
			[[OakAbbreviations abbreviationsForName:@"OakFavoriteChooserBindings"] learnAbbreviation:self.filterString forString:[item objectForKey:@"path"]];
	}
	[super accept:sender];
}

- (void)delete:(id)sender
{
	NSArray* items = self.selectedItems;
	if(!items.count)
		return;

	for(NSDictionary* item in items)
	{
		NSString* path = item[@"path"];
		if(self.sourceIndex == kOakSourceIndexRecentProjects)
		{
			KVDB* db = [self sharedProjectStateDB];
			[db removeObjectForKey:path];
		}
		else if(self.sourceIndex == kOakSourceIndexFavorites)
		{
			NSLog(@"%s %@", sel_getName(_cmd), path);
		}
	}
	[self loadItems:self];
	OakPlayUISound(OakSoundDidTrashItemUISound);
}

- (void)takeSourceIndexFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.sourceIndex = [sender tag];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.window.isKeyWindow)
	{
		[[aMenu addItemWithTitle:@"Recent Projects" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"1"] setTag:kOakSourceIndexRecentProjects];
		[[aMenu addItemWithTitle:@"Favorites" action:@selector(takeSourceIndexFrom:) keyEquivalent:@"2"] setTag:kOakSourceIndexFavorites];
	}
	else
	{
		[aMenu addItemWithTitle:@"No Sources" action:@selector(nop:) keyEquivalent:@""];
	}
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	BOOL activate = YES;
	if([item action] == @selector(takeSourceIndexFrom:))
		[item setState:[item tag] == self.sourceIndex ? NSOnState : NSOffState];
	return activate;
}
@end
