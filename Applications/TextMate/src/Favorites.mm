#import "Favorites.h"
#import <OakFilterList/OakAbbreviations.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakSystem/application.h>
#import <text/ranker.h>
#import <io/entries.h>
#import <text/case.h>
#import <text/ctype.h>
#import <io/path.h>
#import <ns/ns.h>

@interface FavoriteChooser ()
{
	std::multimap<std::string, std::string, text::less_t> favorites;
}
@end

@implementation FavoriteChooser
+ (instancetype)sharedInstance
{
	static id sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super init]))
	{
		self.window.title = @"Open Favorite";
		self.window.frameAutosaveName = @"Open Favorite";
		self.tableView.allowsMultipleSelection = YES;

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
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[statusTextField]-[itemCountTextField]-|"      options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(2)-[searchField]-(8)-[topDivider][scrollView(>=50)][bottomDivider]-(4)-[statusTextField]-(5)-|" options:0 metrics:nil views:views]];

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

		[self updateItems:self];
	}
	return self;
}

- (void)showWindow:(id)sender
{
	if(![self.window isVisible])
	{
		self.filterString = @"";
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
@end
