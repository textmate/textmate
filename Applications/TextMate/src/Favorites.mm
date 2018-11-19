#import "Favorites.h"
#import <OakFilterList/OakAbbreviations.h>
#import <OakAppKit/OakAppKit.h>
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
	NSMutableArray* _originalItems;
}
@property (nonatomic) NSInteger sourceIndex;
@property (nonatomic) NSArray* sourceListLabels;
@end

@implementation FavoriteChooser
+ (instancetype)sharedInstance
{
	static FavoriteChooser* sharedInstance = [self new];
	return sharedInstance;
}

+ (void)initialize
{
	[[NSUserDefaults standardUserDefaults] registerDefaults:@{
		kUserDefaultsOpenProjectSourceIndex: @0,
	}];
}

- (KVDB*)sharedProjectStateDB
{
	NSString* appSupport = [[NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES) objectAtIndex:0] stringByAppendingPathComponent:@"TextMate"];
	return [KVDB sharedDBUsingFile:@"RecentProjects.db" inDirectory:appSupport];
}

- (id)init
{
	if((self = [super init]))
	{
		_sourceIndex      = NSNotFound;
		_sourceListLabels = @[ @"Recent Projects", @"Favorites" ];

		self.window.title = @"Open Recent Project";
		self.tableView.allowsTypeSelect = NO;
		self.tableView.allowsMultipleSelection = YES;
		self.tableView.refusesFirstResponder = NO;
		self.tableView.rowHeight = 38;

		OakScopeBarView* scopeBar = [OakScopeBarView new];
		scopeBar.labels = self.sourceListLabels;

		NSDictionary* titlebarViews = @{
			@"searchField": self.searchField,
			@"dividerView": [self makeDividerView],
			@"scopeBar":    scopeBar,
		};

		NSView* titlebarView = [[NSView alloc] initWithFrame:NSZeroRect];
		OakAddAutoLayoutViewsToSuperview(titlebarViews.allValues, titlebarView);

		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[searchField]-(8)-|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[dividerView]|" options:0 metrics:nil views:titlebarViews]];
		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(8)-[scopeBar]-(>=8)-|" options:0 metrics:nil views:titlebarViews]];

		[titlebarView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-(4)-[searchField]-(8)-[dividerView(==1)]-(4)-[scopeBar]-(4)-|" options:0 metrics:nil views:titlebarViews]];
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

		OakSetupKeyViewLoop(@[ self.tableView, self.searchField, scopeBar ]);

		self.sourceIndex = [[NSUserDefaults standardUserDefaults] integerForKey:kUserDefaultsOpenProjectSourceIndex];
		[scopeBar bind:NSValueBinding toObject:self withKeyPath:@"sourceIndex" options:nil];
	}
	return self;
}

- (IBAction)selectNextTab:(id)sender     { self.sourceIndex = (self.sourceIndex + 1) % self.sourceListLabels.count; }
- (IBAction)selectPreviousTab:(id)sender { self.sourceIndex = (self.sourceIndex + self.sourceListLabels.count - 1) % self.sourceListLabels.count; }

- (NSView*)tableView:(NSTableView*)aTableView viewForTableColumn:(NSTableColumn*)aTableColumn row:(NSInteger)row
{
	NSTableCellView* res = [aTableView makeViewWithIdentifier:aTableColumn.identifier owner:self];
	if(!res)
	{
		NSImage* removeTemplateImage = [NSImage imageWithSize:NSMakeSize(8, 8) flipped:NO drawingHandler:^BOOL(NSRect dstRect){
			[[NSColor blackColor] set];
			NSRectFill(NSInsetRect(dstRect, 0, floor(NSHeight(dstRect)/2)-1));
			return YES;
		}];
		[removeTemplateImage setTemplate:YES];

		NSButton* removeButton = [NSButton new];
		removeButton.controlSize = NSControlSizeSmall;
		removeButton.refusesFirstResponder = YES;
		removeButton.bezelStyle = NSRoundRectBezelStyle;
		removeButton.buttonType = NSMomentaryPushInButton;
		removeButton.image      = removeTemplateImage;
		removeButton.target     = self;
		removeButton.action     = @selector(takeItemToRemoveFrom:);

		res = [[OakFileTableCellView alloc] initWithCloseButton:removeButton];
		res.identifier = aTableColumn.identifier;

		[removeButton bind:NSHiddenBinding toObject:res withKeyPath:@"objectValue.isRemoveDisabled" options:nil];
	}

	res.objectValue = self.items[row];
	return res;
}

- (void)setSourceIndex:(NSInteger)newIndex
{
	if(_sourceIndex == newIndex)
		return;

	_sourceIndex = newIndex;
	[self loadItems:self];
	[self updateItems:self];
	[[NSUserDefaults standardUserDefaults] setInteger:newIndex forKey:kUserDefaultsOpenProjectSourceIndex];
}

- (void)loadItems:(id)sender
{
	NSMutableArray* items = [NSMutableArray new];
	if(_sourceIndex == kOakSourceIndexRecentProjects)
	{
		std::vector<std::string> paths;
		for(id pair in [[[self sharedProjectStateDB] allObjects] sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"value.lastRecentlyUsed" ascending:NO], [NSSortDescriptor sortDescriptorWithKey:@"key.lastPathComponent" ascending:YES selector:@selector(localizedCompare:)] ]])
		{
			if(access([pair[@"key"] fileSystemRepresentation], F_OK) == 0)
				[items addObject:@{ @"path": pair[@"key"] }];
		}
	}
	else if(_sourceIndex == kOakSourceIndexFavorites)
	{
		std::string const favoritesPath = oak::application_t::support("Favorites");
		for(auto const& entry : path::entries(favoritesPath))
		{
			if(entry->d_type == DT_LNK)
			{
				std::string const path = path::resolve(path::join(favoritesPath, entry->d_name));
				if(strncmp("[DIR] ", entry->d_name, 6) == 0)
				{
					bool includeSymlinkName = path::name(path) != std::string(entry->d_name + 6);
					for(auto const& subentry : path::entries(path))
					{
						if(subentry->d_type == DT_DIR)
						{
							NSMutableDictionary* item = [NSMutableDictionary dictionaryWithDictionary:@{
								@"path":             [NSString stringWithCxxString:path::join(path, subentry->d_name)],
								@"isRemoveDisabled": @YES
							}];

							if(includeSymlinkName)
								item[@"name"] = [NSString stringWithFormat:@"%s — %s", subentry->d_name, entry->d_name + 6];

							[items addObject:item];
						}
					}
				}
				else
				{
					NSMutableDictionary* item = [NSMutableDictionary dictionaryWithDictionary:@{
						@"path": [NSString stringWithCxxString:path],
						@"link": [NSString stringWithCxxString:path::join(favoritesPath, entry->d_name)]
					}];

					if(path::name(path) != entry->d_name)
					{
						item[@"name"]   = [NSString stringWithCxxString:entry->d_name];
						item[@"folder"] = [item[@"path"] stringByAbbreviatingWithTildeInPath];
					}

					[items addObject:item];
				}
			}
		}
	}

	_originalItems = [NSMutableArray new];
	for(NSDictionary* item in items)
	{
		NSString* path = item[@"path"];

		NSImage* image = [NSWorkspace.sharedWorkspace iconForFile:path];
		image.size = NSMakeSize(32, 32);

		NSMutableDictionary* tmp = [item mutableCopy];
		[tmp addEntriesFromDictionary:@{
			@"icon":   image,
			@"name":   item[@"name"]   ?: [NSString stringWithCxxString:path::display_name(to_s(path))],
			@"folder": item[@"folder"] ?: [[path stringByDeletingLastPathComponent] stringByAbbreviatingWithTildeInPath],
			@"info":   [path stringByAbbreviatingWithTildeInPath]
		}];
		[_originalItems addObject:tmp];
	}

	if(_sourceIndex == kOakSourceIndexFavorites)
		_originalItems = [[_originalItems sortedArrayUsingDescriptors:@[ [NSSortDescriptor sortDescriptorWithKey:@"name" ascending:YES selector:@selector(localizedCompare:)] ]] mutableCopy];
}

- (void)showWindow:(id)sender
{
	if(![self.window isVisible])
	{
		self.filterString = @"";
		[self loadItems:self];
		[self updateItems:self];
		if([self.tableView numberOfRows])
			[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:0] byExtendingSelection:NO];
	}
	[super showWindow:sender];
}

- (void)updateItems:(id)sender
{
	NSArray* bindings = [[OakAbbreviations abbreviationsForName:@"OakFavoriteChooserBindings"] stringsForAbbreviation:self.filterString];
	std::string const filter = to_s([self.filterString decomposedStringWithCanonicalMapping]);

	std::multimap<double, NSDictionary*> ranked;
	for(NSDictionary* item in _originalItems)
	{
		NSString* name = item[@"name"];

		double rank = ranked.size();
		std::vector<std::pair<size_t, size_t>> ranges;
		if(filter != NULL_STR && filter != "")
		{
			rank = oak::rank(filter, to_s(name), &ranges);
			if(rank <= 0)
				continue;

			NSUInteger bindingIndex = [bindings indexOfObject:item[@"path"]];
			if(bindingIndex != NSNotFound)
					rank = -1.0 * (bindings.count - bindingIndex);
			else	rank = -rank;
		}

		NSMutableDictionary* entry = [item mutableCopy];
		entry[@"name"]   = CreateAttributedStringWithMarkedUpRanges(to_s(name), ranges, NSLineBreakByTruncatingTail);
		entry[@"folder"] = CreateAttributedStringWithMarkedUpRanges(to_s(item[@"folder"]), { }, NSLineBreakByTruncatingHead);
		ranked.emplace(rank, entry);
	}

	NSMutableArray* res = [NSMutableArray new];
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

	for(NSDictionary* item in self.selectedItems)
	{
		if(NSMutableDictionary* tmp = [[[self sharedProjectStateDB] valueForKey:item[@"path"]] mutableCopy])
		{
			tmp[@"lastRecentlyUsed"] = [NSDate date];
			[[self sharedProjectStateDB] setValue:tmp forKey:item[@"path"]];
		}
	}

	[super accept:sender];
}

- (NSUInteger)removeItemsAtIndexes:(NSIndexSet*)anIndexSet
{
	NSMutableArray* items = [self.items mutableCopy];
	anIndexSet = [anIndexSet indexesPassingTest:^BOOL(NSUInteger idx, BOOL* stop){
		return ![items[idx][@"isRemoveDisabled"] boolValue];
	}];

	for(NSDictionary* item in [items objectsAtIndexes:anIndexSet])
	{
		if(NSString* link = item[@"link"])
			[[NSFileManager defaultManager] trashItemAtURL:[NSURL fileURLWithPath:link] resultingItemURL:nil error:nil];
		else if(NSString* path = item[@"path"])
			[[self sharedProjectStateDB] removeObjectForKey:path];
	}

	[self loadItems:self]; // update originalItems
	return [super removeItemsAtIndexes:anIndexSet];
}

- (void)takeItemToRemoveFrom:(NSButton*)sender
{
	NSInteger row = [self.tableView rowForView:sender];
	if(row != -1)
		[self removeItemsAtIndexes:[NSIndexSet indexSetWithIndex:row]];
}

- (void)takeSourceIndexFrom:(id)sender
{
	if([sender respondsToSelector:@selector(tag)])
		self.sourceIndex = [sender tag];
}

- (void)updateShowTabMenu:(NSMenu*)aMenu
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

- (void)deleteForward:(id)sender
{
	NSUInteger itemsRemoved = [self removeItemsAtIndexes:[self.tableView selectedRowIndexes]];
	if(itemsRemoved == 0)
		NSBeep();
}

- (void)deleteBackward:(id)sender
{
	NSUInteger index = [[self.tableView selectedRowIndexes] firstIndex];
	NSUInteger itemsRemoved = [self removeItemsAtIndexes:[self.tableView selectedRowIndexes]];
	if(itemsRemoved == 0)
		NSBeep();
	else if(index && index != NSNotFound && self.tableView.numberOfRows)
		[self.tableView selectRowIndexes:[NSIndexSet indexSetWithIndex:index-1] byExtendingSelection:NO];
}

- (void)insertText:(id)aString
{
	self.filterString = aString;
	[self.window makeFirstResponder:self.searchField];
	NSText* fieldEditor = (NSText*)[self.window firstResponder];
	if([fieldEditor isKindOfClass:[NSText class]])
		[fieldEditor setSelectedRange:NSMakeRange([[fieldEditor string] length], 0)];
}

- (void)doCommandBySelector:(SEL)aSelector
{
	if([self respondsToSelector:aSelector])
	{
		[super doCommandBySelector:aSelector];
	}
	else
	{
		NSUInteger res = OakPerformTableViewActionFromSelector(self.tableView, aSelector);
		if(res == OakMoveAcceptReturn)
			[self accept:self];
		else if(res == OakMoveCancelReturn)
			[self cancel:self];
	}
}

- (void)keyDown:(NSEvent*)anEvent  { [self interpretKeyEvents:@[ anEvent ]]; }
- (void)insertTab:(id)sender       { [self.window selectNextKeyView:self]; }
- (void)insertBacktab:(id)sender   { [self.window selectPreviousKeyView:self]; }
@end
