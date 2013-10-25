#import "OakFileBrowser.h"
#import "OakFSUtilities.h"
#import "ui/OFBHeaderView.h"
#import "ui/OFBOutlineView.h"
#import "ui/OFBPathInfoCell.h"
#import "ui/OFBActionsView.h"
#import "io/FSDataSource.h"
#import "io/FSSCMDataSource.h"
#import "io/FSItem.h"
#import "FSOutlineViewDelegate.h"
#import <Preferences/Keys.h>
#import <io/io.h>
#import <oak/oak.h>
#import <io/entries.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFileManager.h>
#import <OakAppKit/OakFinderLabelChooser.h>
#import <OakAppKit/OakOpenWithMenu.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakAppKit/OakZoomingIcon.h>
#import <OakAppKit/NSView Additions.h>
#import <OakSystem/application.h>
#import <bundles/bundles.h>
#import <document/document.h>
#import <document/collection.h>
#import <ns/ns.h>
#import <text/ctype.h>
#import <regexp/format_string.h>
#import <regexp/glob.h>
#import <settings/settings.h>

OAK_DEBUG_VAR(FileBrowser_Controller);

static NSString* DisplayName (NSURL* url, size_t numberOfParents = 0)
{
	NSString* res = nil;
	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		res = [[NSHost currentHost] localizedName];
	else if([[url scheme] isEqualToString:[kURLLocationBundles scheme]])
		res = @"Bundles";
	else // if([url isFileURL])
		res = [NSString stringWithCxxString:path::display_name([[url path] fileSystemRepresentation], numberOfParents)];
	return res ?: [url absoluteString] ?: @"«nil»";
}

static NSImage* IconImage (NSURL* url, NSSize size = NSMakeSize(16, 16))
{
	NSImage* iconImage = nil;
	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		iconImage = [NSImage imageNamed:NSImageNameComputer];
	else if([[url scheme] isEqualToString:[kURLLocationBundles scheme]])
		iconImage = [NSImage imageNamed:NSImageNameFolderSmart];
	else if([[url scheme] isEqualToString:@"scm"])
		iconImage = [NSImage imageNamed:NSImageNameFolderSmart];
	else // if([url isFileURL])
		iconImage = [OakFileIconImage fileIconImageWithPath:[url path] size:size];

	[iconImage setSize:size];
	return iconImage;
}

@interface OakFileBrowserView : NSView
@end

@implementation OakFileBrowserView
- (BOOL)accessibilityIsIgnored
{
	return NO;
}

- (NSArray*)accessibilityAttributeNames
{
	static NSArray* attributes = nil;
	if(!attributes)
	{
		NSSet* set = [NSSet setWithArray:[super accessibilityAttributeNames]];
		set = [set setByAddingObjectsFromArray:@[
			NSAccessibilityRoleAttribute,
			NSAccessibilityDescriptionAttribute,
		]];
		attributes = [set allObjects];
	}
	return attributes;
}

- (id)accessibilityAttributeValue:(NSString*)attribute
{
	if([attribute isEqualToString:NSAccessibilityRoleAttribute])
		return NSAccessibilityGroupRole;
	else if([attribute isEqualToString:NSAccessibilityDescriptionAttribute])
		return @"File Browser";
	else
		return [super accessibilityAttributeValue:attribute];
}
@end

@interface OakFileBrowser () <OFBOutlineViewMenuDelegate, NSMenuDelegate>
{
	OBJC_WATCH_LEAKS(OakFileBrowser);
	NSUInteger _historyIndex;
}
@property (nonatomic, readwrite)         OakFileBrowserView* view;
@property (nonatomic)                    OFBHeaderView* headerView;
@property (nonatomic)                    OFBOutlineView* outlineView;
@property (nonatomic)                    OFBActionsView* actionsView;

@property (nonatomic)                    FSOutlineViewDelegate* outlineViewDelegate;
@property (nonatomic)                    NSUInteger dataSourceOptions;

@property (nonatomic, readonly)          NSArray* selectedItems;
@property (nonatomic, readonly)          NSArray* selectedPaths;

@property (nonatomic)                    NSMutableArray* history;
@property (nonatomic)                    NSUInteger historyIndex;
@end

static bool is_binary (std::string const& path)
{
	if(path == NULL_STR)
		return false;

	settings_t const& settings = settings_for_path(path);
	if(settings.has(kSettingsBinaryKey))
		return path::glob_t(settings.get(kSettingsBinaryKey, "")).does_match(path);

	return false;
}

static NSMutableSet* SymmetricDifference (NSMutableSet* aSet, NSMutableSet* anotherSet)
{
	NSMutableSet* unionSet = [aSet mutableCopy];
	[unionSet unionSet:anotherSet];
	[anotherSet intersectSet:aSet];
	[unionSet minusSet:anotherSet];
	return unionSet;
}

@implementation OakFileBrowser
- (id)init
{
	if(self = [super init])
	{
		NSString* urlString = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsInitialFileBrowserURLKey];
		_url     = urlString ? [NSURL URLWithString:urlString] : kURLLocationHome;
		_history = [NSMutableArray arrayWithObject:@{ @"url" : _url }];

		BOOL foldersOnTop   = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFoldersOnTopKey];
		BOOL showExtensions = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFileExtensionsKey];

		_dataSourceOptions |= (foldersOnTop   ? kFSDataSourceOptionGroupsFirst   : 0);
		_dataSourceOptions |= (showExtensions ? kFSDataSourceOptionShowExtension : 0);

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	BOOL foldersOnTop   = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFoldersOnTopKey];
	BOOL showExtensions = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFileExtensionsKey];

	BOOL oldFoldersOnTop   = (_dataSourceOptions & kFSDataSourceOptionGroupsFirst) == kFSDataSourceOptionGroupsFirst;
	BOOL oldShowExtensions = (_dataSourceOptions & kFSDataSourceOptionShowExtension) == kFSDataSourceOptionShowExtension;

	if(foldersOnTop != oldFoldersOnTop || showExtensions != oldShowExtensions)
	{
		_dataSourceOptions ^= (foldersOnTop != oldFoldersOnTop     ? kFSDataSourceOptionGroupsFirst   : 0);
		_dataSourceOptions ^= (showExtensions != oldShowExtensions ? kFSDataSourceOptionShowExtension : 0);

		[self reload:self];
	}
}

- (void)createViews
{
	_outlineView = [[OFBOutlineView alloc] initWithFrame:NSZeroRect];
	_outlineView.focusRingType            = NSFocusRingTypeNone;
	_outlineView.allowsMultipleSelection  = YES;
	_outlineView.autoresizesOutlineColumn = NO;
	_outlineView.headerView               = nil;
	_outlineView.target                   = self;
	_outlineView.action                   = @selector(didSingleClickOutlineView:);
	_outlineView.doubleAction             = @selector(didDoubleClickOutlineView:);
	_outlineView.menuDelegate             = self;

	if([[[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsFileBrowserStyleKey] lowercaseString] isEqualToString:@"sourcelist"])
		_outlineView.renderAsSourceList = YES;

	[_outlineView setDraggingSourceOperationMask:NSDragOperationCopy|NSDragOperationMove|NSDragOperationLink forLocal:YES];
	[_outlineView setDraggingSourceOperationMask:NSDragOperationEvery forLocal:NO];
	[_outlineView registerForDraggedTypes:@[ NSFilenamesPboardType ]];

	NSScrollView* scrollView = [NSScrollView new];
	scrollView.hasVerticalScroller   = YES;
	scrollView.hasHorizontalScroller = NO;
	scrollView.borderType            = NSNoBorder;
	scrollView.documentView          = _outlineView;

	_headerView = [[OFBHeaderView alloc] initWithFrame:NSZeroRect];
	_headerView.goBackButton.target  = self;
	_headerView.goBackButton.action  = @selector(goBack:);
	_headerView.goBackButton.enabled = NO;
	_headerView.goForwardButton.target  = self;
	_headerView.goForwardButton.action  = @selector(goForward:);
	_headerView.goForwardButton.enabled = NO;

	[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:_headerView.folderPopUpButton];

	_actionsView = [[OFBActionsView alloc] initWithFrame:NSZeroRect];
	_actionsView.createButton.action    = @selector(newDocumentInDirectory:);
	_actionsView.reloadButton.target    = self;
	_actionsView.reloadButton.action    = @selector(reload:);
	_actionsView.searchButton.action    = @selector(orderFrontFindPanelForFileBrowser:);
	_actionsView.favoritesButton.target = self;
	_actionsView.favoritesButton.action = @selector(goToFavorites:);
	_actionsView.scmButton.target       = self;
	_actionsView.scmButton.action       = @selector(goToSCMDataSource:);

	_actionsView.actionsPopUpButton.menu.delegate = self;

	_view = [OakFileBrowserView new];

	NSCell* cell = [OFBPathInfoCell new];
	cell.lineBreakMode = NSLineBreakByTruncatingMiddle;
	[cell setEditable:YES];

	NSTableColumn* tableColumn = [NSTableColumn new];
	[tableColumn setDataCell:cell];
	[_outlineView addTableColumn:tableColumn];
	[_outlineView setOutlineTableColumn:tableColumn];
	[_outlineView sizeLastColumnToFit];

	_outlineViewDelegate = [FSOutlineViewDelegate new];
	_outlineViewDelegate.outlineView = _outlineView;

	NSDictionary* views = @{
		@"header"         : _headerView,
		@"headerDivider"  : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]),
		@"browser"        : scrollView,
		@"actionsDivider" : OakCreateHorizontalLine([NSColor colorWithCalibratedWhite:0.500 alpha:1], [NSColor colorWithCalibratedWhite:0.750 alpha:1]),
		@"actions"        : _actionsView,
	};

	for(NSView* view in [views allValues])
	{
		[view setTranslatesAutoresizingMaskIntoConstraints:NO];
		[_view addSubview:view];
	}

	[_view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|[browser(==header,==headerDivider,==actionsDivider,==actions)]|" options:0 metrics:nil views:views]];
	[_view addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|[header][headerDivider][browser][actionsDivider][actions]|"      options:NSLayoutFormatAlignAllLeft metrics:nil views:views]];
}

- (void)setupViewWithState:(NSDictionary*)fileBrowserState
{
	[self createViews];
	self.sessionState = fileBrowserState;
	self.url = _url;
}

- (NSRect)iconFrameForEntry:(id)anEntry
{
	NSInteger rowIndex = [_outlineView rowForItem:anEntry];
	if(rowIndex == -1)
		return NSZeroRect;
	NSRect r = [_outlineView frameOfCellAtColumn:0 row:rowIndex];
	r.origin.x += 7.0; // FIXME some hard-coded values here
	r.origin.y -= 1.0;
	r.size = NSMakeSize(16, 16);
	r = [_outlineView convertRect:r toView:nil];
	r.origin = [_outlineView.window convertBaseToScreen:r.origin];
	return r;
}

- (NSRect)iconFrameForURL:(NSURL*)aURL
{
	NSRange visibleRange = [_outlineView rowsInRect:[_outlineView visibleRect]];
	for(NSUInteger row = visibleRange.location; row < NSMaxRange(visibleRange); ++row)
	{
		FSItem* item = [_outlineView itemAtRow:row];
		if([item.url isEqualTo:aURL])
			return [self iconFrameForEntry:item];
	}
	return NSZeroRect;
}

// =============
// = Basic API =
// =============

- (void)setUrl:(NSURL*)aURL
{
	if(_outlineViewDelegate.dataSource && [_url isEqualTo:aURL])
		return;

	_url = aURL;
	_outlineViewDelegate.dataSource = DataSourceForURL(_url, _dataSourceOptions);

	NSMenu* folderPopUpMenu = _headerView.folderPopUpButton.menu;
	[folderPopUpMenu removeAllItems];

	NSMenuItem* menuItem = [folderPopUpMenu addItemWithTitle:_outlineViewDelegate.dataSource.rootItem.name action:@selector(takeURLFrom:) keyEquivalent:@""];
	menuItem.image = _outlineViewDelegate.dataSource.rootItem.icon;
	menuItem.image.size = NSMakeSize(16, 16);
	menuItem.target = self;
	menuItem.representedObject = _url;

	[_headerView.folderPopUpButton selectItem:menuItem];
}

- (void)folderPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	NSMenu* menu = _headerView.folderPopUpButton.menu;
	while([menu numberOfItems] > 1)
		[menu removeItemAtIndex:[menu numberOfItems]-1];

	for(NSURL* currentURL = ParentForURL(_url); currentURL; currentURL = ParentForURL(currentURL))
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:DisplayName(currentURL) action:@selector(takeURLFrom:) keyEquivalent:@""];
		menuItem.representedObject = currentURL;
		menuItem.image             = IconImage(currentURL);
		menuItem.target            = self;
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[[menu addItemWithTitle:@"Other…" action:@selector(orderFrontGoToFolder:) keyEquivalent:@""] setTarget:self];

	if(NSString* path = [[_url filePathURL] path])
	{
		[menu addItem:[NSMenuItem separatorItem]];
		[[menu addItemWithTitle:[NSString stringWithFormat:@"Use “%@” as Project Folder", DisplayName(_url)] action:@selector(takeProjectPathFrom:) keyEquivalent:@""] setRepresentedObject:path];
	}

}

- (void)goToURL:(NSURL*)aURL
{
	ASSERT(_historyIndex != NSNotFound);
	if([_url isEqualTo:aURL])
		return;

	if(_historyIndex + 1 < _history.count)
		[_history removeObjectsInRange:NSMakeRange(_historyIndex + 1, _history.count - (_historyIndex + 1))];
	[_history addObject:@{ @"url" : aURL }];
	self.historyIndex = self.historyIndex + 1;
}

- (void)selectURL:(NSURL*)aURL withParentURL:(NSURL*)parentURL
{
	BOOL alreadyVisible = NO;
	for(NSInteger row = 0; !alreadyVisible && row < [_outlineView numberOfRows]; ++row)
		alreadyVisible = alreadyVisible || [aURL isEqualTo:[[_outlineView itemAtRow:row] url]];

	if(!alreadyVisible)
	{
		BOOL isChild = NO;
		for(NSURL* currentURL = ParentForURL(aURL); currentURL && !isChild; currentURL = ParentForURL(currentURL))
		{
			if([parentURL isEqual:currentURL])
				isChild = YES;
		}
		[self goToURL:isChild ? parentURL : ParentForURL(aURL)];
	}

	[_outlineViewDelegate selectURLs:@[ aURL ] expandChildren:YES];
}

- (void)editURL:(NSURL*)aURL
{
	[_outlineView.window makeFirstResponder:_outlineView];
	[_outlineViewDelegate editURL:aURL];
}

- (NSString*)path
{
	NSURL* tmp = [[_url scheme] isEqualToString:@"scm"] ? ParentForURL(_url) : _url;
	return [tmp isFileURL] ? [tmp path] : nil;
}

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> env;

	std::vector<std::string> selection;
	for(NSString* aPath in self.selectedPaths)
		selection.push_back([aPath fileSystemRepresentation]);

	if(!selection.empty())
	{
		std::vector<std::string> quoted;
		iterate(path, selection)
			quoted.push_back(format_string::replace(*path, "\\A(?m:.*)\\z", "'${0/'/'\\''/}'"));
		env["TM_SELECTED_FILE"]  = selection.back();
		env["TM_SELECTED_FILES"] = text::join(quoted, " ");
	}

	return env;
}

- (BOOL)showExcludedItems
{
	return (_dataSourceOptions & kFSDataSourceOptionIncludeHidden) == kFSDataSourceOptionIncludeHidden;
}

- (void)setShowExcludedItems:(BOOL)flag
{
	if(self.showExcludedItems == flag)
		return;

	if(flag)
			_dataSourceOptions |= kFSDataSourceOptionIncludeHidden;
	else	_dataSourceOptions &= ~kFSDataSourceOptionIncludeHidden;

	[self reload:self];
}

// ===================
// = History support =
// ===================

- (void)setHistory:(NSArray*)newHistory
{
	_history      = [newHistory mutableCopy];
	_historyIndex = NSNotFound;

	_headerView.goBackButton.enabled    = NO;
	_headerView.goForwardButton.enabled = NO;
}

- (NSUInteger)historyIndex
{
	ASSERT(_historyIndex != NSNotFound);
	return _historyIndex;
}

- (void)setHistoryIndex:(NSUInteger)newIndex
{
	ASSERT_LT(newIndex, _history.count);
	if(_historyIndex == newIndex)
		return;

	[self syncHistoryState];
	_historyIndex = newIndex;

	NSDictionary* entry = _history[newIndex];
	[self setUrl:entry[@"url"]];
	[_outlineViewDelegate scrollToOffset:[entry[@"scrollOffset"] floatValue]];

	_headerView.goBackButton.enabled    = self.canGoBack;
	_headerView.goForwardButton.enabled = self.canGoForward;
}

- (void)syncHistoryState
{
	ASSERT(_url);
	if(_historyIndex == NSNotFound)
		return;

	_history[_historyIndex] = @{
		@"url"          : _url,
		@"scrollOffset" : @(NSMinY([_outlineView visibleRect]))
	};
}

- (NSDictionary*)sessionState
{
	[self syncHistoryState];

	NSMutableArray* history = [NSMutableArray array];
	for(NSDictionary* entry in self.history)
	{
		NSMutableDictionary* dict = [entry mutableCopy];
		dict[@"url"] = [dict[@"url"] absoluteString];
		if([dict[@"scrollOffset"] floatValue] == 0)
			[dict removeObjectForKey:@"scrollOffset"];
		[history addObject:dict];
	}

	return @{
		@"history"      : history,
		@"historyIndex" : @(self.historyIndex),
		@"selection"    : [self.selectedURLs valueForKey:@"absoluteString"],
	};
}

- (void)setSessionState:(NSDictionary*)newState
{
	NSMutableArray* newHistory = [NSMutableArray array];
	for(NSDictionary* entry in newState[@"history"])
	{
		if(NSString* urlString = entry[@"url"])
		{
			NSMutableDictionary* dict = [entry mutableCopy];
			dict[@"url"] = [NSURL URLWithString:urlString];
			[newHistory addObject:dict];
		}
	}

	if([newHistory count])
	{
		self.history      = newHistory;
		self.historyIndex = oak::cap<NSUInteger>(0, [newState[@"historyIndex"] unsignedIntValue], newHistory.count);

		NSMutableArray* selection = [NSMutableArray array];
		for(NSString* urlString in newState[@"selection"])
			[selection addObject:[NSURL URLWithString:urlString]];
		[_outlineViewDelegate selectURLs:selection expandChildren:NO];
	}
}

- (BOOL)canGoBack    { return self.historyIndex > 0; }
- (BOOL)canGoForward { return self.historyIndex < self.history.count-1; }

// ==================================
// = Externally provided item state =
// ==================================

- (NSArray*)openURLs
{
	return _outlineViewDelegate.openURLs;
}

- (void)setOpenURLs:(NSArray*)newOpenURLs
{
	if([_outlineViewDelegate.openURLs isEqualToArray:newOpenURLs])
		return;

	NSSet* symmetricDifference = SymmetricDifference([NSMutableSet setWithArray:_outlineViewDelegate.openURLs], [NSMutableSet setWithArray:newOpenURLs]);

	// make a note of files in view, with changed open state
	NSIndexSet* updateRows = [self indexSetforURLs:symmetricDifference];
	_outlineViewDelegate.openURLs = newOpenURLs;
	[_outlineView reloadDataForRowIndexes:updateRows columnIndexes:[NSIndexSet indexSetWithIndex:0]];
}

- (NSArray*)modifiedURLs
{
	return _outlineViewDelegate.modifiedURLs;
}

- (void)setModifiedURLs:(NSArray*)newModifiedURLs
{
	if([_outlineViewDelegate.modifiedURLs isEqualToArray:newModifiedURLs])
		return;

	NSSet* symmetricDifference = SymmetricDifference([NSMutableSet setWithArray:_outlineViewDelegate.modifiedURLs], [NSMutableSet setWithArray:newModifiedURLs]);

	// make a note of files in view, with changed modified state
	NSIndexSet* updateRows = [self indexSetforURLs:symmetricDifference];
	_outlineViewDelegate.modifiedURLs = newModifiedURLs;
	[_outlineView reloadDataForRowIndexes:updateRows columnIndexes:[NSIndexSet indexSetWithIndex:0]];
}

- (NSIndexSet*)indexSetforURLs:(NSSet*)urls
{
	// make a note of files in view, with changed state
	NSMutableIndexSet* updateRows = [NSMutableIndexSet indexSet];
	NSInteger len = [_outlineView numberOfRows];
	for(NSInteger rowIndex = 0; rowIndex < len; ++rowIndex)
	{
		NSURL* file = [[_outlineView itemAtRow:rowIndex] url];
		if([urls containsObject:file])
			[updateRows addIndex:rowIndex];
	}
	return updateRows;
}

// ===============================
// = Wrappers for selected items =
// ===============================

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	NSIndexSet* indexSet = [_outlineView selectedRowIndexes];
	for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[res addObject:[_outlineView itemAtRow:index]];
	return res;
}

- (NSArray*)selectedURLs
{
	return [self.selectedItems valueForKey:@"url"];
}

- (NSArray*)selectedPaths
{
	NSMutableArray* res = [NSMutableArray array];
	for(FSItem* item in self.selectedItems)
	{
		if([item.url isFileURL])
			[res addObject:[item.url path]];
	}
	return res;
}

// ============
// = Services =
// ============

+ (void)initialize
{
	[[NSApplication sharedApplication] registerServicesMenuSendTypes:@[ NSFilenamesPboardType, NSURLPboardType ] returnTypes:nil];
}

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	if(returnType == nil && ([sendType isEqualToString:NSFilenamesPboardType] || [sendType isEqualToString:NSStringPboardType]))
			return self;
	else	return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	return [_outlineView.dataSource outlineView:_outlineView writeItems:self.selectedItems toPasteboard:pboard];
}

// ================
// = Menu Actions =
// ================

- (void)editSelectedEntries:(id)sender { [_outlineView performEditSelectedRow:self]; }

- (void)duplicateSelectedEntries:(id)sender
{
	NSMutableArray* duplicatedURLs = [NSMutableArray array];
	for(NSURL* url in self.selectedURLs)
	{
		if([url isFileURL])
		{
			if(NSURL* res = [[OakFileManager sharedInstance] createDuplicateOfURL:url window:_view.window])
				[duplicatedURLs addObject:res];
		}
	}

	if([duplicatedURLs count] == 1)
		[_outlineViewDelegate editURL:[duplicatedURLs lastObject]];
	else if([duplicatedURLs count] > 1)
		[_outlineViewDelegate selectURLs:duplicatedURLs expandChildren:NO];
}

- (void)revealSelectedItem:(id)sender
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.target isFileURL])
		{
			[self goToURL:ParentForURL(item.target)];
			[_outlineViewDelegate selectURLs:@[ item.target ] expandChildren:NO];
			return;
		}
	}
}

- (void)showPackageContents:(id)sender
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.target isFileURL])
			return (void)[self goToURL:item.target];
	}

	for(FSItem* item in self.selectedItems)
	{
		if([item.target path] && path::is_directory([[item.target path] fileSystemRepresentation]))
			return (void)[self goToURL:[NSURL fileURLWithPath:[item.target path]]];
	}
}

- (void)showSelectedEntriesInFinder:(id)sender
{
	NSArray* urls = self.selectedURLs;
	if(urls.count == 0 && [_url isFileURL])
		urls = @[ _url ];
	[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:urls];
}

- (void)toggleQuickLookPreview:(id)sender
{
	if([QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible])
			[[QLPreviewPanel sharedPreviewPanel] orderOut:nil];
	else	[[QLPreviewPanel sharedPreviewPanel] makeKeyAndOrderFront:nil];
}

- (NSString*)directoryForNewItems
{
	NSMutableSet* folders = [NSMutableSet set];
	for(FSItem* item in self.selectedItems)
	{
		if(![item.url isFileURL])
			continue; // Perhaps we shouldn’t consider the selection if we encounter a non-file URL

		if(!item.leaf && [_outlineView isItemExpanded:item])
			[folders addObject:item.path];
		else if([_url isFileURL]) // TODO Test if parent folder is actually shown by current data source
			[folders addObject:[item.path stringByDeletingLastPathComponent]];
	}
	return [folders count] == 1 ? [folders anyObject] : [[_url filePathURL] path];
}

- (void)newFolder:(id)sender
{
	if(NSString* folder = [self directoryForNewItems])
	{
		if(NSURL* res = [[OakFileManager sharedInstance] createUntitledDirectoryAtURL:[NSURL fileURLWithPath:folder] window:_view.window])
			[self editURL:res];
	}
}

- (BOOL)canSelectRow:(NSUInteger)row
{
	return ![[_outlineView delegate] respondsToSelector:@selector(outlineView:isGroupItem:)] || ![[_outlineView delegate] outlineView:_outlineView isGroupItem:[_outlineView itemAtRow:row]];
}

- (void)delete:(id)anArgument
{
	NSIndexSet* indexSet = [_outlineView selectedRowIndexes];
	if([indexSet count] == 0)
		return;

	NSUInteger rowToSelect = [indexSet lastIndex];
	while(rowToSelect < [_outlineView numberOfRows] && ([indexSet containsIndex:rowToSelect] || ![self canSelectRow:rowToSelect]))
		++rowToSelect;

	if(rowToSelect == [_outlineView numberOfRows])
		do { --rowToSelect; } while(rowToSelect > 0 && ([indexSet containsIndex:rowToSelect]) || ![self canSelectRow:rowToSelect]);

	FSItem* itemToSelect = [indexSet containsIndex:rowToSelect] ? nil : [_outlineView itemAtRow:rowToSelect];

	for(NSURL* url in self.selectedURLs)
	{
		if([url isFileURL])
			[[OakFileManager sharedInstance] trashItemAtURL:url window:_view.window];
	}

	if(itemToSelect)
		[_outlineViewDelegate selectURLs:@[ itemToSelect.url ] expandChildren:NO];
}

- (void)changeColor:(OakFinderLabelChooser*)labelChooser
{
	NSInteger labelIndex = [labelChooser selectedIndex];
	for(NSString* aPath in self.selectedPaths)
	{
		if(!path::set_label_index([aPath fileSystemRepresentation], labelIndex))
			OakRunIOAlertPanel("Failed to change label color for “%s”.", [aPath fileSystemRepresentation]);
	}
}

- (void)addSelectedEntriesToFavorites:(id)sender
{
	std::string favFolder = oak::application_t::support("Favorites");
	if(!path::make_dir(favFolder))
		return (void)OakRunIOAlertPanel("Failed to create Favorites folder.");

	NSArray* urls = self.selectedURLs;
	if(![urls count] && [_url isFileURL])
		urls = @[ _url ];

	for(NSURL* url in urls)
	{
		std::string const dst = path::join(favFolder, path::name(to_s([url path])));
		[[OakFileManager sharedInstance] createSymbolicLinkAtURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]] withDestinationURL:url window:_view.window];
	}
}

- (void)removeSelectedEntriesFromFavorites:(id)sender
{
	for(NSURL* url in self.selectedURLs)
		[[OakFileManager sharedInstance] trashItemAtURL:url window:_view.window];
}

- (IBAction)cut:(id)sender
{
	NSPasteboard* pboard = [NSPasteboard generalPasteboard];
	[self writeSelectionToPasteboard:pboard types:nil];
	if([pboard availableTypeFromArray:@[ NSFilenamesPboardType ]])
	{
		NSArray* paths = [pboard propertyListForType:NSFilenamesPboardType];
		[pboard declareTypes:@[ NSFilenamesPboardType, @"OakFileBrowserOperation" ] owner:nil];
		[pboard setPropertyList:paths forType:NSFilenamesPboardType];
		[pboard setString:@"cut" forType:@"OakFileBrowserOperation"];;
	}
}

- (IBAction)copy:(id)sender
{
	[self writeSelectionToPasteboard:[NSPasteboard generalPasteboard] types:nil];
}

- (BOOL)canPaste
{
	return [_url isFileURL] && [[[NSPasteboard generalPasteboard] availableTypeFromArray:@[ NSFilenamesPboardType ]] isEqualToString:NSFilenamesPboardType];
}

- (IBAction)paste:(id)sender
{
	NSMutableArray* created = [NSMutableArray array];
	if(NSString* folder = [self directoryForNewItems])
	{
		NSPasteboard* pboard = [NSPasteboard generalPasteboard];
		BOOL cut = [[pboard availableTypeFromArray:@[ @"OakFileBrowserOperation" ]] isEqualToString:@"OakFileBrowserOperation"] && [[pboard stringForType:@"OakFileBrowserOperation"] isEqualToString:@"cut"];
		for(NSString* path in [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : @[ ])
		{
			std::string const dst = path::unique(path::join([folder fileSystemRepresentation], path::name([path fileSystemRepresentation])));
			NSURL* dstURL = [NSURL fileURLWithPath:[NSString stringWithCxxString:dst]];
			if(cut)
					[[OakFileManager sharedInstance] moveItemAtURL:[NSURL fileURLWithPath:path] toURL:dstURL window:_view.window];
			else	[[OakFileManager sharedInstance] copyItemAtURL:[NSURL fileURLWithPath:path] toURL:dstURL window:_view.window];
			[created addObject:[NSURL fileURLWithPath:[dstURL path]]]; // recreate to set ‘isDirectory’
		}
	}

	if([created count] > 0)
		[_outlineViewDelegate selectURLs:created expandChildren:NO];
}

- (void)executeBundleCommand:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s((NSString*)[sender representedObject])))
	{
		std::map<std::string, std::string> map = oak::basic_environment();
		map << [self variables] << item->bundle_variables();
		map = bundles::scope_variables(map);
		map = variables_for_path(map, to_s((NSString*)[self.selectedPaths firstObject]));
		document::run(parse_command(item), ng::buffer_t(), ng::ranges_t(), [self.selectedPaths count] == 1 ? document::create(map["TM_SELECTED_FILE"]) : document::document_ptr(), map);
	}
}

// ======================
// = Create Action Menu =
// ======================

- (void)updateMenu:(NSMenu*)aMenu
{
	NSUInteger countOfExistingItems = [aMenu numberOfItems];
	NSString* rootPath = [_url isFileURL] ? [_url path] : nil;

	NSArray* selectedItems = self.selectedItems;
	bool hasFileSelected = false;
	for(FSItem* item in selectedItems)
		hasFileSelected = hasFileSelected || [(item.url ?: item.target) isFileURL];

	if(hasFileSelected)
	{
		FSItem* selectedItem   = [selectedItems count] == 1 ? [selectedItems lastObject] : nil;
		NSString* selectedPath = [selectedItem.url isFileURL] ? selectedItem.path : nil;

		uint32_t flags = path::info(to_s(selectedPath));
		bool isSymlink     = (flags & (path::flag::symlink|path::flag::alias)) != 0;
		bool isPackage     = (flags & path::flag::package) != 0;
		bool isApplication = ((isSymlink && [selectedItem.target isFileURL] ? path::info(to_s(selectedItem.target.path)) : flags) & path::flag::application) != 0;

		[aMenu addItemWithTitle:@"Open" action:@selector(didDoubleClickOutlineView:) keyEquivalent:@""];
		if(!isApplication)
		{
			NSMenuItem* openWithMenuItem = [aMenu addItemWithTitle:@"Open With" action:@selector(revealSelectedItem:) keyEquivalent:@""];
			[OakOpenWithMenu addOpenWithMenuForPaths:[NSSet setWithArray:self.selectedPaths] toMenuItem:openWithMenuItem];
		}

		[aMenu addItem:[NSMenuItem separatorItem]];

		if(isSymlink)
		{
			[aMenu addItemWithTitle:@"Show Original" action:@selector(revealSelectedItem:) keyEquivalent:@""];
		}
		else
		{
			if(isPackage)
				[aMenu addItemWithTitle:@"Show Package Contents" action:@selector(showPackageContents:) keyEquivalent:@""];
			if(!rootPath && selectedPath)
				[aMenu addItemWithTitle:@"Show Enclosing Folder" action:@selector(revealSelectedItem:) keyEquivalent:@""];
		}
	}

	if(rootPath || hasFileSelected)
		[aMenu addItemWithTitle:@"Show in Finder" action:@selector(showSelectedEntriesInFinder:) keyEquivalent:@""];

	if(rootPath)
	{
		[aMenu addItem:[NSMenuItem separatorItem]];
		[[aMenu addItemWithTitle:@"New File"   action:@selector(newDocumentInDirectory:) keyEquivalent:@"n"] setKeyEquivalentModifierMask:NSCommandKeyMask|NSShiftKeyMask];
		[[aMenu addItemWithTitle:@"New Folder" action:@selector(newFolder:)              keyEquivalent:@"n"] setKeyEquivalentModifierMask:NSCommandKeyMask|NSControlKeyMask];
	}

	if(rootPath || hasFileSelected)
	{
		if(hasFileSelected || to_s(rootPath) != oak::application_t::support("Favorites"))
			[aMenu addItem:[NSMenuItem separatorItem]];

		if(hasFileSelected)
		{
			[aMenu addItemWithTitle:@"Rename"     action:@selector(editSelectedEntries:)      keyEquivalent:@""];
			[aMenu addItemWithTitle:@"Duplicate"  action:@selector(duplicateSelectedEntries:) keyEquivalent:@""];
			[aMenu addItemWithTitle:@"Quick Look" action:@selector(toggleQuickLookPreview:)   keyEquivalent:@""];
		}

		if(rootPath && to_s(rootPath) == oak::application_t::support("Favorites"))
		{
			if(hasFileSelected)
				[aMenu addItemWithTitle:@"Remove From Favorites" action:@selector(removeSelectedEntriesFromFavorites:) keyEquivalent:@""];
		}
		else
		{
			[aMenu addItemWithTitle:@"Add to Favorites" action:@selector(addSelectedEntriesToFavorites:) keyEquivalent:@""];
		}
	}

	if(hasFileSelected)
	{
		[aMenu addItem:[NSMenuItem separatorItem]];
		[aMenu addItemWithTitle:@"Move to Trash" action:@selector(delete:) keyEquivalent:@""];

		std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, "callback.file-browser.action-menu");
		if(!items.empty())
		{
			[aMenu addItem:[NSMenuItem separatorItem]];

			std::multimap<std::string, bundles::item_ptr, text::less_t> sorted;
			iterate(item, items)
				sorted.emplace((*item)->name(), *item);

			for(auto pair : sorted)
				[[aMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(executeBundleCommand:) keyEquivalent:@""] setRepresentedObject:[NSString stringWithCxxString:pair.second->uuid()]];
		}
	}

	if([selectedItems count])
	{
		[aMenu addItem:[NSMenuItem separatorItem]];
		[aMenu addItemWithTitle:@"Select None" action:@selector(deselectAll:) keyEquivalent:@"A"];
	}

	if(hasFileSelected || self.canPaste)
	{
		[aMenu addItem:[NSMenuItem separatorItem]];

		if(hasFileSelected)
		{
			[aMenu addItemWithTitle:@"Cut"  action:@selector(cut:)  keyEquivalent:@""];
			[aMenu addItemWithTitle:@"Copy" action:@selector(copy:) keyEquivalent:@""];
		}

		if(self.canPaste)
			[aMenu addItemWithTitle:@"Paste" action:@selector(paste:) keyEquivalent:@""];
	}

	if(hasFileSelected)
	{
		OakFinderLabelChooser* swatch = [[OakFinderLabelChooser alloc] initWithFrame:NSMakeRect(0, 0, 166, 55)];
		swatch.selectedIndex = [[selectedItems lastObject] labelIndex];
		swatch.action        = @selector(changeColor:);
		swatch.target        = self;
		swatch.font          = [aMenu font];

		[aMenu addItem:[NSMenuItem separatorItem]];
		[[aMenu addItemWithTitle:@"Color Swatch" action:@selector(nop:) keyEquivalent:@""] setView:swatch];
	}

	for(NSUInteger i = countOfExistingItems; i < [aMenu numberOfItems]; ++i)
	{
		NSMenuItem* item = [aMenu itemAtIndex:i];
		if(!item.target && [self respondsToSelector:item.action])
			[item setTarget:self];
	}

	if([_view.window.undoManager canUndo] || [_view.window.undoManager canRedo])
	{
		if(countOfExistingItems != [aMenu numberOfItems])
			[aMenu addItem:[NSMenuItem separatorItem]];

		[[aMenu addItemWithTitle:@"Undo" action:@selector(undo:) keyEquivalent:@""] setTarget:_view.window];
		[[aMenu addItemWithTitle:@"Redo" action:@selector(redo:) keyEquivalent:@""] setTarget:_view.window];
	}

	if(countOfExistingItems == [aMenu numberOfItems])
		[aMenu addItemWithTitle:@"No available actions" action:@selector(nop:) keyEquivalent:@""];
}

- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView
{
	NSMenu* menu = [NSMenu new];
	[self updateMenu:menu];
	return menu;
}

- (BOOL)menuHasKeyEquivalent:(NSMenu*)aMenu forEvent:(NSEvent*)anEvent target:(id*)anId action:(SEL*)aSEL
{
	static std::string const keys[] = { "@N", "^@n", "@A" };
	std::string const eventString = to_s(anEvent);
	if(!oak::contains(std::begin(keys), std::end(keys), eventString))
		return NO;

	[self updateMenu:aMenu];
	for(NSMenuItem* item in [aMenu itemArray])
	{
		if(eventString == ns::create_event_string(item.keyEquivalent, item.keyEquivalentModifierMask))
		{
			if(id target = [NSApp targetForAction:item.action])
			{
				*anId = target;
				*aSEL = item.action;
				return YES;
			}
		}
	}

	return NO;
}

- (void)menuNeedsUpdate:(NSMenu*)aMenu
{
	[aMenu removeAllItems];
	[aMenu addItemWithTitle:@"Dummy" action:@selector(nop:) keyEquivalent:@""];
	[self updateMenu:aMenu];
}

// ==================
// = Action methods =
// ==================

- (IBAction)didDoubleClickOutlineView:(id)sender
{
	NSArray* items = _outlineView.clickedRow != -1 ? @[ [_outlineView itemAtRow:_outlineView.clickedRow] ] : self.selectedItems;

	NSMutableArray* urlsToOpen     = [NSMutableArray array];
	NSMutableArray* itemsToAnimate = [NSMutableArray array];

	for(FSItem* item in items)
	{
		NSURL* itemURL = item.target ?: item.url;

		FSItemURLType type = item.urlType;
		if(type == FSItemURLTypeAlias)
		{
			FSItem* tmp = [FSItem itemWithURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path::resolve([itemURL.path fileSystemRepresentation])]]];
			type    = tmp.urlType;
			itemURL = tmp.target ?: tmp.url;
		}
		
		if(type == FSItemURLTypePackage && OakIsAlternateKeyOrMouseEvent())
			type = FSItemURLTypeFolder;
		else if(type == FSItemURLTypeFile && is_binary([itemURL.path fileSystemRepresentation]) && !OakIsAlternateKeyOrMouseEvent())
			type = FSItemURLTypePackage;

		switch(type)
		{
			case FSItemURLTypeFolder:
			case FSItemURLTypeUnknown:
				return [self goToURL:itemURL];
			break;

			case FSItemURLTypePackage:
				[itemsToAnimate addObject:item];
				[[NSWorkspace sharedWorkspace] openFile:itemURL.path];
			break;

			case FSItemURLTypeFile:
				[itemsToAnimate addObject:item];
				[urlsToOpen addObject:itemURL];
			break;
		}
	}

	for(FSItem* item in itemsToAnimate)
		[OakZoomingIcon zoomIcon:item.icon fromRect:[self iconFrameForEntry:item]];
	if([urlsToOpen count])
		[_delegate fileBrowser:self openURLs:urlsToOpen];
}

- (IBAction)didSingleClickOutlineView:(id)sender
{
	NSInteger row = [_outlineView clickedRow];
	NSInteger col = [_outlineView clickedColumn];
	col = row != -1 && col == -1 ? 0 : col; // Clicking a row which participates in multi-row selection causes clickedColumn to return -1 <rdar://10382268>
	NSCell* cell = [_outlineView preparedCellAtColumn:col row:row];
	NSUInteger hit = [cell hitTestForEvent:[NSApp currentEvent] inRect:[_outlineView frameOfCellAtColumn:col row:row] ofView:_outlineView];
	FSItem* item = [_outlineView itemAtRow:row];
	if((hit & OFBPathInfoCellHitRevealItem) && [item.url isFileURL])
		[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[ item.url ]];
	else if(hit & (OFBPathInfoCellHitOpenItem | OFBPathInfoCellHitRevealItem))
		[self didDoubleClickOutlineView:sender];
	else if(hit & OFBPathInfoCellHitCloseButton)
		[_delegate fileBrowser:self closeURL:item.url];
}

- (IBAction)reload:(id)sender
{
	CGFloat scrollOffset = NSMinY([_outlineView visibleRect]);
	_outlineViewDelegate.dataSource = DataSourceForURL(_url, _dataSourceOptions);
	[_outlineViewDelegate scrollToOffset:scrollOffset];
}

- (IBAction)deselectAll:(id)sender
{
	[_outlineView deselectAll:sender];
}

- (IBAction)toggleShowInvisibles:(id)sender
{
	self.showExcludedItems = !self.showExcludedItems;
}

- (IBAction)goToParentFolder:(id)sender   { [self selectURL:_url withParentURL:ParentForURL(_url)]; }
- (IBAction)goToComputer:(id)sender       { [self goToURL:kURLLocationComputer];  }
- (IBAction)goToHome:(id)sender           { [self goToURL:kURLLocationHome];      }
- (IBAction)goToDesktop:(id)sender        { [self goToURL:kURLLocationDesktop];   }

- (IBAction)goToFavorites:(id)sender
{
	if([_url isEqualTo:kURLLocationFavorites] && self.canGoBack)
			[self goBack:sender];
	else	[self goToURL:kURLLocationFavorites];
}

- (IBAction)goToSCMDataSource:(id)sender
{
	if([_url.scheme isEqualToString:@"scm"])
	{
		if(self.canGoBack)
				[self goBack:sender];
		else	[self goToParentFolder:sender];
	}
	else
	{
		for(NSURL* selectedURL in self.selectedURLs)
		{
			if([selectedURL isFileURL] && path::is_directory([[selectedURL path] fileSystemRepresentation]))
				return [self goToURL:[FSSCMDataSource scmURLWithPath:[selectedURL path]]];
		}
		[self goToURL:[FSSCMDataSource scmURLWithPath:[_url path]]];
	}
}

- (IBAction)goBack:(id)sender             { if(self.historyIndex > 0)                    self.historyIndex = self.historyIndex - 1; }
- (IBAction)goForward:(id)sender          { if(self.historyIndex < self.history.count-1) self.historyIndex = self.historyIndex + 1; }

- (IBAction)orderFrontGoToFolder:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setCanChooseFiles:NO];
	[panel setCanChooseDirectories:YES];
	[panel setAllowsMultipleSelection:NO];
	[panel setDirectoryURL:[_url isFileURL] ? _url : nil];
	[panel beginSheetModalForWindow:_view.window completionHandler:^(NSInteger result) {
		if(result == NSOKButton)
			[self goToURL:[[panel URLs] lastObject]];
	}];
}

- (void)takeURLFrom:(id)sender
{
	if(NSURL* url = [sender representedObject])
		[self goToURL:url];
}

// =========
// = Swipe =
// =========

- (BOOL)wantsScrollEventsForSwipeTrackingOnAxis:(NSEventGestureAxis)axis
{
	return axis == NSEventGestureAxisHorizontal;
}

- (void)scrollWheel:(NSEvent*)anEvent
{
	if(![NSEvent isSwipeTrackingFromScrollEventsEnabled] || [anEvent phase] == NSEventPhaseNone || fabsf([anEvent scrollingDeltaX]) <= fabsf([anEvent scrollingDeltaY]))
		return;

	[anEvent trackSwipeEventWithOptions:0 dampenAmountThresholdMin:(self.canGoForward ? -1 : 0) max:(self.canGoBack ? +1 : 0) usingHandler:^(CGFloat gestureAmount, NSEventPhase phase, BOOL isComplete, BOOL* stop) {
		if(phase == NSEventPhaseBegan)
		{
			// Setup animation overlay layers
		}

		// Update animation overlay to match gestureAmount

		if(phase == NSEventPhaseEnded)
		{
			if(gestureAmount > 0 && self.canGoBack)
				[self goBack:self];
			else if(gestureAmount < 0 && self.canGoForward)
				[self goForward:self];
		}

		if(isComplete)
		{
			// Tear down animation overlay here
		}
	}];
}

// ===================
// = Menu Validation =
// ===================

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	BOOL res = YES;
	static std::set<SEL> const requireSelection{ @selector(didDoubleClickOutlineView:), @selector(editSelectedEntries:), @selector(duplicateSelectedEntries:), @selector(cut:), @selector(copy:), @selector(delete:) };

	NSUInteger selectedFiles = 0;
	for(FSItem* item in self.selectedItems)
		selectedFiles += [item.url isFileURL] && path::exists([[item.url path] fileSystemRepresentation]) ? 1 : 0;

	if([item action] == @selector(goToParentFolder:))
		res = ParentForURL(_url) != nil;
	else if([item action] == @selector(goBack:))
		res = self.canGoBack;
	else if([item action] == @selector(goForward:))
		res = self.canGoForward;
	else if([item action] == @selector(newFolder:))
		res = [self directoryForNewItems] != nil;
	else if(selectedFiles == 0 && requireSelection.find([item action]) != requireSelection.end())
		res = NO;
	else if([item action] == @selector(paste:))
		res = self.canPaste;
	else if([item action] == @selector(editSelectedEntries:))
		res = selectedFiles == 1;
	else if([item action] == @selector(toggleShowInvisibles:))
		[item setState:self.showExcludedItems ? NSOnState : NSOffState];

	NSString* quickLookTitle = [QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible] ? @"Close Quick Look" : @"Quick Look%@";

	struct { NSString* format; SEL action; } const menuTitles[] =
	{
		{ @"Cut%@",                   @selector(cut:)                                },
		{ @"Copy%@",                  @selector(copy:)                               },
		{ quickLookTitle,             @selector(toggleQuickLookPreview:)             },
		{ @"Show%@ in Finder",        @selector(showSelectedEntriesInFinder:)        },
		{ @"Add%@ to Favorites",      @selector(addSelectedEntriesToFavorites:)      },
		{ @"Remove%@ From Favorites", @selector(removeSelectedEntriesFromFavorites:) },
	};

	for(auto info : menuTitles)
	{
		if(info.action == [item action])
		{
			NSString* items = @"";
			if(res)
			{
				switch(selectedFiles)
				{
					case 0:  items = [NSString stringWithFormat:@" “%@”", DisplayName(_url)]; break;
					case 1:  items = [NSString stringWithFormat:@" “%@”", ((FSItem*)[self.selectedItems lastObject]).name]; break;
					default: items = [NSString stringWithFormat:@" %ld Items", selectedFiles]; break;
				}
			}
			[item setTitle:[NSString stringWithFormat:info.format, items]];
		}
	}

	return res;
}
@end
