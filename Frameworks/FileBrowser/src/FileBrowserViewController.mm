#import "FileBrowserViewController.h"
#import "FileBrowserView.h"
#import "FileBrowserOutlineView.h"
#import "FileBrowserNotifications.h"
#import "FileItem.h"
#import "FileItemTableCellView.h"
#import "SCMManager.h"
#import "FSEventsManager.h"
#import "OFB/OFBHeaderView.h"
#import "OFB/OFBActionsView.h"
#import "OFB/OFBFinderTagsChooser.h"
#import <MenuBuilder/MenuBuilder.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakOpenWithMenu.h>
#import <OakAppKit/OakFinderTag.h>
#import <OakAppKit/OakZoomingIcon.h>
#import <OakFoundation/OakFoundation.h>
#import <OakCommand/OakCommand.h>
#import <Preferences/Keys.h>
#import <bundles/bundles.h>
#import <ns/ns.h>
#import <regexp/glob.h>
#import <settings/settings.h>
#import <text/ctype.h>
#import <text/utf8.h>

static bool is_binary (std::string const& path)
{
	if(path == NULL_STR)
		return false;

	settings_t const& settings = settings_for_path(path);
	if(settings.has(kSettingsBinaryKey))
		return path::glob_t(settings.get(kSettingsBinaryKey, "")).does_match(path);

	return false;
}

static NSMutableIndexSet* MutableLongestCommonSubsequence (NSArray* lhs, NSArray* rhs)
{
	NSInteger width  = lhs.count + 1;
	NSInteger height = rhs.count + 1;

	NSInteger* matrix = new NSInteger[width * height];

	for(NSInteger i = lhs.count; i >= 0; --i)
	{
		for(NSInteger j = rhs.count; j >= 0; --j)
		{
			if(i == lhs.count || j == rhs.count)
				matrix[width*i + j] = 0;
			else if([lhs[i] isEqual:rhs[j]])
				matrix[width*i + j] = matrix[width*(i+1) + j+1] + 1;
			else
				matrix[width*i + j] = MAX(matrix[width*(i+1) + j], matrix[width*i + j+1]);
		}
	}

	NSMutableIndexSet* res = [NSMutableIndexSet indexSet];
	for(NSInteger i = 0, j = 0; i < lhs.count && j < rhs.count; )
	{
		if([lhs[i] isEqual:rhs[j]])
		{
			[res addIndex:i];
			i++;
			j++;
		}
		else if(matrix[width*i + j+1] < matrix[width*(i+1) + j])
		{
			i++;
		}
		else
		{
			j++;
		}
	}

	delete [] matrix;

	return res;
}

@interface FileBrowserViewController () <NSMenuDelegate, NSOutlineViewDataSource, NSOutlineViewDelegate, NSTextFieldDelegate, QLPreviewPanelDataSource, OakUserDefaultsObserver>
{
	NSUndoManager* _fileBrowserUndoManager;
	NSArray<FileItem*>* _previewItems;

	NSMutableDictionary<NSURL*, id>* _fileItemObservers;

	NSMutableSet<NSURL*>* _loadingURLs;
	NSArray<void(^)()>* _loadingURLsCompletionHandlers;

	NSInteger _expandingChildrenCounter;
	NSInteger _collapsingChildrenCounter;
	NSInteger _nestedCollapsingChildrenCounter;
}
@property (nonatomic) BOOL canExpandSymbolicLinks;
@property (nonatomic) BOOL canExpandPackages;
@property (nonatomic) BOOL sortDirectoriesBeforeFiles;

@property (nonatomic) BOOL showExcludedItems;

@property (nonatomic, readonly) NSArray<FileItem*>* selectedItems;
@property (nonatomic, readonly) NSArray<FileItem*>* previewableItems;

@property (nonatomic) NSMutableSet<NSURL*>* expandedURLs;
@property (nonatomic) NSMutableSet<NSURL*>* selectedURLs;

- (void)expandURLs:(NSArray<NSURL*>*)expandURLs selectURLs:(NSArray<NSURL*>*)selectURLs;
- (NSRect)imageRectOfItem:(FileItem*)item;

- (void)updateDisambiguationSuffixInParent:(FileItem*)item;

// =============================
// = FileBrowserViewController =
// =============================

@property (nonatomic) NSMenuItem* currentLocationMenuItem;
@property (nonatomic) NSMutableArray<NSDictionary*>* history;
@property (nonatomic) NSInteger historyIndex;
@property (nonatomic) FileBrowserView* fileBrowserView;
@end

@implementation FileBrowserViewController
+ (NSSet*)keyPathsForValuesAffectingCanGoBack    { return [NSSet setWithObjects:@"historyIndex", nil]; }
+ (NSSet*)keyPathsForValuesAffectingCanGoForward { return [NSSet setWithObjects:@"historyIndex", nil]; }

- (instancetype)init
{
	if(self = [super init])
	{
		_fileItemObservers = [NSMutableDictionary dictionary];
		_loadingURLs       = [NSMutableSet set];

		_canExpandSymbolicLinks     = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingLinksKey];
		_canExpandPackages          = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingPackagesKey];
		_sortDirectoriesBeforeFiles = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFoldersOnTopKey];

		_expandedURLs = [NSMutableSet set];
		_selectedURLs = [NSMutableSet set];

		OakObserveUserDefaults(self);
	}
	return self;
}

- (void)dealloc
{
	for(id observer in _fileItemObservers.allValues)
		[FileItem removeObserver:observer];
	_fileItemObservers = nil;

	if(_currentLocationMenuItem)
	{
		[_currentLocationMenuItem unbind:NSTitleBinding];
		[_currentLocationMenuItem unbind:NSImageBinding];
	}

	if(OFBHeaderView* headerView = _fileBrowserView.headerView)
	{
		[headerView.goBackButton    unbind:NSEnabledBinding];
		[headerView.goForwardButton unbind:NSEnabledBinding];

		[NSNotificationCenter.defaultCenter removeObserver:self name:NSPopUpButtonWillPopUpNotification object:headerView.folderPopUpButton];
	}
}

- (void)userDefaultsDidChange:(id)sender
{
	self.canExpandSymbolicLinks     = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingLinksKey];
	self.canExpandPackages          = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingPackagesKey];
	self.sortDirectoriesBeforeFiles = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFoldersOnTopKey];
}

- (void)loadView
{
	self.view = self.fileBrowserView;
}

- (FileBrowserView*)fileBrowserView
{
	if(!_fileBrowserView)
	{
		_fileBrowserView = [[FileBrowserView alloc] initWithFrame:NSZeroRect];

		_currentLocationMenuItem = [[NSMenuItem alloc] initWithTitle:@"" action:@selector(takeURLFrom:) keyEquivalent:@""];
		_currentLocationMenuItem.target = self;
		[_currentLocationMenuItem bind:NSTitleBinding toObject:self withKeyPath:@"fileItem.displayName" options:nil];
		[_currentLocationMenuItem bind:NSImageBinding toObject:self withKeyPath:@"fileItem.image" options:nil];

		NSOutlineView* outlineView = _fileBrowserView.outlineView;
		outlineView.dataSource   = self;
		outlineView.delegate     = self;
		outlineView.target       = self;
		outlineView.action       = @selector(didSingleClickOutlineView:);
		outlineView.doubleAction = @selector(didDoubleClickOutlineView:);

		outlineView.menu = [[NSMenu alloc] init];
		outlineView.menu.delegate = self;

		OFBHeaderView* headerView = _fileBrowserView.headerView;
		headerView.goBackButton.target  = self;
		headerView.goBackButton.action  = @selector(goBack:);
		headerView.goBackButton.enabled = NO;

		headerView.goForwardButton.target  = self;
		headerView.goForwardButton.action  = @selector(goForward:);
		headerView.goForwardButton.enabled = NO;

		[headerView.goBackButton    bind:NSEnabledBinding toObject:self withKeyPath:@"canGoBack"    options:nil];
		[headerView.goForwardButton bind:NSEnabledBinding toObject:self withKeyPath:@"canGoForward" options:nil];

		NSMenu* folderPopUpMenu = headerView.folderPopUpButton.menu;
		[folderPopUpMenu removeAllItems];
		[folderPopUpMenu addItem:_currentLocationMenuItem];
		[headerView.folderPopUpButton selectItem:_currentLocationMenuItem];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(folderPopUpButtonWillPopUp:) name:NSPopUpButtonWillPopUpNotification object:headerView.folderPopUpButton];

		OFBActionsView* actionsView = _fileBrowserView.actionsView;

		actionsView.createButton.action    = @selector(newDocumentInDirectory:);
		actionsView.reloadButton.target    = self;
		actionsView.reloadButton.action    = @selector(reload:);
		actionsView.searchButton.action    = @selector(orderFrontFindPanelForFileBrowser:);
		actionsView.favoritesButton.target = self;
		actionsView.favoritesButton.action = @selector(goToFavorites:);
		actionsView.scmButton.target       = self;
		actionsView.scmButton.action       = @selector(goToSCMDataSource:);

		actionsView.actionsPopUpButton.menu.delegate = self;
	}
	return _fileBrowserView;
}

- (void)toggleShowInvisibles:(id)sender
{
	self.showExcludedItems = !self.showExcludedItems;
}

- (NSView*)headerView                { return self.fileBrowserView.headerView;              }
- (NSOutlineView*)outlineView        { return self.fileBrowserView.outlineView;             }
- (NSString*)path                    { return self.URL.filePathURL.path;                    }
- (NSArray<NSURL*>*)selectedFileURLs { return [[self.selectedItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"URL.isFileURL == YES"]] valueForKeyPath:@"URL"]; }

- (BOOL)canGoBack                    { return _historyIndex > 0; }
- (BOOL)canGoForward                 { return _historyIndex+1 < self.history.count; }

- (void)goBack:(id)sender            { self.historyIndex = _historyIndex - 1; }
- (void)goForward:(id)sender         { self.historyIndex = _historyIndex + 1; }

- (NSMutableArray<NSDictionary*>*)history
{
	if(!_history)
		_history = [NSMutableArray array];
	return _history;
}

- (void)setHistoryIndex:(NSInteger)index
{
	_historyIndex = index;
	self.URL = self.history[index][@"url"];
}

- (void)addHistoryURL:(NSURL*)url
{
	if(_historyIndex + 1 < self.history.count)
		[self.history removeObjectsInRange:NSMakeRange(_historyIndex + 1, self.history.count - (_historyIndex + 1))];

	if(NSDictionary* dict = _history.lastObject)
	{
		_history[_history.count-1] = @{
			@"url":          dict[@"url"],
			@"scrollOffset": @(NSMinY(self.outlineView.visibleRect)),
		};
	}

	[self.history addObject:@{ @"url": url }];
	self.historyIndex = self.history.count-1;
}

- (void)goToURL:(NSURL*)url
{
	if(url && ![self.URL isEqual:url])
		[self addHistoryURL:url];
}

- (void)goToComputer:(id)sender      { [self goToURL:kURLLocationComputer]; }
- (void)goToHome:(id)sender          { [self goToURL:[NSURL fileURLWithPath:NSHomeDirectory()]]; }
- (void)goToDesktop:(id)sender       { [self goToURL:[NSFileManager.defaultManager URLForDirectory:NSDesktopDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:nil]]; }

- (void)goToFavorites:(id)sender
{
	if(![self.URL isEqual:kURLLocationFavorites])
		[self goToURL:kURLLocationFavorites];
	else if(self.canGoBack)
		[self goBack:sender];
}

- (void)goToSCMDataSource:(id)sender
{
	NSURL* url = self.URL;
	if([url.scheme isEqualToString:@"file"])
	{
		SCMRepository* repository = [SCMManager.sharedInstance repositoryAtURL:url];
		if(repository && repository.enabled)
		{
			[self goToURL:[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/", [url.path stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLPathAllowedCharacterSet]]]];
		}
		else
		{
			NSAlert* alert = [[NSAlert alloc] init];

			if(repository)
			{
				alert.messageText     = [NSString stringWithFormat:@"Version control is disabled for “%@”.", self.fileItem.localizedName];
				alert.informativeText = @"For performance reasons TextMate will not monitor version control information for this folder.";
			}
			else
			{
				alert.messageText     = [NSString stringWithFormat:@"Version control is not available for “%@”.", self.fileItem.localizedName];
				alert.informativeText = @"You need to initialize the folder using your favorite version control system before TextMate can show you status.";
			}

			[alert addButtonWithTitle:@"OK"];
			[alert beginSheetModalForWindow:self.view.window completionHandler:^(NSModalResponse response){ }];
		}
	}
	else if([url.scheme isEqualToString:@"scm"])
	{
		if(self.canGoBack)
			[self goBack:self];
		else if(NSURL* parentURL = self.fileItem.parentURL)
			[self goToURL:parentURL];
	}
	else
	{
		NSBeep();
	}
}

- (void)goToParentFolder:(id)sender
{
	if(NSURL* url = self.fileItem.parentURL)
	{
		NSURL* cameFromURL = self.URL;
		[self goToURL:url];
		[self expandURLs:nil selectURLs:@[ cameFromURL ]];
	}
}

- (void)takeURLFrom:(id)sender
{
	[self goToURL:[sender representedObject]];
}

- (void)folderPopUpButtonWillPopUp:(NSNotification*)aNotification
{
	NSMenu* menu = self.fileBrowserView.headerView.folderPopUpButton.menu;
	while(menu.numberOfItems > 1)
		[menu removeItemAtIndex:menu.numberOfItems-1];

	FileItem* fileItem = self.fileItem;
	while(fileItem = [FileItem fileItemWithURL:fileItem.parentURL])
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:fileItem.localizedName action:@selector(takeURLFrom:) keyEquivalent:@""];
		menuItem.representedObject = fileItem.resolvedURL;
		menuItem.image             = fileItem.image;
		menuItem.target            = self;
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[[menu addItemWithTitle:@"Other…" action:@selector(orderFrontGoToFolder:) keyEquivalent:@""] setTarget:self];

	FileItem* item = self.fileItem;
	if(NSURL* url = item.URL.filePathURL)
	{
		[menu addItem:[NSMenuItem separatorItem]];
		[[menu addItemWithTitle:[NSString stringWithFormat:@"Use “%@” as Project Folder", item.localizedName] action:@selector(takeProjectPathFrom:) keyEquivalent:@""] setRepresentedObject:url.path];
	}
}

- (void)openItems:(NSArray<FileItem*>*)items animate:(BOOL)animateFlag
{
	NSMutableArray<FileItem*>* itemsToOpen              = [NSMutableArray array];
	NSMutableArray<FileItem*>* itemsToOpenInTextMate    = [NSMutableArray array];
	NSMutableArray<FileItem*>* itemsToShowInFinder      = [NSMutableArray array];
	NSMutableArray<FileItem*>* itemsToShowInFileBrowser = [NSMutableArray array];

	NSEventType eventType           = NSApp.currentEvent.type;
	NSEventModifierFlags eventFlags = NSApp.currentEvent.modifierFlags & (NSEventModifierFlagControl|NSEventModifierFlagOption|NSEventModifierFlagShift|NSEventModifierFlagCommand);
	BOOL isMouseEvent               = eventType == NSEventTypeLeftMouseUp || eventType == NSEventTypeOtherMouseUp || eventType == NSEventTypeOtherMouseUp;
	BOOL commandKeyDown             = isMouseEvent && eventFlags == NSEventModifierFlagCommand;
	BOOL optionKeyDown              = isMouseEvent && eventFlags == NSEventModifierFlagOption;
	BOOL treatPackageAsDirectory    = [NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsAllowExpandingPackagesKey];

	for(FileItem* item in items)
	{
		if(commandKeyDown)
			[itemsToShowInFinder addObject:item];
		else if(item.isDirectory && (treatPackageAsDirectory || !item.isPackage) || item.isLinkToDirectory && (treatPackageAsDirectory || !item.isLinkToPackage) || optionKeyDown && (item.isPackage || item.isLinkToDirectory))
			[itemsToShowInFileBrowser addObject:item];
		else if(item.isPackage || item.isLinkToPackage || item.URL.isFileURL && is_binary(item.URL.fileSystemRepresentation))
			[itemsToOpen addObject:item];
		else
			[itemsToOpenInTextMate addObject:item];
	}

	if(itemsToShowInFileBrowser.count > 0)
		return [self goToURL:itemsToShowInFileBrowser.firstObject.resolvedURL];

	if(animateFlag && ![NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFileBrowserOpenAnimationDisabled])
	{
		for(NSArray* items in @[ itemsToOpen, itemsToOpenInTextMate ])
		{
			for(FileItem* item in items)
				[OakZoomingIcon zoomIcon:item.image fromRect:[self imageRectOfItem:item]];
		}
	}

	if(itemsToShowInFinder.count > 0)
		[NSWorkspace.sharedWorkspace activateFileViewerSelectingURLs:[itemsToShowInFinder valueForKeyPath:@"URL"]];

	for(FileItem* item in itemsToOpen)
		[NSWorkspace.sharedWorkspace openFile:item.resolvedURL.path];

	if(itemsToOpenInTextMate.count > 0)
		[self.delegate fileBrowser:self openURLs:[itemsToOpenInTextMate valueForKeyPath:@"URL"]];
}

- (void)didSingleClickOutlineView:(id)sender
{
	if(NSEvent.modifierFlags & (NSEventModifierFlagControl|NSEventModifierFlagShift|NSEventModifierFlagCommand))
		return;

	if([NSUserDefaults.standardUserDefaults boolForKey:kUserDefaultsFileBrowserSingleClickToOpenKey])
	{
		FileItem* item = [self.outlineView itemAtRow:self.outlineView.clickedRow];
		if(item && !item.isDirectory && !item.isLinkToDirectory && !item.isPackage && !item.isLinkToPackage && !item.isApplication)
			[self openItems:@[ item ] animate:NO];
	}
}

- (void)didDoubleClickOutlineView:(id)sender
{
	[self openItems:self.selectedItems animate:YES];
}

// ===============
// = Action Menu =
// ===============

- (void)updateMenu:(NSMenu*)menu
{
	NSInteger kRequiresSelectionTag = 1;

	NSMenuItem* openWithMenuItem;
	NSMenuItem* insertBundleItemsMenuItem;
	NSMenuItem* finderTagsMenuItem;

	MBMenu const items = {
		{ @"Open",                    @selector(openSelectedItems:)                            },
		{ @"Open With",               @selector(openWithMenuAction:), .ref = &openWithMenuItem },
		{ /* -------- */ },
		{ @"Show Original",           @selector(showOriginal:)                },
		{ @"Show Enclosing Folder",   @selector(showEnclosingFolder:)         },
		{ @"Show Package Contents",   @selector(showPackageContents:)         },
		{ @"Show in Finder",          @selector(showSelectedEntriesInFinder:) },
		{ /* -------- */ },
		{ @"New File",                @selector(newDocumentInDirectory:), @"n", NSEventModifierFlagCommand|NSEventModifierFlagControl },
		{ @"New Folder",              @selector(newFolder:),              @"n", NSEventModifierFlagCommand|NSEventModifierFlagShift   },
		{ /* -------- */ },
		{ @"Rename",                  @selector(editSelectedEntries:)                },
		{ @"Duplicate",               @selector(duplicateSelectedEntries:)           },
		{ @"Quick Look",              @selector(toggleQuickLookPreview:)             },
		{ @"Add to Favorites",        @selector(addSelectedEntriesToFavorites:)      },
		{ @"Remove From Favorites",   @selector(removeSelectedEntriesFromFavorites:) },
		{ /* -------- */ },
		{ @"Move to Trash",           @selector(delete:), .key = NSBackspaceCharacter },
		{ /* -------- */ .ref = &insertBundleItemsMenuItem },
		{ /* -------- */ },
		{ @"Copy",                    @selector(copy:)                                                                            },
		{ @"Copy as Pathname",        @selector(copyAsPathname:),      @"",  NSEventModifierFlagOption, .alternate = YES, .tag = kRequiresSelectionTag },
		{ @"Paste",                   @selector(paste:),               @"v"                                                       },
		{ @"Move Items Here",         @selector(pasteNext:),           @"v", NSEventModifierFlagCommand|NSEventModifierFlagOption },
		{ @"Create Link to Items",    @selector(createLinkToPasteboardItems:) },
		{ /* -------- */ },
		{ @"Finder Tag", .ref = &finderTagsMenuItem,   .tag = kRequiresSelectionTag },
		{ /* -------- */ },
		{ @"Undo",                    @selector(undo:),                @"z"                                                       },
		{ @"Redo",                    @selector(redo:),                @"z", NSEventModifierFlagCommand|NSEventModifierFlagShift  },
		{ /* -------- */ },
	};

	MBCreateMenu(items, menu);

	std::map<SEL, std::string> inactiveKeyEquivalents = {
		{ @selector(openSelectedItems:),        "@" + utf8::to_s(NSDownArrowFunctionKey) },
		{ @selector(editSelectedEntries:),      "" + utf8::to_s(NSCarriageReturnCharacter) },
		{ @selector(duplicateSelectedEntries:), "@d" },
		{ @selector(toggleQuickLookPreview:),   " " },
		{ @selector(copy:),                     "@c" },
		{ @selector(copyAsPathname:),           "~@c" },
	};

	for(NSMenuItem* menuItem in menu.itemArray)
	{
		auto it = inactiveKeyEquivalents.find(menuItem.action);
		if(it != inactiveKeyEquivalents.end())
			[menuItem setInactiveKeyEquivalentCxxString:it->second];
	}

	if(self.previewableItems.count == 0)
	{
		NSInteger i = [menu indexOfItemWithTag:kRequiresSelectionTag];
		while(i != -1)
		{
			[menu removeItemAtIndex:i];
			i = [menu indexOfItemWithTag:kRequiresSelectionTag];
		}
	}
	else
	{
		NSArray<OakFinderTag*>* allTags = [self.selectedItems valueForKeyPath:@"@unionOfArrays.finderTags"];
		NSCountedSet* finderTagsCountedSet = [[NSCountedSet alloc] initWithArray:allTags];

		NSMutableArray<OakFinderTag*>* removeFinderTags = [NSMutableArray array];
		for(OakFinderTag* tag in finderTagsCountedSet)
		{
			if([finderTagsCountedSet countForObject:tag] == self.selectedItems.count)
				[removeFinderTags addObject:tag];
		}

		OFBFinderTagsChooser* chooser = [OFBFinderTagsChooser finderTagsChooserWithSelectedTags:finderTagsCountedSet.objectEnumerator.allObjects andSelectedTagsToRemove:[removeFinderTags copy] forMenu:menu];
		chooser.action               = @selector(didChangeFinderTag:);
		chooser.target               = self;

		finderTagsMenuItem.view = chooser;

		// ================
		// = Bundle Items =
		// ================

		std::multimap<std::string, bundles::item_ptr, text::less_t> sorted;
		for(auto const& item : bundles::query(bundles::kFieldSemanticClass, "callback.file-browser.action-menu"))
			sorted.emplace(item->name(), item);

		NSInteger i = [menu indexOfItem:insertBundleItemsMenuItem];
		for(auto pair : sorted)
		{
			NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:to_ns(pair.first) action:@selector(executeBundleCommand:) keyEquivalent:@""];
			item.representedObject = to_ns(pair.second->uuid());
			[menu insertItem:item atIndex:++i];
		}
	}

	for(NSMenuItem* menuItem in menu.itemArray)
	{
		if(!menuItem.target && menuItem.action && [self respondsToSelector:menuItem.action])
			menuItem.target = self;
	}

	[OakOpenWithMenu addOpenWithMenuForPaths:[NSSet setWithArray:[self.previewableItems valueForKeyPath:@"resolvedURL.path"]] toMenuItem:openWithMenuItem];
}

- (void)menuNeedsUpdate:(NSMenu*)menu
{
	[menu removeAllItems];
	[self updateMenu:menu];
}

// ==================
// = Action Methods =
// ==================

- (void)openSelectedItems:(id)sender
{
	[self openItems:self.selectedItems animate:YES];
}

- (void)openWithMenuAction:(id)sender
{
	// This action only exist to have validateMenuItem: called for the Open With menu
}

- (void)showOriginal:(id)sender
{
	NSURL* resolvedURL = self.selectedItems.firstObject.resolvedURL;
	NSURL* parentURL;
	if([resolvedURL getResourceValue:&parentURL forKey:NSURLParentDirectoryURLKey error:nil])
	{
		[self goToURL:parentURL];
		[self expandURLs:nil selectURLs:@[ resolvedURL ]];
	}
}

- (void)showEnclosingFolder:(id)sender
{
	NSURL* url = self.selectedItems.firstObject.URL;
	if(NSURL* enclosingFolder = url.URLByDeletingLastPathComponent)
	{
		[self goToURL:enclosingFolder];
		[self expandURLs:nil selectURLs:@[ url ]];
	}
}

- (void)showPackageContents:(id)sender
{
	[self goToURL:self.previewableItems.firstObject.resolvedURL];
}

- (void)showSelectedEntriesInFinder:(id)sender
{
	[NSWorkspace.sharedWorkspace activateFileViewerSelectingURLs:[self.previewableItems valueForKeyPath:@"resolvedURL"]];
}

- (NSURL*)newFile:(id)sender
{
	NSURL* directoryURL = self.directoryURLForNewItems;
	if(!directoryURL)
		return nil;

	NSString* pathExtension = @"txt";

	std::string fileType = settings_for_path(NULL_STR, "attr.untitled", directoryURL.fileSystemRepresentation).get(kSettingsFileTypeKey, "text.plain");
	for(auto item : bundles::query(bundles::kFieldGrammarScope, fileType))
	{
		if(NSString* ext = to_ns(item->value_for_field(bundles::kFieldGrammarExtension)))
			pathExtension = ext;
	}

	NSURL* newFileURL = [[directoryURL URLByAppendingPathComponent:@"untitled" isDirectory:NO] URLByAppendingPathExtension:pathExtension];
	NSArray<NSURL*>* urls = [self performOperation:FBOperationNewFile sourceURLs:nil destinationURLs:@[ newFileURL ] unique:YES select:YES];
	if(urls.count == 1 && self.outlineView.numberOfSelectedRows == 1)
	{
		FileItem* newItem = [self.outlineView itemAtRow:self.outlineView.selectedRow];
		if([newItem.URL isEqual:urls.firstObject])
		{
			[self.outlineView scrollRowToVisible:self.outlineView.selectedRow];
			[self.outlineView editColumn:0 row:self.outlineView.selectedRow withEvent:nil select:YES];
		}
	}
	return urls.firstObject;
}

- (NSURL*)newFolder:(id)sender
{
	NSURL* directoryURL = self.directoryURLForNewItems;
	if(!directoryURL)
		return nil;

	NSURL* newFolderURL = [directoryURL URLByAppendingPathComponent:@"untitled folder" isDirectory:YES];
	NSArray<NSURL*>* urls = [self performOperation:FBOperationNewFolder sourceURLs:nil destinationURLs:@[ newFolderURL ] unique:YES select:YES];
	if(urls.count == 1 && self.outlineView.numberOfSelectedRows == 1)
	{
		FileItem* newItem = [self.outlineView itemAtRow:self.outlineView.selectedRow];
		if([newItem.URL isEqual:urls.firstObject])
		{
			[self.outlineView scrollRowToVisible:self.outlineView.selectedRow];
			[self.outlineView editColumn:0 row:self.outlineView.selectedRow withEvent:nil select:YES];
		}
	}
	return urls.firstObject;
}

- (void)editSelectedEntries:(id)sender
{
	NSArray<FileItem*>* items = self.previewableItems;
	if(items.count == 1 && items.firstObject.canRename)
	{
		NSInteger row = [self.outlineView rowForItem:items.firstObject];
		if(row != -1)
		{
			[NSApp activateIgnoringOtherApps:YES];
			[self.outlineView.window makeKeyWindow];
			[self.outlineView editColumn:0 row:row withEvent:nil select:YES];
		}
	}
}

- (void)addSelectedEntriesToFavorites:(id)sender
{
	NSURL* url = kURLLocationFavorites;
	NSError* error;
	if([NSFileManager.defaultManager createDirectoryAtURL:url withIntermediateDirectories:YES attributes:nil error:&error])
	{
		for(FileItem* item in self.previewableItems)
		{
			NSURL* linkURL = [url URLByAppendingPathComponent:item.localizedName];
			if(![NSFileManager.defaultManager createSymbolicLinkAtURL:linkURL withDestinationURL:item.resolvedURL error:&error])
				[self.view.window presentError:error];
		}
	}
	else
	{
		[self.view.window presentError:error];
	}
}

- (void)removeSelectedEntriesFromFavorites:(id)sender
{
	for(FileItem* item in self.previewableItems)
	{
		NSError* error;
		if(![NSFileManager.defaultManager trashItemAtURL:item.URL resultingItemURL:nil error:&error])
			[self.view.window presentError:error];
	}
}

- (void)executeBundleCommand:(id)sender
{
	if(bundles::item_ptr item = bundles::lookup(to_s([sender representedObject])))
	{
		// TODO For commands that have ‘input = document’ we should provide the document
		OakCommand* command = [[OakCommand alloc] initWithBundleCommand:parse_command(item)];
		command.firstResponder = self;
		[command executeWithInput:nil variables:item->bundle_variables() outputHandler:nil];
	}
}

- (BOOL)writeItems:(NSArray<FileItem*>*)items toPasteboard:(NSPasteboard*)pboard
{
	if(!items.count)
		return NO;

	[pboard clearContents];
	[pboard writeObjects:[items valueForKeyPath:@"URL"]];

	// If we use writeObjects: then Terminal.app will paste both URLs and their fallback strings
	if(![pboard availableTypeFromArray:@[ NSPasteboardTypeString ]])
		[pboard setString:[[items valueForKeyPath:@"localizedName"] componentsJoinedByString:@"\n"] forType:NSPasteboardTypeString];

	return YES;
}

- (void)cut:(id)sender
{
	NSPasteboard* pboard = NSPasteboard.generalPasteboard;
	if([self writeItems:self.previewableItems toPasteboard:pboard])
		[pboard setString:@"cut" forType:@"OakFileBrowserOperation"];
}

- (void)copy:(id)sender
{
	[self writeItems:self.previewableItems toPasteboard:NSPasteboard.generalPasteboard];
}

- (void)copyAsPathname:(id)sender
{
	NSMutableArray* pathnames = [NSMutableArray array];
	for(FileItem* item in self.previewableItems)
	{
		if(NSString* path = item.URL.path)
			[pathnames addObject:path];
	}

	[NSPasteboard.generalPasteboard clearContents];
	[NSPasteboard.generalPasteboard writeObjects:pathnames];
}

- (void)paste:(id)sender
{
	BOOL hasOperation = [[NSPasteboard.generalPasteboard availableTypeFromArray:@[ @"OakFileBrowserOperation" ]] isEqualToString:@"OakFileBrowserOperation"];
	BOOL cut = hasOperation && [[NSPasteboard.generalPasteboard stringForType:@"OakFileBrowserOperation"] isEqualToString:@"cut"];
	[self insertItemsFromPasteboardWithOperation:cut ? FBOperationMove : FBOperationCopy];
}

- (void)pasteNext:(id)sender
{
	// We use pasteNext: so that this action is triggered by ⌥⌘V
	[self insertItemsFromPasteboardWithOperation:FBOperationMove];
}

- (void)createLinkToPasteboardItems:(id)sender
{
	[self insertItemsFromPasteboardWithOperation:FBOperationLink];
}

- (void)duplicateSelectedEntries:(id)sender
{
	NSArray<FileItem*>* items = self.previewableItems;

	NSMutableDictionary<NSURL*, NSURL*>* urls = [NSMutableDictionary dictionary];
	if(items.count == 1)
	{
		if(NSURL* url = items.firstObject.URL)
		{
			NSString* base = url.lastPathComponent;
			NSString* newBase;

			NSRegularExpression* dateRegex   = [NSRegularExpression regularExpressionWithPattern:@"(\\b|_)[1-2][0-9]{3}(-|_|)(?!00|1[3-9])[0-1][0-9]\\2(?!00|3[2-9])[0-3][0-9](\\b|_)" options:0 error:nil];
			NSRegularExpression* numberRegex = [NSRegularExpression regularExpressionWithPattern:@"^\\d{2,}" options:0 error:nil];

			if(NSTextCheckingResult* match = [dateRegex firstMatchInString:base options:0 range:NSMakeRange(0, base.length)])
			{
				NSDateFormatter* dateFormatter = [[NSDateFormatter alloc] init];
				dateFormatter.dateFormat = [NSString stringWithFormat:@"yyyy%1$@MM%1$@dd", [base substringWithRange:[match rangeAtIndex:2]]];
				newBase = [base stringByReplacingCharactersInRange:match.range withString:[dateRegex replacementStringForResult:match inString:base offset:0 template:[NSString stringWithFormat:@"$1%@$3", [dateFormatter stringFromDate:NSDate.date]]]];
			}
			else if(NSTextCheckingResult* match = [numberRegex firstMatchInString:base options:0 range:NSMakeRange(0, base.length)])
			{
				std::set<NSInteger> set;
				for(NSURL* otherURL in [NSFileManager.defaultManager contentsOfDirectoryAtURL:url.URLByDeletingLastPathComponent includingPropertiesForKeys:nil options:0 error:nil])
				{
					NSString* otherBase = otherURL.lastPathComponent;
					if(NSTextCheckingResult* tmp = [numberRegex firstMatchInString:otherBase options:0 range:NSMakeRange(0, otherBase.length)])
						set.insert([otherBase substringWithRange:tmp.range].integerValue);
				}

				NSInteger i = [base substringWithRange:match.range].integerValue + 1;
				while(set.find(i) != set.end())
					++i;

				NSString* number = [NSString stringWithFormat:@"%0*ld", (int)match.range.length, i];
				newBase = [base stringByReplacingCharactersInRange:match.range withString:number];
			}

			if(newBase && ![newBase isEqualToString:base])
				urls[url] = [url.URLByDeletingLastPathComponent URLByAppendingPathComponent:newBase isDirectory:url.hasDirectoryPath];
		}
	}

	if(urls.count == 0)
	{
		NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"^(.*?)(?: copy(?: \\d+)?)?(\\.\\w+)?$" options:0 error:nil];
		for(FileItem* item in items)
		{
			NSString* base = item.URL.lastPathComponent;
			NSString* name = [regex stringByReplacingMatchesInString:base options:0 range:NSMakeRange(0, base.length) withTemplate:@"$1 copy$2"];
			urls[item.URL] = [item.URL.URLByDeletingLastPathComponent URLByAppendingPathComponent:name isDirectory:item.isDirectory];
		}
	}

	[self performOperation:FBOperationDuplicate withURLs:urls unique:YES select:YES];
	if(urls.count == 1 && self.outlineView.numberOfSelectedRows == 1)
		[self.outlineView editColumn:0 row:self.outlineView.selectedRow withEvent:nil select:YES];
}

- (void)delete:(id)sender
{
	NSOutlineView* outlineView = self.outlineView;

	NSIndexSet* selectedRowIndexes = outlineView.selectedRowIndexes;
	NSInteger clickedRow = outlineView.clickedRow;

	// User right-clicked a single item that is not part of the selection, only delete that item
	if(clickedRow != -1 && ![selectedRowIndexes containsIndex:clickedRow])
	{
		FileItem* item = [outlineView itemAtRow:clickedRow];
		if(NSURL* url = item.URL.filePathURL)
			[self performOperation:FBOperationTrash sourceURLs:@[ url ] destinationURLs:nil unique:NO select:NO];
	}
	else
	{
		NSMutableArray<NSURL*>* urlsToTrash = [NSMutableArray array];
		FileItem* selectItem;
		FileItem* previousItem;

		NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
		while(FileItem* item = stack.firstObject)
		{
			[stack removeObjectAtIndex:0];

			NSURL* url = item.URL.filePathURL;
			if(url && [selectedRowIndexes containsIndex:[outlineView rowForItem:item]])
			{
				selectItem = previousItem;
				[urlsToTrash addObject:url];
			}
			else
			{
				previousItem = item;
				if([outlineView isItemExpanded:item])
					stack = [[item.arrangedChildren arrayByAddingObjectsFromArray:stack] mutableCopy];
			}
		}

		[self performOperation:FBOperationTrash sourceURLs:urlsToTrash destinationURLs:nil unique:NO select:NO];

		NSInteger selectRow = [outlineView rowForItem:selectItem ?: self.fileItem.arrangedChildren.firstObject];
		if(selectRow != -1)
		{
			[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:selectRow] byExtendingSelection:NO];
			[outlineView scrollRowToVisible:selectRow];
		}
	}
}

- (void)insertItemsFromPasteboardWithOperation:(FBOperation)operation
{
	if(NSURL* directoryURL = self.directoryURLForNewItems)
	{
		NSMutableDictionary* urls = [NSMutableDictionary dictionary];
		for(NSURL* srcURL in [self URLsFromPasteboard:NSPasteboard.generalPasteboard])
		{
			BOOL srcIsDirectory;
			if([NSFileManager.defaultManager fileExistsAtPath:srcURL.path isDirectory:&srcIsDirectory])
			{
				NSURL* destURL = [directoryURL URLByAppendingPathComponent:srcURL.lastPathComponent isDirectory:srcIsDirectory];
				if(![srcURL isEqual:destURL] || operation != FBOperationMove)
					urls[srcURL] = destURL;
			}
		}
		[self performOperation:operation withURLs:urls unique:YES select:YES];
	}
}

- (NSArray<NSURL*>*)URLsFromPasteboard:(NSPasteboard*)pboard
{
	return [pboard readObjectsForClasses:@[ [NSURL class] ] options:nil];
}

- (void)didChangeFinderTag:(OFBFinderTagsChooser*)finderTagsChooser
{
	OakFinderTag* chosenTag = finderTagsChooser.chosenTag;
	for(FileItem* item in self.previewableItems)
	{
		NSMutableArray<OakFinderTag*>* tags = [item.finderTags mutableCopy];
		if(finderTagsChooser.removeChosenTag)
			[tags removeObject:chosenTag];
		else if(![tags containsObject:chosenTag])
			[tags addObject:chosenTag];

		[item.URL setResourceValue:[tags valueForKeyPath:@"displayName"] forKey:NSURLTagNamesKey error:nil];
		item.finderTags = [OakFinderTagManager finderTagsForURL:item.URL];
	}
}

- (BOOL)favoritesDirectoryContainsItems:(NSArray<FileItem*>*)items
{
	for(FileItem* item in items)
	{
		if([kURLLocationFavorites isEqual:item.parentURL])
			return YES;
	}
	return NO;
}

- (BOOL)canPaste
{
	return self.directoryURLForNewItems && [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	NSArray<FileItem*>* selectedItems    = self.selectedItems;
	NSArray<FileItem*>* previewableItems = self.previewableItems;

	BOOL res = YES, hideAndDisable = NO;

	if(menuItem.action == @selector(undo:))
	{
		menuItem.title = self.activeUndoManager.undoMenuItemTitle;
		res = self.activeUndoManager.canUndo;
	}
	else if(menuItem.action == @selector(redo:))
	{
		menuItem.title = self.activeUndoManager.redoMenuItemTitle;
		res = self.activeUndoManager.canRedo;
	}
	else if(menuItem.action == @selector(toggleQuickLookPreview:))
	{
		if([QLPreviewPanel sharedPreviewPanelExists] && [QLPreviewPanel sharedPreviewPanel].isVisible)
			menuItem.title = @"Close Quick Look";
		else if(self.previewableItems.count == 0)
			menuItem.hidden = YES;
		else if(self.previewableItems.count == 1)
			menuItem.title = [NSString stringWithFormat:@"Quick Look “%@”", self.previewableItems.firstObject.localizedName];
		else
			menuItem.title = [NSString stringWithFormat:@"Quick Look %ld Items", self.previewableItems.count];
	}
	else if(menuItem.action == @selector(toggleShowInvisibles:))
		menuItem.dynamicTitle = self.showExcludedItems ? @"Hide Invisible Files" : @"Show Invisible Files";
	else if(menuItem.action == @selector(goBack:))
		res = self.canGoBack;
	else if(menuItem.action == @selector(goForward:))
		res = self.canGoForward;
	else if(menuItem.action == @selector(newFolder:))
		res = self.directoryURLForNewItems ? YES : NO;
	else if(menuItem.action == @selector(openSelectedItems:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(openWithMenuAction:))
		hideAndDisable = previewableItems.count == 0 || selectedItems.count == 1 && selectedItems.firstObject.isApplication;
	else if(menuItem.action == @selector(showSelectedEntriesInFinder:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(showOriginal:))
		hideAndDisable = selectedItems.count != 1 || [selectedItems.firstObject.URL isEqual:selectedItems.firstObject.resolvedURL];
	else if(menuItem.action == @selector(showEnclosingFolder:))
		hideAndDisable = selectedItems.count != 1 || [selectedItems.firstObject.parentURL isEqual:((FileItem*)[self.outlineView parentForItem:selectedItems.firstObject]).URL ?: selectedItems.firstObject.parentURL];
	else if(menuItem.action == @selector(showPackageContents:))
		hideAndDisable = previewableItems.count != 1 || previewableItems.firstObject.isPackage == NO;
	else if(menuItem.action == @selector(editSelectedEntries:))
		hideAndDisable = previewableItems.count != 1 || previewableItems.firstObject.canRename == NO;
	else if(menuItem.action == @selector(addSelectedEntriesToFavorites:))
		hideAndDisable = previewableItems.count == 0 || [self favoritesDirectoryContainsItems:previewableItems];
	else if(menuItem.action == @selector(removeSelectedEntriesFromFavorites:))
		hideAndDisable = previewableItems.count == 0 || ![self favoritesDirectoryContainsItems:previewableItems];
	else if(menuItem.action == @selector(delete:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(cut:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(copy:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(copyAsPathname:))
		hideAndDisable = previewableItems.count == 0;
	else if(menuItem.action == @selector(paste:))
		hideAndDisable = self.canPaste == NO;
	else if(menuItem.action == @selector(pasteNext:))
		hideAndDisable = self.canPaste == NO;
	else if(menuItem.action == @selector(createLinkToPasteboardItems:))
		hideAndDisable = self.canPaste == NO;
	else if(menuItem.action == @selector(duplicateSelectedEntries:))
		hideAndDisable = previewableItems.count == 0;

	menuItem.hidden = hideAndDisable && menuItem.target == self;
	if(res = res && !hideAndDisable)
	{
		NSString* copyAsPathnameTitle = previewableItems.count > 1 ? @"Copy%@ as Pathnames" : @"Copy%@ as Pathname";

		struct { NSString* format; SEL action; } const menuTitles[] =
		{
			{ @"Cut%@",                   @selector(cut:)                                },
			{ @"Copy%@",                  @selector(copy:)                               },
			{ copyAsPathnameTitle,        @selector(copyAsPathname:)                     },
			{ @"Show%@ in Finder",        @selector(showSelectedEntriesInFinder:)        },
			{ @"Add%@ to Favorites",      @selector(addSelectedEntriesToFavorites:)      },
			{ @"Remove%@ From Favorites", @selector(removeSelectedEntriesFromFavorites:) },
		};

		for(auto const& info : menuTitles)
		{
			if(menuItem.target == self && menuItem.action == info.action)
			{
				NSString* items;
				switch(previewableItems.count)
				{
					case 0:  items = [NSString stringWithFormat:@" “%@”", self.fileItem.localizedName]; break;
					case 1:  items = [NSString stringWithFormat:@" “%@”", previewableItems.firstObject.localizedName]; break;
					default: items = [NSString stringWithFormat:@" %ld Items", previewableItems.count]; break;
				}
				[menuItem updateTitle:[NSString stringWithFormat:info.format, items]];
			}
		}

		NSString* folderNameForNewItems;
		if([self.directoryURLForNewItems getResourceValue:&folderNameForNewItems forKey:NSURLLocalizedNameKey error:nil])
		{
			if(menuItem.action == @selector(newFolder:))
				menuItem.dynamicTitle = [NSString stringWithFormat:@"New Folder in “%@”", folderNameForNewItems];

			if(menuItem.action == @selector(paste:) && menuItem.target == self)
			{
				NSInteger count = [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
				if(count == 1)
						menuItem.dynamicTitle = [NSString stringWithFormat:@"Paste Item in “%@”", folderNameForNewItems];
				else	menuItem.dynamicTitle = [NSString stringWithFormat:@"Paste %ld Items in “%@”", count, folderNameForNewItems];
			}
			else if(menuItem.action == @selector(pasteNext:) && menuItem.target == self)
			{
				NSInteger count = [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
				if(count == 1)
						menuItem.dynamicTitle = [NSString stringWithFormat:@"Move Item to “%@”", folderNameForNewItems];
				else	menuItem.dynamicTitle = [NSString stringWithFormat:@"Move %ld Items to “%@”", count, folderNameForNewItems];
			}
			else if(menuItem.action == @selector(createLinkToPasteboardItems:) && menuItem.target == self)
			{
				NSInteger count = [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
				if(count == 1)
						menuItem.dynamicTitle = [NSString stringWithFormat:@"Create Link in “%@”", folderNameForNewItems];
				else	menuItem.dynamicTitle = [NSString stringWithFormat:@"Create Link to %ld Items in “%@”", count, folderNameForNewItems];
			}
		}
	}

	return res;
}

// =====================
// = NSRestorableState =
// =====================

+ (NSArray<NSString*>*)restorableStateKeyPaths
{
	return @[ @"showExcludedItems" ];
}

- (void)restoreStateWithCoder:(NSCoder*)state
{
	[super restoreStateWithCoder:state];

	NSArray* newHistory = [state decodeObjectForKey:@"history"];
	if(newHistory.count)
	{
		self.history      = [newHistory mutableCopy];
		self.historyIndex = std::clamp<NSInteger>([state decodeIntegerForKey:@"historyIndex"], 0, newHistory.count);

		NSArray<NSURL*>* expandedURLs = [state decodeObjectForKey:@"expandedURLs"];
		NSArray<NSURL*>* selectedURLs = [state decodeObjectForKey:@"selectedURLs"];
		[self expandURLs:expandedURLs selectURLs:selectedURLs];
	}
}

- (void)encodeRestorableStateWithCoder:(NSCoder*)state
{
	[super encodeRestorableStateWithCoder:state];

	NSMutableArray* history = [NSMutableArray array];
	NSUInteger from = _history.count > 5 ? _history.count - 5 : 0;
	for(NSUInteger i = from; i < _history.count; ++i)
	{
		NSDictionary* record = _history[i];
		NSNumber* scrollOffset = i == _historyIndex ? @(NSMinY(self.outlineView.visibleRect)) : record[@"scrollOffset"];
		if(scrollOffset && scrollOffset.doubleValue > 0)
		{
			[history addObject:@{
				@"url":          record[@"url"],
				@"scrollOffset": scrollOffset,
			}];
		}
		else
		{
			[history addObject:@{ @"url": record[@"url"], }];
		}
	}

	[state encodeObject:history forKey:@"history"];
	[state encodeInteger:_historyIndex - from forKey:@"historyIndex"];
	[state encodeObject:self.selectedURLs.allObjects forKey:@"selectedURLs"];
	[state encodeObject:self.expandedURLs.allObjects forKey:@"expandedURLs"];
}

// ==============
// = Public API =
// ==============

- (id)sessionState
{
	if(NSKeyedArchiver* coder = [[NSKeyedArchiver alloc] init])
	{
		[self encodeRestorableStateWithCoder:coder];
		[coder finishEncoding];
		return coder.encodedData;
	}
	return nil;
}

- (void)setupViewWithState:(id)state
{
	if([state isKindOfClass:[NSData class]])
	{
		if(NSCoder* coder = [[NSKeyedUnarchiver alloc] initForReadingWithData:state])
			[self restoreStateWithCoder:coder];
	}
	else if([state isKindOfClass:[NSDictionary class]])
	{
		NSDictionary* fileBrowserState = state;

		self.showExcludedItems = [fileBrowserState[@"showHidden"] boolValue];

		NSMutableArray* newHistory = [NSMutableArray array];
		for(NSDictionary* entry in fileBrowserState[@"history"])
		{
			if(NSString* urlString = entry[@"url"])
			{
				[newHistory addObject:@{
					@"url": [NSURL URLWithString:urlString],
				}];
			}
		}

		if(newHistory.count)
		{
			self.history      = newHistory;
			self.historyIndex = std::clamp([fileBrowserState[@"historyIndex"] unsignedIntegerValue], (NSUInteger)0, newHistory.count);

			NSMutableArray<NSURL*>* expandedURLs = [NSMutableArray array];
			for(NSString* urlString in fileBrowserState[@"expanded"])
				[expandedURLs addObject:[NSURL URLWithString:urlString]];

			NSMutableArray<NSURL*>* selectedURLs = [NSMutableArray array];
			for(NSString* urlString in fileBrowserState[@"selection"])
				[selectedURLs addObject:[NSURL URLWithString:urlString]];

			[self expandURLs:expandedURLs selectURLs:selectedURLs];
		}
	}
}

- (std::map<std::string, std::string>)variables
{
	std::map<std::string, std::string> env;

	if(self.selectedFileURLs.count)
	{
		std::vector<std::string> paths;
		for(NSURL* url in self.selectedFileURLs)
			paths.emplace_back(path::escape(url.fileSystemRepresentation));

		env["TM_SELECTED_FILE"]  = self.selectedFileURLs.lastObject.fileSystemRepresentation;
		env["TM_SELECTED_FILES"] = text::join(paths, " ");
	}

	return env;
}

- (void)selectURL:(NSURL*)url withParentURL:(NSURL*)parentURL
{
	NSURL* fileReferenceURL = url.fileReferenceURL;
	for(NSInteger i = 0; i < self.outlineView.numberOfRows; ++i)
	{
		FileItem* item = [self.outlineView itemAtRow:i];
		if([url isEqual:item.URL] || [fileReferenceURL isEqual:item.fileReferenceURL])
		{
			[self.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
			[self centerSelectionInVisibleArea:self];
			return;
		}
	}

	NSURL* currentParent = self.URL;
	NSMutableSet<NSURL*>* expandURLs = [self.expandedURLs mutableCopy];

	NSURL* childURL = url;
	while(true)
	{
		NSNumber* flag;
		if([childURL getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] && flag.boolValue)
			break;

		NSURL* potentialParentURL;
		if(![childURL getResourceValue:&potentialParentURL forKey:NSURLParentDirectoryURLKey error:nil] || [childURL isEqual:potentialParentURL])
			break;

		childURL = potentialParentURL;
		if([childURL isEqual:currentParent])
		{
			parentURL = currentParent;
			break;
		}

		if([childURL isEqual:parentURL])
			break;

		[expandURLs addObject:childURL];
	}

	if([childURL isEqual:parentURL])
	{
		[self goToURL:parentURL];
		[self expandURLs:expandURLs.allObjects selectURLs:@[ url ]];
	}
	else
	{
		[self goToURL:url.URLByDeletingLastPathComponent];
		[self expandURLs:nil selectURLs:@[ url ]];
	}
}

- (void)deselectAll:(id)sender
{
	[self.outlineView deselectAll:sender];
}

- (void)orderFrontGoToFolder:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];

	panel.canChooseFiles          = NO;
	panel.canChooseDirectories    = YES;
	panel.allowsMultipleSelection = NO;
	panel.directoryURL            = self.URL.filePathURL;

	[panel beginSheetModalForWindow:self.view.window completionHandler:^(NSModalResponse result) {
		if(result == NSModalResponseOK)
			[self goToURL:panel.URLs.lastObject];
	}];
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
	if(!NSEvent.isSwipeTrackingFromScrollEventsEnabled || anEvent.phase == NSEventPhaseNone || fabs(anEvent.scrollingDeltaX) <= fabs(anEvent.scrollingDeltaY))
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

// ========================
// = From FileBrowserView =
// ========================

- (void)setCanExpandSymbolicLinks:(BOOL)flag
{
	if(_canExpandSymbolicLinks == flag)
		return;
	_canExpandSymbolicLinks = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(item.isLinkToDirectory && (_canExpandPackages || !item.isLinkToPackage))
			[self.outlineView reloadItem:item reloadChildren:YES];
		if([self.outlineView isExpandable:item] && item.arrangedChildren)
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setCanExpandPackages:(BOOL)flag
{
	if(_canExpandPackages == flag)
		return;
	_canExpandPackages = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(item.isDirectory && item.isPackage)
			[self.outlineView reloadItem:item reloadChildren:YES];
		if([self.outlineView isExpandable:item] && item.arrangedChildren)
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setSortDirectoriesBeforeFiles:(BOOL)flag
{
	if(_sortDirectoriesBeforeFiles == flag)
		return;
	_sortDirectoriesBeforeFiles = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		[self rearrangeChildrenInParent:item];
		if(item == self.fileItem || [self.outlineView isItemExpanded:item])
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (void)setShowExcludedItems:(BOOL)flag
{
	if(_showExcludedItems == flag)
		return;
	_showExcludedItems = flag;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		[self rearrangeChildrenInParent:item];
		if(item == self.fileItem || [self.outlineView isItemExpanded:item])
			[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (NSComparator)itemComparator
{
	NSArray<NSSortDescriptor*>* sortDescriptors = @[
		[NSSortDescriptor sortDescriptorWithKey:@"localizedName" ascending:YES selector:@selector(localizedCompare:)],
		[NSSortDescriptor sortDescriptorWithKey:@"URL.URLByDeletingLastPathComponent.lastPathComponent" ascending:YES selector:@selector(localizedCompare:)],
	];

	return ^NSComparisonResult(FileItem* lhs, FileItem* rhs){
		if(_sortDirectoriesBeforeFiles)
		{
			if((lhs.isDirectory || lhs.isLinkToDirectory) && !(rhs.isDirectory || rhs.isLinkToDirectory))
				return NSOrderedAscending;
			else if((rhs.isDirectory || rhs.isLinkToDirectory) && !(lhs.isDirectory || lhs.isLinkToDirectory))
				return NSOrderedDescending;
		}

		for(NSSortDescriptor* sortDescriptor in sortDescriptors)
		{
			NSComparisonResult order = [sortDescriptor compareObject:lhs toObject:rhs];
			if(order != NSOrderedSame)
				return order;
		}

		return NSOrderedSame;
	};
}

- (NSPredicate*)itemPredicateForChildrenInParent:(FileItem*)parentOrNil
{
	NSPredicate* predicate = [NSPredicate predicateWithValue:YES];
	if(!_showExcludedItems)
	{
		NSURL* directoryURL = (parentOrNil ?: self.fileItem).URL;
		settings_t const settings = settings_for_path(NULL_STR, "", directoryURL.fileSystemRepresentation);
		bool excludeMissingFiles = [directoryURL.scheme isEqual:@"scm"] ? false : settings.get(kSettingsExcludeSCMDeletedKey, false);

		path::glob_list_t globs;
		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey),                path::kPathItemFile);
		globs.add_exclude_glob(settings.get(kSettingsExcludeInBrowserKey),            path::kPathItemAny);
		globs.add_exclude_glob(settings.get(kSettingsExcludeKey),                     path::kPathItemAny);

		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesInBrowserKey), path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeDirectoriesKey),          path::kPathItemDirectory);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesInBrowserKey),       path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeFilesKey),                path::kPathItemFile);
		globs.add_include_glob(settings.get(kSettingsIncludeInBrowserKey),            path::kPathItemAny);
		globs.add_include_glob(settings.get(kSettingsIncludeKey, "*"),                path::kPathItemAny);

		predicate = [NSPredicate predicateWithBlock:^BOOL(FileItem* item, NSDictionary* bindings){
			if(item.hidden && ![item.URL.lastPathComponent hasPrefix:@"."])
				return NO;

			if(excludeMissingFiles && item.isMissing)
				return NO;

			char const* path = item.URL.fileSystemRepresentation;
			size_t itemType  = item.isDirectory ? path::kPathItemDirectory : path::kPathItemFile;
			return item.hidden ? globs.include(path, itemType) : !globs.exclude(path, itemType);
		}];
	}
	return predicate;
}

- (NSArray<FileItem*>*)arrangeChildren:(NSArray<FileItem*>*)children inParent:(FileItem*)parentOrNil
{
	return [[children filteredArrayUsingPredicate:[self itemPredicateForChildrenInParent:parentOrNil]] sortedArrayUsingComparator:self.itemComparator];
}

- (void)rearrangeChildrenInParent:(FileItem*)item
{
	NSMutableArray<FileItem*>* existingChildren = item.arrangedChildren;
	if(existingChildren && existingChildren.count * item.children.count < 250000)
	{
		NSArray* newArrangedChildren = [self arrangeChildren:item.children inParent:item];

		// ================
		// = Remove Items =
		// ================

		NSMutableIndexSet* indexesToRemove = [NSMutableIndexSet indexSet];
		for(NSUInteger i = 0; i < existingChildren.count; ++i)
		{
			if(![newArrangedChildren containsObject:existingChildren[i]])
				[indexesToRemove addIndex:i];
		}

		if(indexesToRemove.count)
		{
			BOOL wasFirstResponderInOutlineView = [self.outlineView.window.firstResponder isKindOfClass:[NSView class]] && [(NSView*)self.outlineView.window.firstResponder isDescendantOf:self.outlineView];

			[existingChildren removeObjectsAtIndexes:indexesToRemove];
			[self.outlineView removeItemsAtIndexes:indexesToRemove inParent:(item != self.fileItem ? item : nil) withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideUp];

			if(wasFirstResponderInOutlineView && !([self.outlineView.window.firstResponder isKindOfClass:[NSView class]] && [(NSView*)self.outlineView.window.firstResponder isDescendantOf:self.outlineView]))
				[self.outlineView.window makeFirstResponder:self.outlineView];
		}

		// =======================
		// = Move Items (rename) =
		// =======================

		NSComparator compare = self.itemComparator;

		BOOL alreadySorted = YES;
		for(NSUInteger i = 1; alreadySorted && i < existingChildren.count; ++i)
			alreadySorted = compare(existingChildren[i-1], existingChildren[i]) != NSOrderedDescending;

		if(!alreadySorted)
		{
			NSMutableIndexSet* lcs = MutableLongestCommonSubsequence(existingChildren, newArrangedChildren);

			std::vector<std::pair<BOOL, FileItem*>> v;
			for(NSUInteger i = 0; i < existingChildren.count; ++i)
				v.emplace_back([lcs containsIndex:i], existingChildren[i]);

			for(NSUInteger i = 0; i < v.size(); )
			{
				if(v[i].first == YES)
				{
					i++;
				}
				else
				{
					FileItem* child = existingChildren[i];

					v.erase(v.begin() + i);
					NSInteger newIndex = 0;
					for(; newIndex < v.size(); ++newIndex)
					{
						if(v[newIndex].first && compare(child, v[newIndex].second) == NSOrderedAscending)
							break;
					}
					v.emplace(v.begin() + newIndex, YES, child);

					[existingChildren removeObjectAtIndex:i];
					[existingChildren insertObject:child atIndex:newIndex];
					[self.outlineView moveItemAtIndex:i inParent:(item != self.fileItem ? item : nil) toIndex:newIndex inParent:(item != self.fileItem ? item : nil)];
				}
			}
		}

		// ================
		// = Insert Items =
		// ================

		NSMutableIndexSet* insertionIndexes = [NSMutableIndexSet indexSet];
		for(NSUInteger i = 0; i < newArrangedChildren.count; ++i)
		{
			FileItem* child = newArrangedChildren[i];
			if(![existingChildren containsObject:child])
				[insertionIndexes addIndex:i];
		}

		if(insertionIndexes.count)
		{
			[existingChildren insertObjects:[newArrangedChildren objectsAtIndexes:insertionIndexes] atIndexes:insertionIndexes];
			[self.outlineView insertItemsAtIndexes:insertionIndexes inParent:(item != self.fileItem ? item : nil) withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideUp];
		}
	}
	else
	{
		item.arrangedChildren = [[self arrangeChildren:item.children inParent:item] mutableCopy];
		[self.outlineView reloadItem:(item != self.fileItem ? item : nil) reloadChildren:YES];

		if(item == self.fileItem)
			[self.outlineView setNeedsDisplay:YES];
	}

	[self updateDisambiguationSuffixInParent:item];
}

- (NSString*)disambiguationSuffixForURL:(NSURL*)url numberOfParents:(NSInteger)numberOfParents
{
	NSMutableArray* parentNames = [NSMutableArray array];
	for(NSUInteger i = 0; i < numberOfParents; ++i)
	{
		NSNumber* flag;
		if([url getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] && flag.boolValue)
			return nil;

		NSURL* parentURL;
		if(![url getResourceValue:&parentURL forKey:NSURLParentDirectoryURLKey error:nil] || [url isEqual:parentURL])
			return nil;

		NSString* parentName;
		if(![parentURL getResourceValue:&parentName forKey:NSURLLocalizedNameKey error:nil])
			return nil;

		[parentNames addObject:parentName];
		url = parentURL;
	}
	return [[parentNames.reverseObjectEnumerator allObjects] componentsJoinedByString:@"/"];
}

- (void)updateDisambiguationSuffixInParent:(FileItem*)item
{
	NSArray* children = [item.arrangedChildren filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"URL.isFileURL == YES"]];
	for(FileItem* child in children)
		child.disambiguationSuffix = nil;

	NSInteger showNumberOfParents = 1;
	while(children.count)
	{
		NSCountedSet* countOfConflicts = [[NSCountedSet alloc] initWithArray:[children valueForKeyPath:@"displayName"]];
		NSMutableArray* conflictedChildren = [NSMutableArray array];
		for(FileItem* child in children)
		{
			if([countOfConflicts countForObject:child.displayName] == 1)
				continue;

			if(NSString* newSuffix = [self disambiguationSuffixForURL:child.URL numberOfParents:showNumberOfParents])
			{
				child.disambiguationSuffix = [@" — " stringByAppendingString:newSuffix];
				[conflictedChildren addObject:child];
			}
		}
		children = conflictedChildren;
		++showNumberOfParents;
	}
}

- (void)setModifiedURLs:(NSArray<NSURL*>*)newModifiedURLs
{
	_modifiedURLs = newModifiedURLs;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		item.modified = [_modifiedURLs containsObject:item.URL];
		if(item.children)
			[stack addObjectsFromArray:item.children];
	}
}

- (void)setOpenURLs:(NSArray<NSURL*>*)newOpenURLs
{
	_openURLs = newOpenURLs;

	if(!self.fileItem)
		return;

	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		item.open = [_openURLs containsObject:item.URL];
		if(item.children)
			[stack addObjectsFromArray:item.children];
	}
}

// ===========================
// = Loading/Expanding Items =
// ===========================

- (void)setFileItem:(FileItem*)item
{
	if(_fileItem)
	{
		// Remove visible but non-selected/expanded items from pending selection/expansion
		_expandedURLs = [self.expandedURLs mutableCopy];
		_selectedURLs = [self.selectedURLs mutableCopy];

		for(id observer in _fileItemObservers.allValues)
			[FileItem removeObserver:observer];
		_fileItemObservers = [NSMutableDictionary dictionary];
	}

	_fileItem = item;

	[self.outlineView reloadItem:nil reloadChildren:YES];
	[self.outlineView deselectAll:self];
	[self.outlineView scrollRowToVisible:0];

	[self loadChildrenForItem:item expandChildren:NO];

	[self invalidateRestorableState];
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification
{
	FileItem* item = aNotification.userInfo[@"NSObject"];
	[self loadChildrenForItem:item expandChildren:_expandingChildrenCounter > 0];
	[self invalidateRestorableState];
}

- (void)loadChildrenForItem:(FileItem*)item expandChildren:(BOOL)flag
{
	if(item.arrangedChildren || item.children)
		return;

	NSURL* url = item.URL;

	if(_fileItemObservers[url])
	{
		// ================
		// = Debug Output =
		// ================

		NSMutableArray<NSString*>* itemInfo = [NSMutableArray array];

		NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
		while(FileItem* item = stack.firstObject)
		{
			if(item.isDirectory)
			{
				NSMutableString* info = [item.URL.path mutableCopy];
				if(item == self.fileItem || [self.outlineView isItemExpanded:item])
					[info appendString:@" [expanded]"];
				if([_fileItemObservers objectForKey:item.URL])
					[info appendString:@" [observing]"];
				if([_loadingURLs containsObject:item.URL])
					[info appendString:@" [loading]"];
				if(item.arrangedChildren || item.children)
					[info appendFormat:@" [%lu / %lu children]", item.arrangedChildren.count, item.children.count];
				[itemInfo addObject:info];
			}

			[stack removeObjectAtIndex:0];
			if(NSArray<FileItem*>* children = item.arrangedChildren)
				[stack addObjectsFromArray:children];
		}

		NSLog(@"%s *** Observer already exists for: %@\n%@", sel_getName(_cmd), url, [itemInfo componentsJoinedByString:@"\n"]);

		// ===================================
		// = Temporary (possible) workaround =
		// ===================================

		[FileItem removeObserver:_fileItemObservers[url]];
		_fileItemObservers[url] = nil;
	}

	[_loadingURLs addObject:url];

	__weak FileBrowserViewController* weakSelf = self;
	_fileItemObservers[url] = [FileItem addObserverToDirectoryAtURL:item.resolvedURL usingBlock:^(NSArray<NSURL*>* urls){
		[weakSelf didReceiveURLs:urls forItemWithURL:url expandChildren:flag];
	}];
}

- (FileItem*)findItemForURL:(NSURL*)url
{
	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if([item.URL isEqual:url])
			return item;
		if(NSArray<FileItem*>* children = item.arrangedChildren)
			[stack addObjectsFromArray:children];
	}
	return nil;
}

- (void)didReceiveURLs:(NSArray<NSURL*>*)urls forItemWithURL:(NSURL*)url expandChildren:(BOOL)flag
{
	FileItem* item = [self findItemForURL:url];
	if(!item)
	{
		NSLog(@"%s *** unable to find item for %@", sel_getName(_cmd), url);
	}
	else if(item != self.fileItem && ![self.outlineView isItemExpanded:item])
	{
		NSLog(@"%s *** item no longer expanded: %@", sel_getName(_cmd), item);

		item.children = nil;
		item.arrangedChildren = nil;
		[self.outlineView reloadItem:item reloadChildren:YES];

		[FileItem removeObserver:_fileItemObservers[url]];
		_fileItemObservers[url] = nil;
	}
	else
	{
		NSMutableArray* children = [NSMutableArray array];

		if(item.children)
		{
			NSMutableSet<NSURL*>* newURLs = [NSMutableSet setWithArray:urls];

			for(FileItem* child in item.children)
			{
				if(NSURL* url = child.fileReferenceURL.filePathURL)
					child.URL = url;

				if([newURLs containsObject:child.URL])
				{
					[newURLs removeObject:child.URL];
					[child updateFileProperties];
					[children addObject:child];
				}
			}

			urls = newURLs.allObjects;
		}

		for(NSURL* url in urls)
		{
			FileItem* newItem = [FileItem fileItemWithURL:url];
			newItem.open     = [_openURLs containsObject:url];
			newItem.modified = [_modifiedURLs containsObject:url];
			[children addObject:newItem];
		}

		item.children = [children copy];
		[self rearrangeChildrenInParent:item];

		for(FileItem* child in item.arrangedChildren)
		{
			if((flag && !child.isSymbolicLink || [_expandedURLs containsObject:child.URL] || [child.URL.scheme isEqualToString:@"scm"]) && [self.outlineView isExpandable:child])
				[self.outlineView expandItem:child expandChildren:flag && !child.isSymbolicLink];

			if([_selectedURLs containsObject:child.URL])
			{
				[self.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:[self.outlineView rowForItem:child]] byExtendingSelection:YES];
				[_selectedURLs removeObject:child.URL];
			}
		}
	}

	[_loadingURLs removeObject:url];
	[self checkLoadCompletionHandlers];
}

- (void)checkLoadCompletionHandlers
{
	if(_loadingURLs.count == 0)
	{
		NSArray<void(^)()>* completionHandlers = _loadingURLsCompletionHandlers;
		_loadingURLsCompletionHandlers = nil;
		for(void(^handler)() in completionHandlers)
			handler();
	}
}

- (void)expandURLs:(NSArray<NSURL*>*)expandURLs selectURLs:(NSArray<NSURL*>*)selectURLs
{
	_loadingURLsCompletionHandlers = [(_loadingURLsCompletionHandlers ?: @[ ]) arrayByAddingObject:^{
		[self performSelector:@selector(centerSelectionInVisibleArea:) withObject:self afterDelay:0];
	}];

	_expandedURLs = expandURLs ? [NSMutableSet setWithArray:expandURLs] : _expandedURLs;
	_selectedURLs = selectURLs ? [NSMutableSet setWithArray:selectURLs] : _selectedURLs;

	NSMutableArray<FileItem*>* stack = [self.fileItem.arrangedChildren mutableCopy];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if([_expandedURLs containsObject:item.URL])
		{
			[self.outlineView expandItem:item];
			if(NSArray<FileItem*>* arrangedChildren = item.arrangedChildren)
				[stack addObjectsFromArray:arrangedChildren];
		}
	}

	NSMutableIndexSet* indexesToSelect = [NSMutableIndexSet indexSet];
	for(NSUInteger i = 0; i < self.outlineView.numberOfRows; ++i)
	{
		FileItem* item = [self.outlineView itemAtRow:i];
		if([_selectedURLs containsObject:item.URL])
			[indexesToSelect addIndex:i];
	}
	[self.outlineView selectRowIndexes:indexesToSelect byExtendingSelection:NO];

	[self checkLoadCompletionHandlers];
}

- (void)outlineView:(NSOutlineView*)outlineView willExpandItem:(FileItem*)item expandChildren:(BOOL)flag
{
	_expandingChildrenCounter += flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView didExpandItem:(FileItem*)item expandChildren:(BOOL)flag
{
	_expandingChildrenCounter -= flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView willCollapseItem:(id)someItem collapseChildren:(BOOL)flag
{
	_collapsingChildrenCounter += flag ? 1 : 0;
}

- (void)outlineView:(NSOutlineView*)outlineView didCollapseItem:(id)someItem collapseChildren:(BOOL)flag
{
	_collapsingChildrenCounter -= flag ? 1 : 0;
}

- (void)outlineViewItemWillCollapse:(NSNotification*)aNotification
{
	FileItem* item = aNotification.userInfo[@"NSObject"];
	if(_nestedCollapsingChildrenCounter == 0 || _collapsingChildrenCounter > 0)
		[_expandedURLs removeObject:item.URL];

	_nestedCollapsingChildrenCounter += 1;
}

- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification
{
	_nestedCollapsingChildrenCounter -= 1;

	if(_nestedCollapsingChildrenCounter == 0)
		[self invalidateRestorableState];
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	[self invalidateRestorableState];
}

// ================
// = Location URL =
// ================

- (NSURL*)URL
{
	return _fileItem.URL;
}

- (void)setURL:(NSURL*)url
{
	if(FileItem* item = [FileItem fileItemWithURL:url])
		self.fileItem = item;
}

- (void)reload:(id)sender
{
	NSMutableArray<FileItem*>* stack = [NSMutableArray arrayWithObject:self.fileItem];
	while(FileItem* item = stack.firstObject)
	{
		[stack removeObjectAtIndex:0];
		if(!item.arrangedChildren)
			continue;
		[FSEventsManager.sharedInstance reloadDirectoryAtURL:item.resolvedURL];
		[stack addObjectsFromArray:item.arrangedChildren];
	}
}

- (NSArray<FileItem*>*)selectedItems
{
	NSIndexSet* indexSet;

	NSInteger clickedRow = self.outlineView.clickedRow;
	if(0 <= clickedRow && clickedRow < self.outlineView.numberOfRows && ![self.outlineView.selectedRowIndexes containsIndex:clickedRow])
			indexSet = [NSIndexSet indexSetWithIndex:clickedRow];
	else	indexSet = self.outlineView.selectedRowIndexes;

	NSMutableArray* res = [NSMutableArray array];
	for(NSUInteger index = indexSet.firstIndex; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[res addObject:[self.outlineView itemAtRow:index]];
	return res;
}

- (NSURL*)directoryURLForNewItems
{
	NSMutableArray<NSURL*>* candidates = [NSMutableArray array];
	for(FileItem* item in self.selectedItems)
	{
		if(item.resolvedURL.isFileURL && [self.outlineView isItemExpanded:item])
		{
			[candidates addObject:item.resolvedURL];
		}
		else if(FileItem* parentItem = [self.outlineView parentForItem:item])
		{
			if(parentItem.resolvedURL.isFileURL)
				[candidates addObject:parentItem.resolvedURL];
		}
	}
	return candidates.lastObject ?: self.fileItem.URL.filePathURL;
}

- (void)centerSelectionInVisibleArea:(id)sender
{
	if(self.outlineView.numberOfSelectedRows == 0)
		return;

	NSInteger row = self.outlineView.selectedRowIndexes.firstIndex;

	NSRect rowRect     = [self.outlineView rectOfRow:row];
	NSRect visibleRect = self.outlineView.visibleRect;
	if(NSMinY(rowRect) < NSMinY(visibleRect) || NSMaxY(rowRect) > NSMaxY(visibleRect))
		[self.outlineView scrollPoint:NSMakePoint(NSMinX(rowRect), round(NSMidY(rowRect) - NSHeight(visibleRect)/2))];
}

- (NSSet<NSURL*>*)selectedURLs
{
	NSMutableSet<NSURL*>* res = [_selectedURLs mutableCopy];
	NSIndexSet* selectedIndexes = self.outlineView.selectedRowIndexes;
	for(NSUInteger i = 0; i < self.outlineView.numberOfRows; ++i)
	{
		FileItem* item = [self.outlineView itemAtRow:i];
		if([selectedIndexes containsIndex:i])
				[res addObject:item.URL];
		else	[res removeObject:item.URL];
	}
	return [res copy];
}

- (NSSet<NSURL*>*)expandedURLs
{
	NSMutableSet<NSURL*>* res = [_expandedURLs mutableCopy];
	for(NSUInteger i = 0; i < self.outlineView.numberOfRows; ++i)
	{
		FileItem* item = [self.outlineView itemAtRow:i];
		if([self.outlineView isItemExpanded:item] && ![item.URL.scheme isEqualToString:@"scm"])
				[res addObject:item.URL];
		else	[res removeObject:item.URL];
	}
	return [res copy];
}

// ===========================
// = NSOutlineViewDataSource =
// ===========================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(FileItem*)item
{
	return (item ?: _fileItem).arrangedChildren.count;
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(FileItem*)item
{
	return (item ?: _fileItem).arrangedChildren[childIndex];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(FileItem*)item
{
	return item.isDirectory && (_canExpandPackages || !item.isPackage) || (_canExpandSymbolicLinks && item.isLinkToDirectory && (_canExpandPackages || !item.isLinkToPackage));
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(FileItem*)item
{
	return [item.URL.scheme isEqualToString:@"scm"];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(FileItem*)item
{
	return item.URL.isFileURL;
}

- (id)outlineView:(NSOutlineView*)outlineView objectValueForTableColumn:(NSTableColumn*)tableColumn byItem:(FileItem*)item
{
	return item;
}

- (id <NSPasteboardWriting>)outlineView:(NSOutlineView*)outlineView pasteboardWriterForItem:(FileItem*)item
{
	return item.URL.filePathURL;
}

// ===============================
// = Table cell view constructor =
// ===============================

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FileItem*)item
{
	FileItemTableCellView* res = [outlineView makeViewWithIdentifier:tableColumn.identifier owner:self];
	if(!res)
	{
		res = [[FileItemTableCellView alloc] init];
		res.identifier  = tableColumn.identifier;
		res.openButton.target  = self;
		res.openButton.action  = @selector(takeItemToOpenFrom:);
		res.closeButton.target = self;
		res.closeButton.action = @selector(takeItemToCloseFrom:);
		res.textField.delegate = self;
	}
	return res;
}

- (void)takeItemToOpenFrom:(id)sender
{
	NSInteger row = [self.outlineView rowForView:sender];
	if(row != -1)
	{
		FileItem* item = [self.outlineView itemAtRow:row];
		[self openItems:@[ item ] animate:YES];
	}
}

- (void)takeItemToCloseFrom:(id)sender
{
	NSInteger row = [self.outlineView rowForView:sender];
	if(row != -1)
	{
		FileItem* item = [self.outlineView itemAtRow:row];
		[self.delegate fileBrowser:self closeURL:item.URL];
	}
}

- (BOOL)control:(NSTextField*)textField textShouldEndEditing:(NSText*)fieldEditor
{
	NSInteger row = [self.outlineView rowForView:textField];
	if(row == -1)
		return NO;

	FileItem* item = [self.outlineView itemAtRow:row];
	NSURL* newURL = [[item.URL URLByDeletingLastPathComponent] URLByAppendingPathComponent:fieldEditor.string isDirectory:item.isDirectory];
	if(![item.URL isEqual:newURL])
	{
		// Because of the animation we need to run this after field editor has been removed
		dispatch_async(dispatch_get_main_queue(), ^{
			[self performOperation:FBOperationRename withURLs:@{ item.URL: newURL } unique:NO select:YES];
		});
	}

	return YES;
}

// =============
// = QuickLook =
// =============

- (NSArray<FileItem*>*)previewableItems
{
	return [self.selectedItems filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"previewItemURL != nil"]];
}

- (void)toggleQuickLookPreview:(id)sender
{
	if([QLPreviewPanel sharedPreviewPanelExists] && [[QLPreviewPanel sharedPreviewPanel] isVisible])
			[[QLPreviewPanel sharedPreviewPanel] orderOut:nil];
	else	[[QLPreviewPanel sharedPreviewPanel] makeKeyAndOrderFront:nil];
}

- (BOOL)acceptsPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	return YES;
}

- (void)beginPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = self.previewableItems;
	previewPanel.delegate   = self;
	previewPanel.dataSource = self;
}

- (void)endPreviewPanelControl:(QLPreviewPanel*)previewPanel
{
	_previewItems = nil;
}

- (NSInteger)numberOfPreviewItemsInPreviewPanel:(QLPreviewPanel*)previewPanel
{
	return _previewItems.count;
}

- (id <QLPreviewItem>)previewPanel:(QLPreviewPanel*)panel previewItemAtIndex:(NSInteger)index
{
	return _previewItems[index];
}

- (NSRect)previewPanel:(QLPreviewPanel*)previewPanel sourceFrameOnScreenForPreviewItem:(id <QLPreviewItem>)item
{
	return [self imageRectOfItem:item];
}

- (NSRect)imageRectOfItem:(FileItem*)item
{
	NSInteger row = [self.outlineView rowForItem:item];
	if(row != -1)
	{
		FileItemTableCellView* view = [self.outlineView viewAtColumn:0 row:row makeIfNecessary:YES];
		if([view isKindOfClass:[FileItemTableCellView class]])
		{
			NSButton* imageButton = view.openButton;
			NSRect imageRect = NSIntersectionRect([imageButton convertRect:imageButton.bounds toView:nil], [self.outlineView convertRect:self.outlineView.visibleRect toView:nil]);
			return NSIsEmptyRect(imageRect) ? NSZeroRect : [view.window convertRectToScreen:imageRect];
		}
	}
	return NSZeroRect;
}

- (BOOL)previewPanel:(QLPreviewPanel*)previewPanel handleEvent:(NSEvent*)event
{
	std::string const eventString = to_s(event);
	if((event.type == NSEventTypeKeyUp || event.type == NSEventTypeKeyDown) && (eventString == utf8::to_s(NSUpArrowFunctionKey) || eventString == utf8::to_s(NSDownArrowFunctionKey)))
	{
		[self.view.window sendEvent:event];
		_previewItems = self.previewableItems;
		[previewPanel reloadData];
		return YES;
	}
	return NO;
}

// ============
// = Services =
// ============

+ (void)initialize
{
	[NSApplication.sharedApplication registerServicesMenuSendTypes:@[ NSFilenamesPboardType, NSURLPboardType ] returnTypes:@[ ]];
}

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	return returnType == nil && sendType != nil && [@[ NSFilenamesPboardType, NSURLPboardType ] containsObject:sendType] ? self : nil;
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	NSArray<NSURL*>* urls = [self.previewableItems valueForKeyPath:@"URL"];
	if(urls.count == 0)
		return NO;

	[pboard clearContents];
	return [pboard writeObjects:urls];
}

// ===================
// = Accepting Drops =
// ===================

- (NSDragOperation)outlineView:(NSOutlineView*)outlineView validateDrop:(id <NSDraggingInfo>)info proposedItem:(FileItem*)item proposedChildIndex:(NSInteger)childIndex
{
	NSURL* dropURL = (item ?: self.fileItem).resolvedURL.filePathURL;
	if(![self.outlineView isExpandable:item] || !dropURL || ![NSFileManager.defaultManager fileExistsAtPath:dropURL.path])
		return NSDragOperationNone;

	NSPasteboard* pboard  = info.draggingPasteboard;
	NSArray* draggedPaths = [pboard propertyListForType:NSFilenamesPboardType];

	dev_t targetDevice   = path::device(dropURL.fileSystemRepresentation);
	BOOL linkOperation   = (NSApp.currentEvent.modifierFlags & NSEventModifierFlagControl) == NSEventModifierFlagControl;
	BOOL toggleOperation = (NSApp.currentEvent.modifierFlags & NSEventModifierFlagOption) == NSEventModifierFlagOption;

	// We accept the drop as long as it is valid for at least one of the items
	for(NSString* draggedPath in draggedPaths)
	{
		BOOL sameSource = (path::device(draggedPath.fileSystemRepresentation) == targetDevice);
		NSDragOperation operation = linkOperation ? NSDragOperationLink : ((sameSource != toggleOperation) ? NSDragOperationMove : NSDragOperationCopy);

		// Can’t move into same location
		NSString* parentPath = draggedPath.stringByDeletingLastPathComponent;
		if(operation == NSDragOperationMove && [parentPath isEqualToString:dropURL.path])
			continue;

		[outlineView setDropItem:item dropChildIndex:NSOutlineViewDropOnItemIndex];
		return operation;
	}
	return NSDragOperationNone;
}

static NSDragOperation filter (NSDragOperation mask)
{
	return (mask & NSDragOperationMove) ? NSDragOperationMove : ((mask & NSDragOperationCopy) ? NSDragOperationCopy : ((mask & NSDragOperationLink) ? NSDragOperationLink : 0));
}

- (BOOL)outlineView:(NSOutlineView*)outlineView acceptDrop:(id <NSDraggingInfo>)info item:(FileItem*)item childIndex:(NSInteger)childIndex
{
	FileItem* newParent = item ?: self.fileItem;

	NSDragOperation op = filter(info.draggingSourceOperationMask);
	if(op == 0 || ![self.outlineView isExpandable:newParent] || !newParent.resolvedURL.isFileURL)
		return NO;

	NSMutableDictionary<NSURL*, NSURL*>* urls = [NSMutableDictionary dictionary];
	for(NSURL* url in [self URLsFromPasteboard:info.draggingPasteboard])
		urls[url] = [newParent.resolvedURL URLByAppendingPathComponent:url.lastPathComponent isDirectory:op != NSDragOperationLink && url.hasDirectoryPath];

	switch(op)
	{
		case NSDragOperationLink: [self performOperation:FBOperationLink withURLs:urls unique:NO select:NO]; break;
		case NSDragOperationCopy: [self performOperation:FBOperationCopy withURLs:urls unique:NO select:NO]; break;
		case NSDragOperationMove: [self performOperation:FBOperationMove withURLs:urls unique:NO select:NO]; break;
	}

	return YES;
}

- (void)outlineView:(NSOutlineView*)outlineView didTrashURLs:(NSArray<NSURL*>*)someURLs
{
	[self performOperation:FBOperationTrash sourceURLs:someURLs destinationURLs:nil unique:NO select:NO];
}

// =============
// = Undo/Redo =
// =============

- (NSUndoManager*)undoManager
{
	if(!_fileBrowserUndoManager)
		_fileBrowserUndoManager = [[NSUndoManager alloc] init];
	return _fileBrowserUndoManager;
}

- (NSUndoManager*)activeUndoManager
{
	NSResponder* firstResponder = self.view.window.firstResponder;
	if([firstResponder isKindOfClass:[NSView class]] && [(NSView*)firstResponder isDescendantOf:self.view])
			return firstResponder.undoManager;
	else	return self.undoManager;
}

- (void)undo:(id)sender
{
	[self.activeUndoManager undo];
}

- (void)redo:(id)sender
{
	[self.activeUndoManager redo];
}
@end
