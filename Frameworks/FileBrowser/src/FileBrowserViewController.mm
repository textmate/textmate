#import "FileBrowserViewController.h"
#import "FileBrowserView.h"
#import "FileItem.h"
#import "SCMManager.h"
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

static bool is_binary (std::string const& path)
{
	if(path == NULL_STR)
		return false;

	settings_t const& settings = settings_for_path(path);
	if(settings.has(kSettingsBinaryKey))
		return path::glob_t(settings.get(kSettingsBinaryKey, "")).does_match(path);

	return false;
}

@interface FileBrowserViewController () <NSMenuDelegate>
@property (nonatomic) NSMenuItem* currentLocationMenuItem;
@property (nonatomic) NSMutableArray<NSDictionary*>* history;
@property (nonatomic) NSInteger historyIndex;
@property (nonatomic) FileBrowserView* fileBrowserView;
@end

@implementation FileBrowserViewController
+ (NSSet*)keyPathsForValuesAffectingCanGoBack    { return [NSSet setWithObjects:@"historyIndex", nil]; }
+ (NSSet*)keyPathsForValuesAffectingCanGoForward { return [NSSet setWithObjects:@"historyIndex", nil]; }

- (void)dealloc
{
	if(_currentLocationMenuItem)
	{
		[_currentLocationMenuItem unbind:NSTitleBinding];
		[_currentLocationMenuItem unbind:NSImageBinding];
	}

	if(OFBHeaderView* headerView = _fileBrowserView.headerView)
	{
		[headerView.goBackButton    unbind:NSEnabledBinding];
		[headerView.goForwardButton unbind:NSEnabledBinding];
	}
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

		_fileBrowserView.target      = self;
		_fileBrowserView.openAction  = @selector(didClickItemImageButton:);
		_fileBrowserView.closeAction = @selector(didClickItemCloseButton:);

		_currentLocationMenuItem = [[NSMenuItem alloc] initWithTitle:@"" action:@selector(takeURLFrom:) keyEquivalent:@""];
		_currentLocationMenuItem.target = self;
		[_currentLocationMenuItem bind:NSTitleBinding toObject:_fileBrowserView withKeyPath:@"fileItem.displayName" options:nil];
		[_currentLocationMenuItem bind:NSImageBinding toObject:_fileBrowserView withKeyPath:@"fileItem.image" options:nil];

		NSOutlineView* outlineView = _fileBrowserView.outlineView;
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
	self.fileBrowserView.showExcludedItems = !self.fileBrowserView.showExcludedItems;
}

- (NSView*)headerView                { return self.fileBrowserView.headerView;              }
- (NSOutlineView*)outlineView        { return self.fileBrowserView.outlineView; }
- (NSString*)path                    { return self.fileBrowserView.URL.filePathURL.path;    }
- (NSURL*)directoryURLForNewItems    { return self.fileBrowserView.directoryURLForNewItems; }
- (NSArray<FileItem*>*)selectedItems { return self.fileBrowserView.selectedItems; }
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
	self.fileBrowserView.URL = self.history[index][@"url"];
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
	if(url && ![self.fileBrowserView.URL isEqual:url])
		[self addHistoryURL:url];
}

- (void)goToComputer:(id)sender      { [self goToURL:kURLLocationComputer]; }
- (void)goToHome:(id)sender          { [self goToURL:[NSURL fileURLWithPath:NSHomeDirectory()]]; }
- (void)goToDesktop:(id)sender       { [self goToURL:[NSFileManager.defaultManager URLForDirectory:NSDesktopDirectory inDomain:NSUserDomainMask appropriateForURL:nil create:YES error:nil]]; }

- (void)goToFavorites:(id)sender
{
	if(![self.fileBrowserView.URL isEqual:kURLLocationFavorites])
		[self goToURL:kURLLocationFavorites];
	else if(self.canGoBack)
		[self goBack:sender];
}

- (void)goToSCMDataSource:(id)sender
{
	NSURL* url = self.fileBrowserView.URL;
	if([url.scheme isEqualToString:@"file"])
	{
		SCMRepository* repository = [SCMManager.sharedInstance repositoryAtURL:url];
		if(repository && repository.enabled)
		{
			[self goToURL:[NSURL URLWithString:[NSString stringWithFormat:@"scm://localhost%@/", [url.path stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]]]];
		}
		else
		{
			NSAlert* alert = [[NSAlert alloc] init];

			if(repository)
			{
				alert.messageText     = [NSString stringWithFormat:@"Version control is disabled for “%@”.", self.fileBrowserView.fileItem.localizedName];
				alert.informativeText = @"For performance reasons TextMate will not monitor version control information for this folder.";
			}
			else
			{
				alert.messageText     = [NSString stringWithFormat:@"Version control is not available for “%@”.", self.fileBrowserView.fileItem.localizedName];
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
		else if(NSURL* parentURL = self.fileBrowserView.fileItem.parentURL)
			[self goToURL:parentURL];
	}
	else
	{
		NSBeep();
	}
}

- (void)goToParentFolder:(id)sender
{
	if(NSURL* url = self.fileBrowserView.fileItem.parentURL)
	{
		NSURL* cameFromURL = self.fileBrowserView.URL;
		[self goToURL:url];
		[self.fileBrowserView expandURLs:nil selectURLs:@[ cameFromURL ]];
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

	FileItem* fileItem = self.fileBrowserView.fileItem;
	while(fileItem = [FileItem fileItemWithURL:fileItem.parentURL])
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:fileItem.localizedName action:@selector(takeURLFrom:) keyEquivalent:@""];
		menuItem.representedObject = fileItem.resolvedURL;
		menuItem.image             = fileItem.image;
		menuItem.target            = self;
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[[menu addItemWithTitle:@"Other…" action:@selector(orderFrontGoToFolder:) keyEquivalent:@""] setTarget:self];

	FileItem* item = self.fileBrowserView.fileItem;
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
				[OakZoomingIcon zoomIcon:item.image fromRect:[self.fileBrowserView imageRectOfItem:item]];
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

- (void)didClickItemImageButton:(FileItem*)item
{
	[self openItems:@[ item ] animate:YES];
}

- (void)didClickItemCloseButton:(FileItem*)item
{
	[self.delegate fileBrowser:self closeURL:item.URL];
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
		{ @"Quick Look",              @selector(toggleQuickLookPreview:), .target = self.fileBrowserView },
		{ @"Add to Favorites",        @selector(addSelectedEntriesToFavorites:)      },
		{ @"Remove From Favorites",   @selector(removeSelectedEntriesFromFavorites:) },
		{ /* -------- */ },
		{ @"Move to Trash",           @selector(deleteURLs:) },
		{ /* -------- */ .ref = &insertBundleItemsMenuItem },
		{ /* -------- */ },
		{ @"Copy",                    @selector(copyURLs:)                                                                        },
		{ @"Copy as Pathname",        @selector(copyAsPathname:),      @"",  NSEventModifierFlagOption, .alternate = YES, .tag = kRequiresSelectionTag },
		{ @"Paste",                   @selector(pasteURLs:)                                                                       },
		{ @"Move Items Here",         @selector(moveURLs:),            @"v", NSEventModifierFlagCommand|NSEventModifierFlagOption },
		{ /* -------- */ },
		{ @"Finder Tag", .ref = &finderTagsMenuItem,   .tag = kRequiresSelectionTag },
		{ /* -------- */ },
		{ @"Undo",                    @selector(undo:), .target = self.fileBrowserView },
		{ @"Redo",                    @selector(redo:), .target = self.fileBrowserView },
		{ /* -------- */ },
	};

	MBCreateMenu(items, menu);

	if(self.fileBrowserView.previewableItems.count == 0)
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

	[OakOpenWithMenu addOpenWithMenuForPaths:[NSSet setWithArray:[self.fileBrowserView.previewableItems valueForKeyPath:@"resolvedURL.path"]] toMenuItem:openWithMenuItem];
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
		[self.fileBrowserView expandURLs:nil selectURLs:@[ resolvedURL ]];
	}
}

- (void)showEnclosingFolder:(id)sender
{
	NSURL* url = self.selectedItems.firstObject.URL;
	if(NSURL* enclosingFolder = url.URLByDeletingLastPathComponent)
	{
		[self goToURL:enclosingFolder];
		[self.fileBrowserView expandURLs:nil selectURLs:@[ url ]];
	}
}

- (void)showPackageContents:(id)sender
{
	[self goToURL:self.fileBrowserView.previewableItems.firstObject.resolvedURL];
}

- (void)showSelectedEntriesInFinder:(id)sender
{
	[NSWorkspace.sharedWorkspace activateFileViewerSelectingURLs:[self.fileBrowserView.previewableItems valueForKeyPath:@"resolvedURL"]];
}

- (NSURL*)newFile:(id)sender
{
	NSURL* directoryURL = self.fileBrowserView.directoryURLForNewItems;
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
	NSArray<NSURL*>* urls = [self.fileBrowserView performOperation:FBOperationNewFile sourceURLs:nil destinationURLs:@[ newFileURL ] unique:YES select:YES];
	if(urls.count == 1 && self.fileBrowserView.outlineView.numberOfSelectedRows == 1)
	{
		FileItem* newItem = [self.fileBrowserView.outlineView itemAtRow:self.fileBrowserView.outlineView.selectedRow];
		if([newItem.URL isEqual:urls.firstObject])
		{
			[self.fileBrowserView.outlineView scrollRowToVisible:self.fileBrowserView.outlineView.selectedRow];
			[self.fileBrowserView.outlineView editColumn:0 row:self.fileBrowserView.outlineView.selectedRow withEvent:nil select:YES];
		}
	}
	return urls.firstObject;
}

- (NSURL*)newFolder:(id)sender
{
	NSURL* directoryURL = self.fileBrowserView.directoryURLForNewItems;
	if(!directoryURL)
		return nil;

	NSURL* newFolderURL = [directoryURL URLByAppendingPathComponent:@"untitled folder" isDirectory:YES];
	NSArray<NSURL*>* urls = [self.fileBrowserView performOperation:FBOperationNewFolder sourceURLs:nil destinationURLs:@[ newFolderURL ] unique:YES select:YES];
	if(urls.count == 1 && self.fileBrowserView.outlineView.numberOfSelectedRows == 1)
	{
		FileItem* newItem = [self.fileBrowserView.outlineView itemAtRow:self.fileBrowserView.outlineView.selectedRow];
		if([newItem.URL isEqual:urls.firstObject])
		{
			[self.fileBrowserView.outlineView scrollRowToVisible:self.fileBrowserView.outlineView.selectedRow];
			[self.fileBrowserView.outlineView editColumn:0 row:self.fileBrowserView.outlineView.selectedRow withEvent:nil select:YES];
		}
	}
	return urls.firstObject;
}

- (void)editSelectedEntries:(id)sender
{
	NSArray<FileItem*>* items = self.fileBrowserView.previewableItems;
	if(items.count == 1 && items.firstObject.canRename)
	{
		NSInteger row = [self.fileBrowserView.outlineView rowForItem:items.firstObject];
		if(row != -1)
		{
			[NSApp activateIgnoringOtherApps:YES];
			[self.fileBrowserView.window makeKeyWindow];
			[self.fileBrowserView.outlineView editColumn:0 row:row withEvent:nil select:YES];
		}
	}
}

- (void)addSelectedEntriesToFavorites:(id)sender
{
	NSURL* url = kURLLocationFavorites;
	NSError* error;
	if([NSFileManager.defaultManager createDirectoryAtURL:url withIntermediateDirectories:YES attributes:nil error:&error])
	{
		for(FileItem* item in self.fileBrowserView.previewableItems)
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
	for(FileItem* item in self.fileBrowserView.previewableItems)
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

	if(![pboard.types containsObject:NSStringPboardType])
		[pboard writeObjects:[items valueForKeyPath:@"localizedName"]];

	return YES;
}

- (void)cutURLs:(id)sender
{
	NSPasteboard* pboard = NSPasteboard.generalPasteboard;
	if([self writeItems:self.fileBrowserView.previewableItems toPasteboard:pboard])
		[pboard setString:@"cut" forType:@"OakFileBrowserOperation"];
}

- (void)copyURLs:(id)sender
{
	[self writeItems:self.fileBrowserView.previewableItems toPasteboard:NSPasteboard.generalPasteboard];
}

- (void)copyAsPathname:(id)sender
{
	NSMutableArray* pathnames = [NSMutableArray array];
	for(FileItem* item in self.fileBrowserView.previewableItems)
	{
		if(NSString* path = item.URL.path)
			[pathnames addObject:path];
	}

	[NSPasteboard.generalPasteboard clearContents];
	[NSPasteboard.generalPasteboard writeObjects:pathnames];
}

- (void)pasteURLs:(id)sender
{
	BOOL hasOperation = [[NSPasteboard.generalPasteboard availableTypeFromArray:@[ @"OakFileBrowserOperation" ]] isEqualToString:@"OakFileBrowserOperation"];
	BOOL cut = hasOperation && [[NSPasteboard.generalPasteboard stringForType:@"OakFileBrowserOperation"] isEqualToString:@"cut"];
	[self insertItemsAndRemoveOriginal:cut];
}

- (void)moveURLs:(id)sender
{
	[self insertItemsAndRemoveOriginal:YES];
}

- (void)duplicateSelectedEntries:(id)sender
{
	NSArray<FileItem*>* items = self.fileBrowserView.previewableItems;

	NSMutableDictionary<NSURL*, NSURL*>* urls = [NSMutableDictionary dictionary];
	if(items.count == 1)
	{
		NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"(\\b|_)\\d{4}(?:-\\d{2}){2}(\\b|_)" options:0 error:nil];
		NSDateFormatter* formatter = [[NSDateFormatter alloc] init];
		formatter.dateFormat = @"yyyy-MM-dd";

		if(NSURL* url = items.firstObject.URL)
		{
			NSString* base = url.lastPathComponent;
			NSString* name = [regex stringByReplacingMatchesInString:base options:0 range:NSMakeRange(0, base.length) withTemplate:[NSString stringWithFormat:@"$1%@$2", [formatter stringFromDate:[NSDate date]]]];
			if(![base isEqualToString:name])
				urls[url] = [url.URLByDeletingLastPathComponent URLByAppendingPathComponent:name isDirectory:url.tmHasDirectoryPath];
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

	[self.fileBrowserView performOperation:FBOperationDuplicate withURLs:urls unique:YES select:YES];
	if(urls.count == 1 && self.fileBrowserView.outlineView.numberOfSelectedRows == 1)
		[self.fileBrowserView.outlineView editColumn:0 row:self.fileBrowserView.outlineView.selectedRow withEvent:nil select:YES];
}

- (void)deleteURLs:(id)sender
{
	NSOutlineView* outlineView = self.fileBrowserView.outlineView;

	NSIndexSet* selectedRowIndexes = outlineView.selectedRowIndexes;
	NSInteger clickedRow = outlineView.clickedRow;

	// User right-clicked a single item that is not part of the selection, only delete that item
	if(clickedRow != -1 && ![selectedRowIndexes containsIndex:clickedRow])
	{
		FileItem* item = [outlineView itemAtRow:clickedRow];
		if(NSURL* url = item.URL.filePathURL)
			[self.fileBrowserView performOperation:FBOperationTrash sourceURLs:@[ url ] destinationURLs:nil unique:NO select:NO];
	}
	else
	{
		NSMutableArray<NSURL*>* urlsToTrash = [NSMutableArray array];
		FileItem* selectItem;
		FileItem* previousItem;

		NSMutableArray<FileItem*>* stack = [self.fileBrowserView.fileItem.arrangedChildren mutableCopy];
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

		[self.fileBrowserView performOperation:FBOperationTrash sourceURLs:urlsToTrash destinationURLs:nil unique:NO select:NO];

		NSInteger selectRow = [outlineView rowForItem:selectItem ?: self.fileBrowserView.fileItem.arrangedChildren.firstObject];
		if(selectRow != -1)
		{
			[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:selectRow] byExtendingSelection:NO];
			[outlineView scrollRowToVisible:selectRow];
		}
	}
}

- (void)cut:(id)sender       { [self cutURLs:sender];    }
- (void)copy:(id)sender      { [self copyURLs:sender];   }
- (void)paste:(id)sender     { [self pasteURLs:sender];  }
- (void)pasteNext:(id)sender { [self moveURLs:sender];   }
- (void)delete:(id)sender    { [self deleteURLs:sender]; }

- (void)insertItemsAndRemoveOriginal:(BOOL)removeOriginal
{
	if(NSURL* directoryURL = self.fileBrowserView.directoryURLForNewItems)
	{
		NSMutableDictionary* urls = [NSMutableDictionary dictionary];
		for(NSURL* srcURL in [self URLsFromPasteboard:NSPasteboard.generalPasteboard])
		{
			BOOL srcIsDirectory;
			if([NSFileManager.defaultManager fileExistsAtPath:srcURL.path isDirectory:&srcIsDirectory])
			{
				NSURL* destURL = [directoryURL URLByAppendingPathComponent:srcURL.lastPathComponent isDirectory:srcIsDirectory];
				if(![srcURL isEqual:destURL] || !removeOriginal)
					urls[srcURL] = destURL;
			}
		}

		if(removeOriginal)
				[self.fileBrowserView performOperation:FBOperationMove withURLs:urls unique:YES select:YES];
		else	[self.fileBrowserView performOperation:FBOperationCopy withURLs:urls unique:YES select:YES];
	}
}

- (NSArray<NSURL*>*)URLsFromPasteboard:(NSPasteboard*)pboard
{
	NSMutableArray<NSURL*>* res = [NSMutableArray array];
	for(NSString* path in [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : @[ ])
		[res addObject:[NSURL fileURLWithPath:path]];
	return res;
}

- (void)didChangeFinderTag:(OFBFinderTagsChooser*)finderTagsChooser
{
	OakFinderTag* chosenTag = finderTagsChooser.chosenTag;
	for(FileItem* item in self.fileBrowserView.previewableItems)
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
	return self.fileBrowserView.directoryURLForNewItems && [[NSPasteboard.generalPasteboard availableTypeFromArray:@[ NSFilenamesPboardType ]] isEqualToString:NSFilenamesPboardType];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menuItem
{
	NSArray<FileItem*>* selectedItems    = self.selectedItems;
	NSArray<FileItem*>* previewableItems = self.fileBrowserView.previewableItems;

	BOOL res = YES;

	if(menuItem.action == @selector(toggleShowInvisibles:))
		menuItem.dynamicTitle = self.fileBrowserView.showExcludedItems ? @"Hide Invisible Files" : @"Show Invisible Files";
	else if(menuItem.action == @selector(goBack:))
		res = self.canGoBack;
	else if(menuItem.action == @selector(goForward:))
		res = self.canGoForward;
	else if(menuItem.action == @selector(newFolder:))
		res = self.fileBrowserView.directoryURLForNewItems ? YES : NO;
	else if(menuItem.action == @selector(openSelectedItems:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(openWithMenuAction:))
		menuItem.hidden = previewableItems.count == 0 || selectedItems.count == 1 && selectedItems.firstObject.isApplication;
	else if(menuItem.action == @selector(showSelectedEntriesInFinder:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(showOriginal:))
		menuItem.hidden = selectedItems.count != 1 || [selectedItems.firstObject.URL isEqual:selectedItems.firstObject.resolvedURL];
	else if(menuItem.action == @selector(showEnclosingFolder:))
		menuItem.hidden = selectedItems.count != 1 || [selectedItems.firstObject.parentURL isEqual:((FileItem*)[self.fileBrowserView.outlineView parentForItem:selectedItems.firstObject]).URL ?: selectedItems.firstObject.parentURL];
	else if(menuItem.action == @selector(showPackageContents:))
		menuItem.hidden = previewableItems.count != 1 || previewableItems.firstObject.isPackage == NO;
	else if(menuItem.action == @selector(editSelectedEntries:))
		menuItem.hidden = previewableItems.count != 1 || previewableItems.firstObject.canRename == NO;
	else if(menuItem.action == @selector(addSelectedEntriesToFavorites:))
		menuItem.hidden = previewableItems.count == 0 || [self favoritesDirectoryContainsItems:previewableItems];
	else if(menuItem.action == @selector(removeSelectedEntriesFromFavorites:))
		menuItem.hidden = previewableItems.count == 0 || ![self favoritesDirectoryContainsItems:previewableItems];
	else if(menuItem.action == @selector(cutURLs:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(copyURLs:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(copyAsPathname:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(pasteURLs:))
		menuItem.hidden = self.canPaste == NO;
	else if(menuItem.action == @selector(moveURLs:))
		menuItem.hidden = self.canPaste == NO;
	else if(menuItem.action == @selector(duplicateSelectedEntries:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(deleteURLs:))
		menuItem.hidden = previewableItems.count == 0;
	else if(menuItem.action == @selector(cut:))
		res = previewableItems.count != 0;
	else if(menuItem.action == @selector(copy:))
		res = previewableItems.count != 0;
	else if(menuItem.action == @selector(paste:))
		res = self.canPaste;
	else if(menuItem.action == @selector(pasteNext:))
		res = self.canPaste;
	else if(menuItem.action == @selector(delete:))
		res = previewableItems.count != 0;

	if(res && !menuItem.hidden)
	{
		NSString* copyAsPathnameTitle = previewableItems.count > 1 ? @"Copy%@ as Pathnames" : @"Copy%@ as Pathname";

		struct { NSString* format; SEL action; } const menuTitles[] =
		{
			{ @"Cut%@",                   @selector(cut:)                                },
			{ @"Cut%@",                   @selector(cutURLs:)                            },
			{ @"Copy%@",                  @selector(copy:)                               },
			{ @"Copy%@",                  @selector(copyURLs:)                           },
			{ copyAsPathnameTitle,        @selector(copyAsPathname:)                     },
			{ @"Show%@ in Finder",        @selector(showSelectedEntriesInFinder:)        },
			{ @"Add%@ to Favorites",      @selector(addSelectedEntriesToFavorites:)      },
			{ @"Remove%@ From Favorites", @selector(removeSelectedEntriesFromFavorites:) },
		};

		for(auto const& info : menuTitles)
		{
			if(info.action == menuItem.action)
			{
				NSString* items;
				switch(previewableItems.count)
				{
					case 0:  items = [NSString stringWithFormat:@" “%@”", self.fileBrowserView.fileItem.localizedName]; break;
					case 1:  items = [NSString stringWithFormat:@" “%@”", previewableItems.firstObject.localizedName]; break;
					default: items = [NSString stringWithFormat:@" %ld Items", previewableItems.count]; break;
				}
				menuItem.dynamicTitle = [NSString stringWithFormat:info.format, items];
			}
		}

		NSString* folderNameForNewItems;
		if([self.fileBrowserView.directoryURLForNewItems getResourceValue:&folderNameForNewItems forKey:NSURLLocalizedNameKey error:nil])
		{
			if(menuItem.action == @selector(newFolder:))
				menuItem.dynamicTitle = [NSString stringWithFormat:@"New Folder in “%@”", folderNameForNewItems];

			if(menuItem.action == @selector(paste:) || menuItem.action == @selector(pasteURLs:))
			{
				NSInteger count = [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
				if(count == 1)
						menuItem.dynamicTitle = [NSString stringWithFormat:@"Paste Item in “%@”", folderNameForNewItems];
				else	menuItem.dynamicTitle = [NSString stringWithFormat:@"Paste %ld Items in “%@”", count, folderNameForNewItems];
			}
			else if(menuItem.action == @selector(pasteNext:) || menuItem.action == @selector(moveURLs:))
			{
				NSInteger count = [self URLsFromPasteboard:NSPasteboard.generalPasteboard].count;
				if(count == 1)
						menuItem.dynamicTitle = [NSString stringWithFormat:@"Move Item to “%@”", folderNameForNewItems];
				else	menuItem.dynamicTitle = [NSString stringWithFormat:@"Move %ld Items to “%@”", count, folderNameForNewItems];
			}
		}
	}

	return res;
}

// ==============
// = Public API =
// ==============

- (NSArray<NSURL*>*)modifiedURLs                          { return self.fileBrowserView.modifiedURLs; }
- (void)setModifiedURLs:(NSArray<NSURL*>*)newModifiedURLs { self.fileBrowserView.modifiedURLs = newModifiedURLs; }

- (NSArray<NSURL*>*)openURLs                              { return self.fileBrowserView.openURLs; }
- (void)setOpenURLs:(NSArray<NSURL*>*)newOpenURLs         { self.fileBrowserView.openURLs = newOpenURLs; }

- (NSDictionary*)sessionState
{
	NSMutableArray* history = [NSMutableArray array];
	NSUInteger from = _history.count > 5 ? _history.count - 5 : 0;
	for(NSUInteger i = from; i < _history.count; ++i)
	{
		NSDictionary* record = _history[i];
		NSNumber* scrollOffset = i == _historyIndex ? @(NSMinY(self.outlineView.visibleRect)) : record[@"scrollOffset"];
		if(scrollOffset && scrollOffset.doubleValue > 0)
		{
			[history addObject:@{
				@"url":          [record[@"url"] absoluteString],
				@"scrollOffset": scrollOffset,
			}];
		}
		else
		{
			[history addObject:@{ @"url": [record[@"url"] absoluteString], }];
		}
	}

	return @{
		@"history":      history,
		@"historyIndex": @(_historyIndex - from),
		@"selection":    [self.fileBrowserView.selectedURLs.allObjects valueForKeyPath:@"absoluteString"] ?: @[ ],
		@"expanded":     [self.fileBrowserView.expandedURLs.allObjects valueForKeyPath:@"absoluteString"] ?: @[ ],
		@"showHidden":   @(self.fileBrowserView.showExcludedItems),
	};
}

- (void)setupViewWithState:(NSDictionary*)fileBrowserState
{
	self.fileBrowserView.showExcludedItems = [fileBrowserState[@"showHidden"] boolValue];

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
		self.historyIndex = oak::cap<NSUInteger>(0, [fileBrowserState[@"historyIndex"] intValue], newHistory.count);

		NSMutableArray<NSURL*>* expandedURLs = [NSMutableArray array];
		for(NSString* urlString in fileBrowserState[@"expanded"])
			[expandedURLs addObject:[NSURL URLWithString:urlString]];

		NSMutableArray<NSURL*>* selectedURLs = [NSMutableArray array];
		for(NSString* urlString in fileBrowserState[@"selection"])
			[selectedURLs addObject:[NSURL URLWithString:urlString]];

		[self.fileBrowserView expandURLs:expandedURLs selectURLs:selectedURLs];
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
	for(NSInteger i = 0; i < self.fileBrowserView.outlineView.numberOfRows; ++i)
	{
		FileItem* item = [self.fileBrowserView.outlineView itemAtRow:i];
		if([url isEqual:item.URL])
		{
			[self.fileBrowserView.outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
			[self.fileBrowserView centerSelectionInVisibleArea:self];
			return;
		}
	}

	NSURL* currentParent = self.fileBrowserView.URL;
	NSMutableSet<NSURL*>* expandURLs = [self.fileBrowserView.expandedURLs mutableCopy];

	NSURL* childURL = url;
	while(true)
	{
		NSNumber* flag;
		if([childURL getResourceValue:&flag forKey:NSURLIsVolumeKey error:nil] && [flag boolValue])
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
		[self.fileBrowserView expandURLs:expandURLs.allObjects selectURLs:@[ url ]];
	}
	else
	{
		[self goToURL:url.URLByDeletingLastPathComponent];
		[self.fileBrowserView expandURLs:nil selectURLs:@[ url ]];
	}
}

- (void)reload:(id)sender
{
	[self.fileBrowserView reload:self];
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
	panel.directoryURL            = self.fileBrowserView.URL.filePathURL;

	[panel beginSheetModalForWindow:self.view.window completionHandler:^(NSInteger result) {
		if(result == NSFileHandlingPanelOKButton)
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
@end
