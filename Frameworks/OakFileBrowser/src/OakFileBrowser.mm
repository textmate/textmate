#import "OakFileBrowser.h"
#import "OakFSUtilities.h"
#import "OakHistoryController.h"
#import "ui/OakFileBrowserView.h"
#import "ui/OFBOutlineView.h"
#import "ui/OFBPathInfoCell.h"
#import "io/FSDataSource.h"
#import "io/FSSCMDataSource.h"
#import "io/FSItem.h"
#import "FSOutlineViewDelegate.h"
#import <Preferences/Keys.h>
#import <io/io.h>
#import <oak/oak.h>
#import <io/entries.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakFinderLabelChooser.h>
#import <OakAppKit/OakOpenWithMenu.h>
#import <OakAppKit/OakZoomingIcon.h>
#import <OakAppKit/OakPreview.h>
#import <OakAppKit/NSView Additions.h>
#import <OakAppKit/OakSound.h>
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

@interface OakFileBrowser () <OFBOutlineViewMenuDelegate>
{
	OBJC_WATCH_LEAKS(OakFileBrowser);
}
@property (nonatomic)                    OakHistoryController* historyController;
@property (nonatomic, readwrite, copy)   NSURL* url;
@property (nonatomic)                    FSOutlineViewDelegate* outlineViewDelegate;
@property (nonatomic, readwrite)         OakFileBrowserView* view;
@property (nonatomic)                    NSUInteger dataSourceOptions;
@property (nonatomic, readonly)          NSArray* selectedItems;
@property (nonatomic, readonly)          NSArray* selectedPaths;
- (void)updateView;
- (void)loadFileBrowserOptions;
@end

static NSString* const kUserDefaultsFileBrowserDataSourceOptions = @"FileBrowser DataSourceOptions";

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
- (BOOL)acceptsFirstResponder { return NO; }
- (NSString*)location
{
	NSURL* tmp = [[_url scheme] isEqualToString:@"scm"] ? ParentForURL(_url) : _url;
	return [tmp isFileURL] ? [tmp path] : nil;
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	NSIndexSet* indexSet = [_view.outlineView selectedRowIndexes];
	for(NSUInteger index = [indexSet firstIndex]; index != NSNotFound; index = [indexSet indexGreaterThanIndex:index])
		[res addObject:[_view.outlineView itemAtRow:index]];
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

- (void)reload:(id)sender
{
	[_historyController setCurrentURLScrollOffset:NSMinY([_view.outlineView visibleRect])];
	_outlineViewDelegate.dataSource = DataSourceForURL(_url, _dataSourceOptions);
	[_outlineViewDelegate scrollToOffset:_historyController.currentURLScrollOffset];
}

- (void)setURL:(NSURL*)aURL
{
	if(_outlineViewDelegate.dataSource && [_url isEqualTo:aURL])
		return;

	[_historyController setCurrentURLScrollOffset:NSMinY([_view.outlineView visibleRect])];
	_url = aURL;
	_outlineViewDelegate.dataSource = DataSourceForURL(aURL, _dataSourceOptions);
	[self updateView];
}

- (void)pushURL:(NSURL*)aURL
{
	if(_outlineViewDelegate.dataSource && [_url isEqualTo:aURL])
		return;

	[self setURL:aURL];
	[_historyController addURLToHistory:aURL];
	[self updateView];
}

- (void)showURL:(NSURL*)aURL
{
	D(DBF_FileBrowser_Controller, bug("url: %s\n", [[aURL absoluteString] UTF8String]););
	if(![aURL isFileURL] || [[aURL path] isDirectory])
	{
		[self pushURL:aURL];
	}
	else
	{
		BOOL alreadyVisible = NO;
		for(NSInteger row = 0; !alreadyVisible && row < [_view.outlineView numberOfRows]; ++row)
			alreadyVisible = [aURL isEqualTo:[[_view.outlineView itemAtRow:row] url]];
		if(!alreadyVisible)
			[self pushURL:ParentForURL(aURL)];

		[_outlineViewDelegate selectURLs:@[ aURL ] byExpandingAncestors:NO];
	}
}

- (void)revealURL:(NSURL*)aURL
{
	for(NSURL* currentURL = ParentForURL(aURL); currentURL; currentURL = ParentForURL(currentURL))
	{
		if([_url isEqual:currentURL])
		{
			[_outlineViewDelegate selectURLs:@[ aURL ] byExpandingAncestors:YES];
			return;
		}
	}
	[self showURL:aURL];
}

- (void)deselectAll:(id)sender
{
	[_view.outlineView deselectAll:sender];
}

- (NSArray*)openURLs
{
	return _outlineViewDelegate.openURLs;
}

- (void)setOpenURLs:(NSArray*)newOpenURLs
{
	if(!settings_for_path(NULL_STR, "", to_s(self.location)).get(kSettingsFileBrowserDocumentStatusKey, true))
		return;
	
	if([_outlineViewDelegate.openURLs isEqualToArray:newOpenURLs])
		return;

	NSSet* symmetricDifference = SymmetricDifference([NSMutableSet setWithArray:_outlineViewDelegate.openURLs], [NSMutableSet setWithArray:newOpenURLs]);

	// make a note of files in view, with changed open state
	NSIndexSet* updateRows = [self indexSetforURLs:symmetricDifference];

	_outlineViewDelegate.openURLs = newOpenURLs;

	// make sure all items are accounted for
	// if the counts are equal, all items are in view and no need re-index folders
	if([updateRows count] == [symmetricDifference count])
			[_view.outlineView reloadDataForRowIndexes:updateRows columnIndexes:[NSIndexSet indexSetWithIndex:0]];
	else	[_view.outlineView reloadData];
}

- (NSArray*)modifiedURLs
{
	return _outlineViewDelegate.modifiedURLs;
}

- (void)setModifiedURLs:(NSArray*)newModifiedURLs
{
	if(!settings_for_path(NULL_STR, "", to_s(self.location)).get(kSettingsFileBrowserDocumentStatusKey, true))
		return;
	
	if([_outlineViewDelegate.modifiedURLs isEqualToArray:newModifiedURLs])
		return;

	NSSet* symmetricDifference = SymmetricDifference([NSMutableSet setWithArray:_outlineViewDelegate.modifiedURLs], [NSMutableSet setWithArray:newModifiedURLs]);

	// make a note of files in view, with changed open state
	NSIndexSet* updateRows = [self indexSetforURLs:symmetricDifference];
	_outlineViewDelegate.modifiedURLs = newModifiedURLs;

	// make sure all items are accounted for
	// if the counts are equal, all items are in view and no need re-index folders
	if([updateRows count] == [symmetricDifference count])
			[_view.outlineView reloadDataForRowIndexes:updateRows columnIndexes:[NSIndexSet indexSetWithIndex:0]];
	else	[_view.outlineView reloadData];
}

- (NSIndexSet*)indexSetforURLs:(NSSet*)urls
{
	// make a note of files in view, with changed open state
	NSMutableIndexSet* updateRows = [NSMutableIndexSet indexSet];
	NSInteger len = [_view.outlineView numberOfRows];
	for(int rowIndex = 0; rowIndex < len ; rowIndex++)
	{
		NSURL* file = [[_view.outlineView itemAtRow:rowIndex] url];
		if([urls containsObject:file])
		{
			[updateRows addIndex:rowIndex];
		}
	}
	return updateRows;
}
// ======================
// = History Controller =
// ======================

- (NSDictionary*)sessionState
{
	if(_view.outlineView)
		[_historyController setCurrentURLScrollOffset:NSMinY([_view.outlineView visibleRect])];
	return _historyController.state;
}

// ====================
// = Browsing Actions =
// ====================

- (IBAction)didDoubleClickOutlineView:(id)sender
{
	NSArray* items = _view.outlineView.clickedRow != -1 ? @[ [_view.outlineView itemAtRow:_view.outlineView.clickedRow] ] : self.selectedItems;

	NSMutableArray* urlsToOpen     = [NSMutableArray array];
	NSMutableArray* itemsToAnimate = [NSMutableArray array];

	for(FSItem* item in items)
	{
		NSURL* itemURL = item.target ?: item.url;

		FSItemURLType type = item.urlType;
		if(type == FSItemURLTypePackage && OakIsAlternateKeyOrMouseEvent())
			type = FSItemURLTypeFolder;
		else if(type == FSItemURLTypeFile && is_binary([itemURL.path fileSystemRepresentation]))
			type = FSItemURLTypePackage;
		else if(type == FSItemURLTypeAlias)
		{
			FSItem* tmp = [FSItem itemWithURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:path::resolve([itemURL.path fileSystemRepresentation])]]];
			type    = tmp.urlType;
			itemURL = tmp.target ?: tmp.url;
		}

		switch(type)
		{
			case FSItemURLTypeFolder:
			case FSItemURLTypeUnknown:
				return [self pushURL:itemURL];
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
		[OakZoomingIcon zoomIcon:[OakFileIconImage fileIconImageWithPath:item.path size:NSMakeSize(128, 128)] fromRect:[_view iconFrameForEntry:item]];
	if([urlsToOpen count])
		[_delegate fileBrowser:self openURLs:urlsToOpen];
}

- (IBAction)didSingleClickOutlineView:(id)sender
{
	NSInteger row = [_view.outlineView clickedRow];
	NSInteger col = [_view.outlineView clickedColumn];
	col = row != -1 && col == -1 ? 0 : col; // Clicking a row which participates in multi-row selection causes clickedColumn to return -1 <rdar://10382268>
	OFBPathInfoCell* cell = (OFBPathInfoCell*)[_view.outlineView preparedCellAtColumn:col row:row];
	NSInteger hit = [cell hitTestForEvent:[NSApp currentEvent] inRect:[_view.outlineView frameOfCellAtColumn:col row:row] ofView:_view.outlineView];
	if(hit & OakImageAndTextCellHitImage)
	{
		NSURL* itemURL = ((FSItem*)[_view.outlineView itemAtRow:row]).url;
		
		if(([[NSApp currentEvent] modifierFlags] & NSCommandKeyMask) && [itemURL isFileURL])
			[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[ itemURL ]];
		else	[self didDoubleClickOutlineView:sender];
	}
	else if(hit & OFBPathInfoCellHitCloseButton)
	{
		FSItem* item = [_view.outlineView itemAtRow:row];
		[_delegate fileBrowser:self closeURL:item.url];
	}
}

// =================
// = Menu Delegate =
// =================

- (BOOL)canUndo { return NO; }
- (BOOL)canRedo { return NO; }

- (void)editSelectedEntries:(id)sender { [_view.outlineView performEditSelectedRow:self]; }

- (void)duplicateSelectedEntries:(id)sender
{
	NSMutableArray* duplicatedURLs = [NSMutableArray array];
	for(NSString* aPath in self.selectedPaths)
	{
		std::string const& dupPath = path::duplicate([aPath fileSystemRepresentation]);
		if(dupPath != NULL_STR)
				[duplicatedURLs addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:dupPath]]];
		else	OakRunIOAlertPanel("Failed to duplicate the file at “%s”.", [aPath fileSystemRepresentation]);
	}

	if([duplicatedURLs count])
	{
		OakPlayUISound(OakSoundDidMoveItemUISound);
		if([duplicatedURLs count] == 1)
				[_outlineViewDelegate editURL:[duplicatedURLs lastObject]];
		else	[_outlineViewDelegate selectURLs:duplicatedURLs byExpandingAncestors:NO];
	}
}

- (void)revealSelectedItem:(id)sender
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.target isFileURL])
		{
			[self pushURL:ParentForURL(item.target)];
			[_outlineViewDelegate selectURLs:@[ item.target ] byExpandingAncestors:NO];
			return;
		}
	}
}

- (void)showPackageContents:(id)sender
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.target isFileURL])
			return (void)[self showURL:item.target];
	}

	for(FSItem* item in self.selectedItems)
	{
		if([item.target path] && path::is_directory([[item.target path] fileSystemRepresentation]))
			return (void)[self showURL:[NSURL fileURLWithPath:[item.target path]]];
	}
}

- (void)showSelectedEntriesInFinder:(id)sender
{
	for(FSItem* item in self.selectedItems)
		[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[ item.url ]];
}

- (NSString*)parentForNewFolder
{
	NSMutableSet* folders = [NSMutableSet set];
	for(FSItem* item in self.selectedItems)
	{
		if(![item.url isFileURL])
			continue; // Perhaps we shouldn’t consider the selection if we encounter a non-file URL

		if(!item.leaf && [_view.outlineView isItemExpanded:item])
			[folders addObject:item.path];
		else if([_url isFileURL]) // TODO Test if parent folder is actually shown by current data source
			[folders addObject:[item.path stringByDeletingLastPathComponent]];
	}
	return [folders count] == 1 ? [folders anyObject] : ([_url isFileURL] ? [_url path] : nil);
}

- (void)newFolderInSelectedFolder:(id)sender
{
	if(NSString* folder = [self parentForNewFolder])
	{
		std::string const dst = path::unique(path::join([folder fileSystemRepresentation], "untitled folder"));
		if(path::make_dir(dst))
				[_outlineViewDelegate editURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]]];
		else	OakRunIOAlertPanel("Failed to create new folder in “%s”.", path::parent([folder fileSystemRepresentation]).c_str());
	}
}

- (void)delete:(id)anArgument
{
	BOOL didTrashSomething = NO;
	for(NSString* aPath in self.selectedPaths)
	{
		std::string const trashPath = path::move_to_trash([aPath fileSystemRepresentation]);
		if(trashPath != NULL_STR)
				didTrashSomething = YES;
		else	OakRunIOAlertPanel("Failed to move the file at “%s” to the trash.", [aPath fileSystemRepresentation]);
	}

	if(didTrashSomething)
		OakPlayUISound(OakSoundDidTrashItemUISound);
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

	NSArray* paths = self.selectedPaths;
	if(![paths count] && [_url isFileURL])
		paths = @[ [_url path] ];

	for(NSString* aPath in paths)
	{
		std::string const src = [aPath fileSystemRepresentation];
		std::string const dst = path::join(favFolder, path::name(src));
		path::link(src, dst);
	}
}

- (void)updateVariables:(std::map<std::string, std::string>&)env
{
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

	if(NSString* dir = self.location)
		env["PWD"] = [dir fileSystemRepresentation];
}

- (void)executeBundleCommand:(id)sender
{
	std::map<std::string, std::string> map;
	[self updateVariables:map];

	if(bundles::item_ptr item = bundles::lookup(to_s((NSString*)[sender representedObject])))
		document::run(parse_command(item), ng::buffer_t(), ng::ranges_t(), [self.selectedPaths count] == 1 ? document::create(map["TM_SELECTED_FILE"]) : document::document_ptr(), map);
}

- (NSRect)previewPanel:(NSPanel*)panel frameForURL:(NSURL*)aURL
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.url isEqual:aURL])
			return [_view iconFrameForEntry:item];
	}
	return NSZeroRect;
}

- (void)quickLookSelectedEntries:(id)sender
{
	NSMutableArray* urls = [NSMutableArray array];
	for(NSURL* aURL in self.selectedURLs)
	{
		if([aURL isFileURL])
			[urls addObject:aURL];
	}
	OakShowPreviewForURLs(urls);
}

- (void)cut:(id)sender
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

- (void)copy:(id)sender
{
	[self writeSelectionToPasteboard:[NSPasteboard generalPasteboard] types:nil];
}

- (void)paste:(id)sender
{
	NSMutableArray* created = [NSMutableArray array];
	if(NSString* folder = [self parentForNewFolder])
	{
		NSPasteboard* pboard = [NSPasteboard generalPasteboard];
		BOOL cut = [[pboard availableTypeFromArray:@[ @"OakFileBrowserOperation" ]] isEqualToString:@"OakFileBrowserOperation"] && [[pboard stringForType:@"OakFileBrowserOperation"] isEqualToString:@"cut"];
		for(NSString* path in [pboard availableTypeFromArray:@[ NSFilenamesPboardType ]] ? [pboard propertyListForType:NSFilenamesPboardType] : nil)
		{
			std::string const src = [path fileSystemRepresentation];
			std::string const dst = path::unique(path::join([folder fileSystemRepresentation], path::name(src)));
			if(cut ? path::rename(src, dst) : path::copy(src, dst))
				[created addObject:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]]];
		}
	}

	if([created count] > 0)
	{
		OakPlayUISound(OakSoundDidMoveItemUISound);
		[_outlineViewDelegate selectURLs:created byExpandingAncestors:NO];
	}
}

- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView
{
	NSMenu* menu = [NSMenu new];
	[menu setAutoenablesItems:NO];

	NSInteger numberOfSelectedRows = [anOutlineView numberOfSelectedRows];
	if(numberOfSelectedRows == 0)
	{
		if([_url isFileURL])
		{
			[menu addItemWithTitle:@"New Folder" action:@selector(newFolderInSelectedFolder:) keyEquivalent:@""];
			[menu addItemWithTitle:@"Add to Favorites" action:@selector(addSelectedEntriesToFavorites:) keyEquivalent:@""];
		}
	}
	else
	{
		BOOL pathsExist   = YES;
		BOOL showOpenWith = YES;
		for(FSItem* item in self.selectedItems)
		{
			pathsExist   = pathsExist   && !([item.url isFileURL] && !path::exists([[item.url path] fileSystemRepresentation]));
			showOpenWith = showOpenWith && !([(item.target ?: item.url) isFileURL] && (path::info([[(item.target ?: item.url) path] fileSystemRepresentation]) & path::flag::application));
		}

		BOOL singleItem = [self.selectedItems count] == 1;
		FSItem* item = singleItem ? [self.selectedItems lastObject] : nil;

		BOOL showEnclosingFolder = item && [item.url isFileURL] && [@[ @"search", @"scm" ] containsObject:[_url scheme]];
		BOOL showPackageContents = item && [item.url isFileURL] && (path::info([item.path fileSystemRepresentation]) & path::flag::package);
		BOOL showOriginal        = item && [item.url isFileURL] && (path::info([item.path fileSystemRepresentation]) & (path::flag::symlink|path::flag::alias));
		BOOL canCreateFolder     = [self parentForNewFolder] ? YES : NO;

		struct { NSString* label; SEL action; BOOL enable; BOOL include; } const menuLabels[] =
		{
			{ @"Open",                    @selector(didDoubleClickOutlineView:),                  pathsExist, YES },
			{ @"Open With",               NULL,                                                   pathsExist, showOpenWith },
			{ @"Show Preview",            @selector(quickLookSelectedEntries:),     singleItem && pathsExist, YES },
			{ nil,                        NULL,                                                          YES, YES },
			{ @"Open Enclosing Folder",   @selector(revealSelectedItem:),                                YES, showEnclosingFolder }, // scm://, search://
			{ @"Show Package Contents",   @selector(showPackageContents:),                               YES, showPackageContents }, // .app, .tmBundle, …
			{ @"Show Original",           @selector(revealSelectedItem:),                                YES, showOriginal        }, // symbolic links, aliases
			{ @"Show in Finder",          @selector(showSelectedEntriesInFinder:),  singleItem && pathsExist, YES },
			{ nil,                        NULL,                                                          YES, YES },
			{ @"Rename",                  @selector(editSelectedEntries:),          singleItem && pathsExist, YES },
			{ @"Duplicate",               @selector(duplicateSelectedEntries:),                   pathsExist, YES },
			{ @"New Folder",              @selector(newFolderInSelectedFolder:),             canCreateFolder, YES },
			{ nil,                        NULL,                                                          YES, YES },
			{ @"Move to Trash",           @selector(delete:),                                     pathsExist, YES },
			{ nil,                        NULL,                                                          YES, YES },
			{ @"Copy",                    @selector(copy:),                                              YES, YES },
			{ nil,                        NULL,                                                          YES, YES },
			{ @"Color Label",             NULL,                                                   pathsExist, YES },
			{ @"Add to Favorites",        @selector(addSelectedEntriesToFavorites:),              pathsExist, YES },
		};

		for(size_t i = 0; i < sizeofA(menuLabels); i++)
		{
			if(!menuLabels[i].include)
				continue;

			if(NSString* label = menuLabels[i].label)
			{
				NSMenuItem* menuItem = [menu addItemWithTitle:label action:menuLabels[i].action keyEquivalent:@""];
				[menuItem setTarget:self];
				if(!menuLabels[i].enable || menuLabels[i].action && ![self respondsToSelector:menuLabels[i].action])
					[menuItem setEnabled:NO];
			}
			else
			{
				[menu addItem:[NSMenuItem separatorItem]];
			}
		}

		std::vector<bundles::item_ptr> const& items = bundles::query(bundles::kFieldSemanticClass, "callback.file-browser.action-menu");
		if(!items.empty())
		{
			NSInteger i = [menu indexOfItemWithTitle:@"Move to Trash"];
			[menu insertItem:[NSMenuItem separatorItem] atIndex:++i];

			std::multimap<std::string, bundles::item_ptr, text::less_t> sorted;
			iterate(item, items)
				sorted.insert(std::make_pair((*item)->name(), *item));

			iterate(pair, sorted)
			{
				NSMenuItem* item = [[NSMenuItem alloc] initWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(executeBundleCommand:) keyEquivalent:@""];
				item.representedObject = [NSString stringWithCxxString:pair->second->uuid()];
				item.target            = self;
				[menu insertItem:item atIndex:++i];
			}
		}

		OakFinderLabelChooser* swatch = [[OakFinderLabelChooser alloc] initWithFrame:NSMakeRect(0, 0, 166, 37)];
		swatch.selectedIndex          = numberOfSelectedRows == 1 ? [[self.selectedItems lastObject] labelIndex] : 0;
		swatch.target                 = self;
		swatch.action                 = @selector(changeColor:);
		[[menu itemWithTitle:@"Color Label"] setView:swatch];

		if(NSMenuItem* openWithMenuItem = [menu itemWithTitle:@"Open With"])
			[OakOpenWithMenu addOpenWithMenuForPaths:[NSSet setWithArray:self.selectedPaths] toMenuItem:openWithMenuItem];
	}

	// [menu addItem:[NSMenuItem separatorItem]];
	// if([self canUndo] || [self canRedo])
	// 		[[menu addItemWithTitle:[NSString stringWithCxxString:title_for_operation(/**undoOperation*/)] action:([self canUndo] ? @selector(undo:) : @selector(redo:)) keyEquivalent:@""] setTarget:self];
	// else	[[menu addItemWithTitle:@"Can’t Undo" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];

	return menu;
}

// ==================
// = Setup/Teardown =
// ==================

- (id)init
{
	if(self = [super init])
	{
		NSString* urlString = [[NSUserDefaults standardUserDefaults] stringForKey:kUserDefaultsInitialFileBrowserURLKey];

		self.url                = urlString ? [NSURL URLWithString:urlString] : kURLLocationHome;
		self.historyController  = [OakHistoryController new];
		[self loadFileBrowserOptions];

		BOOL foldersOnTop   = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFoldersOnTopKey];
		BOOL showExtensions = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFileExtensionsKey];

		_dataSourceOptions |= (foldersOnTop   ? kFSDataSourceOptionGroupsFirst   : 0);
		_dataSourceOptions |= (showExtensions ? kFSDataSourceOptionShowExtension : 0);

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
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

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)setView:(OakFileBrowserView*)aView
{
	if(aView != _view)
	{
		if(_view)
		{
			_outlineViewDelegate = nil;

			if(_view.delegate == self)
				_view.delegate = nil;
			if(_view.persistentNextResponder == self)
				_view.persistentNextResponder = nil;
		}

		if(_view = aView)
		{
			_view.delegate = self;
			_view.persistentNextResponder = self;

			_outlineViewDelegate = [FSOutlineViewDelegate new];
			_outlineViewDelegate.outlineView = _view.outlineView;

			_view.outlineView.target       = self;
			_view.outlineView.action       = @selector(didSingleClickOutlineView:);
			_view.outlineView.doubleAction = @selector(didDoubleClickOutlineView:);
			_view.outlineView.menuDelegate = self;

			[_view.outlineView setDraggingSourceOperationMask:NSDragOperationCopy|NSDragOperationMove|NSDragOperationLink forLocal:YES];
			[_view.outlineView setDraggingSourceOperationMask:NSDragOperationEvery forLocal:NO];
			[_view.outlineView registerForDraggedTypes:@[ NSFilenamesPboardType ]];

			[self updateView];
		}
	}
}

- (void)setupViewWithState:(NSDictionary*)fileBrowserState
{
	self.view = [[OakFileBrowserView alloc] initWithFrame:NSZeroRect];
	_historyController.state = fileBrowserState;
	if(!_historyController.currentURL)
		[_historyController addURLToHistory:_url];
	CGFloat scrollOffset = _historyController.currentURLScrollOffset;
	[self setURL:_historyController.currentURL];
	[_historyController setCurrentURLScrollOffset:scrollOffset];
	[_outlineViewDelegate scrollToOffset:_historyController.currentURLScrollOffset];
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
	return [_view.outlineView.dataSource outlineView:_view.outlineView writeItems:self.selectedItems toPasteboard:pboard];
}

// ================
// = View Options =
// ================

static struct data_source_options_map_t { NSString* const name; NSUInteger flag; } const DataSourceOptionsMap[] =
{
	{ @"Show Hidden Items",      kFSDataSourceOptionIncludeHidden },
	{ @"Sort by File Extension", kFSDataSourceOptionSortByType    },
};

- (void)loadFileBrowserOptions
{
	NSArray* array = [[NSUserDefaults standardUserDefaults] arrayForKey:kUserDefaultsFileBrowserDataSourceOptions];
	_dataSourceOptions = 0;
	iterate(it, DataSourceOptionsMap)
	{
		if([array containsObject:it->name])
			_dataSourceOptions |= it->flag;
	}
}

- (void)saveFileBrowserOptions
{
	NSMutableArray* array = [NSMutableArray array];
	iterate(it, DataSourceOptionsMap)
	{
		if((_dataSourceOptions & it->flag) == it->flag)
			[array addObject:it->name];
	}

	if([array count] == 0)
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsFileBrowserDataSourceOptions];
	else	[[NSUserDefaults standardUserDefaults] setObject:array forKey:kUserDefaultsFileBrowserDataSourceOptions];
}

- (IBAction)toggleViewOption:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	_dataSourceOptions ^= [sender tag];
	[self saveFileBrowserOptions];
	[self reload:self];
}

- (IBAction)showFolderSpecificPreferences:(id)sender
{
	[_delegate fileBrowser:self openURLs:@[ [NSURL fileURLWithPath:[[_url path] stringByAppendingPathComponent:@".tm_properties"] isDirectory:NO]] ];
}

- (IBAction)showOptionsPopUpMenu:(id)sender
{
	NSMenu* menu = [NSMenu new];
	iterate(it, DataSourceOptionsMap)
	{
		NSMenuItem* item = [menu addItemWithTitle:it->name action:@selector(toggleViewOption:) keyEquivalent:@""];
		[item setState:(((_dataSourceOptions & it->flag) == it->flag) ? NSOnState : NSOffState)];
		[item setTarget:self];
		[item setTag:it->flag];
	}

	[menu addItem:[NSMenuItem separatorItem]];
	NSMenuItem* menuItem = [menu addItemWithTitle:@"Reload" action:@selector(reload:) keyEquivalent:@""];
	[menuItem setTarget:self];

	if([_url isFileURL])
	{
		[menu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* menuItem = [menu addItemWithTitle:@"Preferences…" action:@selector(showFolderSpecificPreferences:) keyEquivalent:@""];
		[menuItem setTarget:self];
	}

	[_view displayMenu:menu fromHeaderColumn:fb::options selectedIndex:0 popup:NO];
}

// =======================
// = Header View Actions =
// =======================

- (void)updateView
{
	_view.titleText          = _outlineViewDelegate.dataSource.rootItem.name;
	_view.titleImage         = _outlineViewDelegate.dataSource.rootItem.icon;
	_view.canGoBackward      = _historyController.previousURL ? YES : NO;
	_view.canGoForward       = _historyController.nextURL     ? YES : NO;
	[_view setNeedsDisplay:YES];
}

- (IBAction)goToComputer:(id)sender       { [self pushURL:kURLLocationComputer];  }
- (IBAction)goToHome:(id)sender           { [self pushURL:kURLLocationHome];      }
- (IBAction)goToDesktop:(id)sender        { [self pushURL:kURLLocationDesktop];   }
- (IBAction)goToFavorites:(id)sender      { [self pushURL:kURLLocationFavorites]; }

- (IBAction)goToSCMDataSource:(id)sender
{
	if([_url.scheme isEqualToString:@"scm"])
	{
		if(_historyController.previousURL)
				[self goBack:sender];
		else	[self goToParentFolder:sender];
	}
	else
	{
		for(NSURL* selectedURL in self.selectedURLs)
		{
			if([selectedURL isFileURL] && path::is_directory([[selectedURL path] fileSystemRepresentation]))
				return [self pushURL:[FSSCMDataSource scmURLWithPath:[selectedURL path]]];
		}
		[self pushURL:[FSSCMDataSource scmURLWithPath:[_url path]]];
	}
}

- (IBAction)goBack:(id)sender             { if(_historyController.previousURL) { [self setURL:_historyController.previousURL]; [_historyController retreat:self]; [_outlineViewDelegate scrollToOffset:_historyController.currentURLScrollOffset]; [self updateView]; } }
- (IBAction)goForward:(id)sender          { if(_historyController.nextURL)     { [self setURL:_historyController.nextURL];     [_historyController advance:self]; [_outlineViewDelegate scrollToOffset:_historyController.currentURLScrollOffset]; [self updateView]; } }

- (IBAction)goToParentFolder:(id)sender
{
	[self pushURL:ParentForURL(_url)];
}

- (IBAction)orderFrontGoToFolder:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setCanChooseFiles:NO];
	[panel setCanChooseDirectories:YES];
	[panel setAllowsMultipleSelection:NO];
	[panel setDirectoryURL:[NSURL fileURLWithPath:self.location]];
	[panel beginSheetModalForWindow:_view.window completionHandler:^(NSInteger result) {
		if(result == NSOKButton)
			[self showURL:[[panel URLs] lastObject]];
	}];
}

- (void)takeURLFrom:(id)sender
{
	if([sender representedObject])
		[self pushURL:[sender representedObject]];
}

- (IBAction)showFolderPopUpMenu:(id)sender
{
	NSMenu* menu                   = [NSMenu new];
	NSMutableSet* visibleLocations = [NSMutableSet setWithObjects:kURLLocationComputer, kURLLocationHome, kURLLocationFavorites, nil];

	// Add path hierarchy
	for(NSURL* currentURL = _url; currentURL; currentURL = ParentForURL(currentURL))
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:DisplayName(currentURL) action:@selector(takeURLFrom:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:currentURL];
		[menuItem setImage:IconImage(currentURL)];
		[visibleLocations addObject:currentURL];
	}

	// Add recent locations
	NSMutableArray* recentURLs = [NSMutableArray array];
	for(NSUInteger index = 0; index < _historyController.recentLocations.count && index < 10; ++index)
	{
		NSURL* recentURL = [_historyController.recentLocations objectAtIndex:index];
		if(![visibleLocations containsObject:recentURL])
			[recentURLs addObject:recentURL];
	}

	if([recentURLs count])
	{
		[menu addItem:[NSMenuItem separatorItem]];
		[[menu addItemWithTitle:@"Recent Places" action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];

		// TODO Disambiguate paths
		// std::vector<size_t> const& parents = path::disambiguate(recentPaths);
		for(NSURL* recentURL in recentURLs)
		{
			NSMenuItem* menuItem = [menu addItemWithTitle:DisplayName(recentURL/*, parents[index]*/) action:@selector(takeURLFrom:) keyEquivalent:@""];
			[menuItem setTarget:self];
			[menuItem setRepresentedObject:recentURL];
			[menuItem setImage:IconImage(recentURL)];
		}
	}

	[menu addItem:[NSMenuItem separatorItem]];
	[[menu addItemWithTitle:@"Other…" action:@selector(orderFrontGoToFolder:) keyEquivalent:@""] setTarget:self];

	[_view displayMenu:menu fromHeaderColumn:fb::title selectedIndex:0 popup:YES];
}

- (IBAction)didClickHeaderColumn:(id)sender
{
	switch([sender tag])
	{
		case fb::goBack:           return [self goBack:sender];
		case fb::goForward:        return [self goForward:sender];
		case fb::scmDataSource:    return [self goToSCMDataSource:sender];
		case fb::favorites:        return [self goToFavorites:sender];
		case fb::home:             return [self goToHome:sender];
		case fb::computer:         return [self goToComputer:sender];
		case fb::title:            return [self showFolderPopUpMenu:sender];
		case fb::options:          return [self showOptionsPopUpMenu:sender];
	}
}

- (IBAction)takeHistoryIndexFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	[self setURL:[_historyController urlAtIndex:[sender tag]]];
	_historyController.historyIndex = [sender tag];
	[_outlineViewDelegate scrollToOffset:_historyController.currentURLScrollOffset];
	[self updateView];
}

- (void)showHistoryItems:(NSArray*)urls fromHeaderColumn:(fb::header_column)columnTag
{
	// TODO DisplayName paths
	NSMutableSet* seenPaths = [NSMutableSet set];
	NSMenu* menu = [NSMenu new];
	for(NSURL* aURL in urls)
	{
		if([seenPaths containsObject:[aURL path]])
			continue;
		if([aURL isFileURL])
			[seenPaths addObject:[aURL path]];

		NSMenuItem* menuItem = [menu addItemWithTitle:DisplayName(aURL) action:@selector(takeHistoryIndexFrom:) keyEquivalent:@""];
		[menuItem setTag:[[aURL fragment] intValue]];
		[menuItem setTarget:self];
		[menuItem setImage:IconImage(aURL)];
	}
	[_view displayMenu:menu fromHeaderColumn:columnTag selectedIndex:0 popup:NO];
}

- (IBAction)showBackMenu:(id)sender
{
	NSMutableArray* urls = [NSMutableArray array];
	for(NSInteger historyIndex = _historyController.historyIndex - 1; historyIndex >= 0 && _historyController.historyIndex - historyIndex <= 10; --historyIndex)
		[urls addObject:[NSURL URLWithString:[[[_historyController urlAtIndex:historyIndex] absoluteString] stringByAppendingFormat:@"#%ld", historyIndex]]];
	[self showHistoryItems:urls fromHeaderColumn:fb::goBack];
}

- (IBAction)showForwardMenu:(id)sender
{
	NSMutableArray* urls = [NSMutableArray array];
	for(NSInteger historyIndex = _historyController.historyIndex + 1; historyIndex < _historyController.historyCount && historyIndex - _historyController.historyIndex <= 10; ++historyIndex)
		[urls addObject:[NSURL URLWithString:[[[_historyController urlAtIndex:historyIndex] absoluteString] stringByAppendingFormat:@"#%ld", historyIndex]]];
	[self showHistoryItems:urls fromHeaderColumn:fb::goForward];
}

- (void)showFolderContents:(NSURL*)aURL inMenuFromCell:(fb::header_column)column
{
	std::multimap<std::string, NSURL*, text::less_t> urls;
	if([[aURL scheme] isEqualToString:[kURLLocationComputer scheme]])
	{
		citerate(volume, path::volumes())
			urls.insert(std::make_pair(path::display_name(*volume), [NSURL fileURLWithPath:[NSString stringWithCxxString:*volume] isDirectory:YES]));
	}
	else if([aURL isFileURL])
	{
		std::string const dir = [[aURL path] fileSystemRepresentation];
		citerate(entry, path::entries(dir, "*"))
		{
			std::string const& path = path::join(dir, (*entry)->d_name);
			uint32_t flags = path::info(path);
			std::string const& resolved = (flags & path::flag::symlink) ? path::resolve(path) : path;
			if((flags & path::flag::directory) || ((flags & path::flag::symlink) && (path::info(resolved) & path::flag::directory)))
				urls.insert(std::make_pair(path::display_name(path), [NSURL fileURLWithPath:[NSString stringWithCxxString:resolved] isDirectory:YES]));
		}
	}

	NSMenu* menu = [NSMenu new];
	iterate(pair, urls)
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(takeURLFrom:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:pair->second];
		[menuItem setImage:IconImage(pair->second)];
	}
	[_view displayMenu:menu fromHeaderColumn:column selectedIndex:0 popup:NO];
}

- (IBAction)didTriggerMenuForHeaderColumn:(id)sender
{
	switch([sender tag])
	{
		case fb::goBack:      return [self showBackMenu:sender];
		case fb::goForward:   return [self showForwardMenu:sender];
		case fb::favorites:   return [self showFolderContents:kURLLocationFavorites inMenuFromCell:fb::header_column([sender tag])];
		case fb::home:        return [self showFolderContents:kURLLocationHome      inMenuFromCell:fb::header_column([sender tag])];
		case fb::computer:    return [self showFolderContents:kURLLocationComputer  inMenuFromCell:fb::header_column([sender tag])];
	}
}

// ===================
// = Menu Validation =
// ===================

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if([item action] == @selector(goToParentFolder:))
			return ParentForURL(_url) != nil;
	else if([item action] == @selector(goBack:))
			return _historyController.previousURL ? YES : NO;
	else if([item action] == @selector(goForward:))
			return _historyController.nextURL ? YES : NO;
	else if([item action] == @selector(delete:))
			return [_view.outlineView numberOfSelectedRows] > 0;
	else if([item action] == @selector(undo:))
			return [self canUndo];
	else if([item action] == @selector(redo:))
			return [self canRedo];
	else	return YES;
}
@end
