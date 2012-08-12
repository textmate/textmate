#import "OakFileBrowser.h"
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
#import <oak/CocoaSTL.h>
#import <oak/oak.h>
#import <io/entries.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakStatusBar.h>
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

OAK_DEBUG_VAR(FileBrowser_Controller);

@interface OakFileBrowser ()
@property (nonatomic, retain)            OakHistoryController* historyController;
@property (nonatomic, copy, readwrite)   NSURL* url;
@property (nonatomic, retain, readwrite) NSView* view;
@property (nonatomic, readonly)          NSArray* selectedItems;
@property (nonatomic, readonly)          NSArray* selectedPaths;
- (void)updateView;
- (void)loadFileBrowserOptions;
@end

static NSString* const kUserDefaultsFileBrowserDataSourceOptions = @"FileBrowser DataSourceOptions";

static NSURL* kURLLocationComputer;
static NSURL* kURLLocationHome;
static NSURL* kURLLocationDesktop;
static NSURL* kURLLocationFavorites;
static NSURL* kURLLocationBundles;

static NSString* DisplayName (NSURL* url, size_t numberOfParents = 0)
{
	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		return [(NSString*)SCDynamicStoreCopyComputerName(NULL, NULL) autorelease];
	else if([[url scheme] isEqualToString:[kURLLocationBundles scheme]])
		return @"Bundles";
	else // if([url isFileURL])
		return [NSString stringWithCxxString:path::display_name([[url path] fileSystemRepresentation], numberOfParents)];
}

static NSImage* IconImage (NSURL* url, NSSize size = (NSSize){16, 16})
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

static NSURL* ParentForURL (NSURL* url)
{
	struct statfs buf;
	NSString* currentPath = [url path];
	NSString* parentPath  = [currentPath stringByDeletingLastPathComponent];

	if([[url scheme] isEqualToString:[kURLLocationComputer scheme]])
		return nil;
	else if([currentPath isEqualToString:parentPath] || [url isFileURL] && statfs([currentPath fileSystemRepresentation], &buf) == 0 && path::normalize(buf.f_mntonname) == path::normalize([currentPath fileSystemRepresentation]))
		return kURLLocationComputer;
	else if([url isFileURL])
		return [NSURL fileURLWithPath:parentPath isDirectory:YES];
	else if([[url scheme] isEqualToString:@"scm"])
		return [NSURL fileURLWithPath:[url path] isDirectory:YES];
	else
		return [[[NSURL alloc] initWithScheme:[url scheme] host:[url host] path:parentPath] autorelease];
}

@implementation OakFileBrowser
@synthesize url, historyController, delegate, view;

- (BOOL)acceptsFirstResponder { return NO; }
- (NSString*)location
{
	NSURL* tmp = [[url scheme] isEqualToString:@"scm"] ? ParentForURL(url) : url;
	return [tmp isFileURL] ? [tmp path] : nil;
}

- (NSArray*)selectedItems
{
	NSMutableArray* res = [NSMutableArray array];
	citerate(index, [view.outlineView selectedRowIndexes])
		[res addObject:[view.outlineView itemAtRow:*index]];
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
	[historyController setCurrentURLScrollOffset:NSMinY([view.outlineView visibleRect])];
	outlineViewDelegate.dataSource = DataSourceForURL(url, dataSourceOptions);
	[outlineViewDelegate scrollToOffset:historyController.currentURLScrollOffset];
}

- (void)setURL:(NSURL*)aURL
{
	if(outlineViewDelegate.dataSource && [self.url isEqualTo:aURL])
		return;

	[historyController setCurrentURLScrollOffset:NSMinY([view.outlineView visibleRect])];
	self.url = aURL;
	outlineViewDelegate.dataSource = DataSourceForURL(aURL, dataSourceOptions);
	[self updateView];
}

- (void)pushURL:(NSURL*)aURL
{
	if(outlineViewDelegate.dataSource && [self.url isEqualTo:aURL])
		return;

	[self setURL:aURL];
	[historyController addURLToHistory:aURL];
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
		for(NSInteger row = 0; !alreadyVisible && row < [view.outlineView numberOfRows]; ++row)
			alreadyVisible = [aURL isEqualTo:[[view.outlineView itemAtRow:row] url]];
		if(!alreadyVisible)
			[self pushURL:ParentForURL(aURL)];

		[outlineViewDelegate selectURLs:@[ aURL ]];
	}
}

- (NSArray*)openURLs
{
	return outlineViewDelegate.openURLs;
}

- (void)setOpenURLs:(NSArray*)newOpenURLs
{
	outlineViewDelegate.openURLs = newOpenURLs;
	[view.outlineView reloadData];
}

- (NSArray*)modifiedURLs
{
	return outlineViewDelegate.modifiedURLs;
}

- (void)setModifiedURLs:(NSArray*)newModifiedURLs
{
	outlineViewDelegate.modifiedURLs = newModifiedURLs;
	[view.outlineView reloadData];
}

// ======================
// = History Controller =
// ======================

- (NSDictionary*)sessionState
{
	if(view.outlineView)
		[historyController setCurrentURLScrollOffset:NSMinY([view.outlineView visibleRect])];
	return historyController.state;
}

// ====================
// = Browsing Actions =
// ====================

- (IBAction)didDoubleClickOutlineView:(id)sender
{
	NSArray* items = view.outlineView.clickedRow != -1 ? @[ [view.outlineView itemAtRow:view.outlineView.clickedRow] ] : self.selectedItems;

	NSMutableArray* urlsToOpen     = [NSMutableArray array];
	NSMutableArray* itemsToAnimate = [NSMutableArray array];

	for(FSItem* item in items)
	{
		NSURL* itemURL = item.target ?: item.url;

		FSItemURLType type = item.urlType;
		if(type == FSItemURLTypePackage && OakIsAlternateKeyOrMouseEvent())
			type = FSItemURLTypeFolder;
		else if(type == FSItemURLTypeFile && document::is_binary([itemURL.path fileSystemRepresentation]))
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
		[OakZoomingIcon zoomIcon:[OakFileIconImage fileIconImageWithPath:item.path size:NSMakeSize(128, 128)] fromRect:[view iconFrameForEntry:item]];
	if([urlsToOpen count])
		[delegate fileBrowser:self openURLs:urlsToOpen];
}

- (IBAction)didSingleClickOutlineView:(id)sender
{
	NSInteger row = [view.outlineView clickedRow];
	NSInteger col = [view.outlineView clickedColumn];
	col = row != -1 && col == -1 ? 0 : col; // Clicking a row which participates in multi-row selection causes clickedColumn to return -1 <rdar://10382268>
	OFBPathInfoCell* cell = (OFBPathInfoCell*)[view.outlineView preparedCellAtColumn:col row:row];
	NSInteger hit = [cell hitTestForEvent:[NSApp currentEvent] inRect:[view.outlineView frameOfCellAtColumn:col row:row] ofView:view.outlineView];
	if((hit & OakImageAndTextCellHitImage) && !([[NSApp currentEvent] modifierFlags] & NSCommandKeyMask))
	{
		[self didDoubleClickOutlineView:sender];
	}
	else if(hit & OFBPathInfoCellHitCloseButton)
	{
		FSItem* item = [view.outlineView itemAtRow:row];
		[delegate fileBrowser:self closeURL:item.url];
	}
}

// =================
// = Menu Delegate =
// =================

- (BOOL)canUndo { return NO; }
- (BOOL)canRedo { return NO; }

- (void)editSelectedEntries:(id)sender { [view.outlineView performEditSelectedRow:self]; }

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
				[outlineViewDelegate editURL:[duplicatedURLs lastObject]];
		else	[outlineViewDelegate selectURLs:duplicatedURLs];
	}
}

- (void)revealSelectedItem:(id)sender
{
	for(FSItem* item in self.selectedItems)
	{
		if([item.target isFileURL])
		{
			[self pushURL:ParentForURL(item.target)];
			[outlineViewDelegate selectURLs:@[ item.target ]];
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
}

- (void)showSelectedEntriesInFinder:(id)sender
{
	for(NSString* aPath in self.selectedPaths)
		[[NSWorkspace sharedWorkspace] selectFile:aPath inFileViewerRootedAtPath:[aPath stringByDeletingLastPathComponent]];
}

- (NSString*)parentForNewFolder
{
	NSMutableSet* folders = [NSMutableSet set];
	for(FSItem* item in self.selectedItems)
	{
		if(![item.url isFileURL])
			continue; // Perhaps we shouldn’t consider the selection if we encounter a non-file URL

		if(!item.leaf && [view.outlineView isItemExpanded:item])
			[folders addObject:item.path];
		else if([url isFileURL]) // TODO Test if parent folder is actually shown by current data source
			[folders addObject:[item.path stringByDeletingLastPathComponent]];
	}
	return [folders count] == 1 ? [folders anyObject] : ([url isFileURL] ? [url path] : nil);
}

- (void)newFolderInSelectedFolder:(id)sender
{
	if(NSString* folder = [self parentForNewFolder])
	{
		std::string const dst = path::unique(path::join([folder fileSystemRepresentation], "untitled folder"));
		if(path::make_dir(dst))
				[outlineViewDelegate editURL:[NSURL fileURLWithPath:[NSString stringWithCxxString:dst]]];
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
	if(![paths count] && [url isFileURL])
		paths = @[ [url path] ];

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
			return [view iconFrameForEntry:item];
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
		[outlineViewDelegate selectURLs:created];
	}
}

- (NSMenu*)menuForOutlineView:(NSOutlineView*)anOutlineView
{
	NSMenu* menu = [[NSMenu new] autorelease];
	[menu setAutoenablesItems:NO];

	NSInteger numberOfSelectedRows = [anOutlineView numberOfSelectedRows];
	if(numberOfSelectedRows == 0)
	{
		if([url isFileURL])
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

		BOOL showEnclosingFolder = item && [item.url isFileURL] && [@[ @"search", @"scm" ] containsObject:[url scheme]];
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

		OakFinderLabelChooser* swatch = [[[OakFinderLabelChooser alloc] initWithFrame:NSMakeRect(0, 0, 166, 37)] autorelease];
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
		self.historyController  = [[OakHistoryController new] autorelease];
		[self loadFileBrowserOptions];

		BOOL foldersOnTop   = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFoldersOnTopKey];
		BOOL showExtensions = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFileExtensionsKey];

		dataSourceOptions |= (foldersOnTop   ? kFSDataSourceOptionGroupsFirst   : 0);
		dataSourceOptions |= (showExtensions ? kFSDataSourceOptionShowExtension : 0);

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(userDefaultsDidChange:) name:NSUserDefaultsDidChangeNotification object:[NSUserDefaults standardUserDefaults]];
	}
	return self;
}

- (void)userDefaultsDidChange:(NSNotification*)aNotification
{
	BOOL foldersOnTop   = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFoldersOnTopKey];
	BOOL showExtensions = [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsShowFileExtensionsKey];

	BOOL oldFoldersOnTop   = (dataSourceOptions & kFSDataSourceOptionGroupsFirst) == kFSDataSourceOptionGroupsFirst;
	BOOL oldShowExtensions = (dataSourceOptions & kFSDataSourceOptionShowExtension) == kFSDataSourceOptionShowExtension;

	if(foldersOnTop != oldFoldersOnTop || showExtensions != oldShowExtensions)
	{
		dataSourceOptions ^= (foldersOnTop != oldFoldersOnTop     ? kFSDataSourceOptionGroupsFirst   : 0);
		dataSourceOptions ^= (showExtensions != oldShowExtensions ? kFSDataSourceOptionShowExtension : 0);

		[self reload:self];
	}
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];

	self.view              = nil;
	self.url               = nil;
	self.historyController = nil;

	[super dealloc];
}

- (void)setView:(OakFileBrowserView*)aView
{
	if(aView != view)
	{
		if(view)
		{
			[outlineViewDelegate release];
			outlineViewDelegate = nil;

			if(view.delegate == self)
				view.delegate = nil;
			if(view.persistentNextResponder == self)
				view.persistentNextResponder = nil;

			[view release];
		}

		if(view = [aView retain])
		{
			view.delegate = self;
			view.persistentNextResponder = self;

			outlineViewDelegate = [FSOutlineViewDelegate new];
			outlineViewDelegate.outlineView = view.outlineView;

			view.outlineView.target       = self;
			view.outlineView.action       = @selector(didSingleClickOutlineView:);
			view.outlineView.doubleAction = @selector(didDoubleClickOutlineView:);
			view.outlineView.menuDelegate = self;

			[view.outlineView setDraggingSourceOperationMask:NSDragOperationCopy|NSDragOperationMove|NSDragOperationLink forLocal:YES];
			[view.outlineView setDraggingSourceOperationMask:NSDragOperationEvery forLocal:NO];
			[view.outlineView registerForDraggedTypes:@[ NSFilenamesPboardType ]];

			[self updateView];
		}
	}
}

- (void)setupViewWithSize:(NSSize)viewSize resizeIndicatorOnRight:(BOOL)flag state:(NSDictionary*)fileBrowserState
{
	self.view = [[[OakFileBrowserView alloc] initWithFrame:(NSRect){ NSZeroPoint, viewSize }] autorelease];
	view.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
	[view setShowsResizeIndicator:YES onRight:flag];
	historyController.state = fileBrowserState;
	if(!historyController.currentURL)
		[historyController addURLToHistory:url];
	CGFloat scrollOffset = historyController.currentURLScrollOffset;
	[self setURL:historyController.currentURL];
	[historyController setCurrentURLScrollOffset:scrollOffset];
	[outlineViewDelegate scrollToOffset:historyController.currentURLScrollOffset];
}

// ============
// = Services =
// ============

+ (void)initialize
{
	[[NSApplication sharedApplication] registerServicesMenuSendTypes:@[ NSFilenamesPboardType, NSURLPboardType ] returnTypes:nil];

	kURLLocationComputer  = [[NSURL alloc] initWithString:@"computer:///"];
	kURLLocationHome      = [[NSURL alloc] initFileURLWithPath:NSHomeDirectory() isDirectory:YES];
	kURLLocationDesktop   = [[NSURL alloc] initFileURLWithPath:[NSString stringWithCxxString:path::desktop()] isDirectory:YES];
	kURLLocationFavorites = [[NSURL alloc] initFileURLWithPath:[NSString stringWithCxxString:oak::application_t::support("Favorites")] isDirectory:YES];
	kURLLocationBundles   = [[NSURL alloc] initWithString:@"bundles:///"];
}

- (id)validRequestorForSendType:(NSString*)sendType returnType:(NSString*)returnType
{
	if(returnType == nil && ([sendType isEqualToString:NSFilenamesPboardType] || [sendType isEqualToString:NSStringPboardType]))
			return self;
	else	return [super validRequestorForSendType:sendType returnType:returnType];
}

- (BOOL)writeSelectionToPasteboard:(NSPasteboard*)pboard types:(NSArray*)types
{
	return [view.outlineView.dataSource outlineView:view.outlineView writeItems:self.selectedItems toPasteboard:pboard];
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
	dataSourceOptions = 0;
	iterate(it, DataSourceOptionsMap)
	{
		if([array containsObject:it->name])
			dataSourceOptions |= it->flag;
	}
}

- (void)saveFileBrowserOptions
{
	NSMutableArray* array = [NSMutableArray array];	
	iterate(it, DataSourceOptionsMap)
	{
		if((dataSourceOptions & it->flag) == it->flag)
			[array addObject:it->name];
	}

	if([array count] == 0)
			[[NSUserDefaults standardUserDefaults] removeObjectForKey:kUserDefaultsFileBrowserDataSourceOptions];
	else	[[NSUserDefaults standardUserDefaults] setObject:array forKey:kUserDefaultsFileBrowserDataSourceOptions];
}

- (IBAction)toggleViewOption:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	dataSourceOptions ^= [sender tag];
	[self saveFileBrowserOptions];
	[self reload:self];
}

- (IBAction)showFolderSpecificPreferences:(id)sender
{
	[delegate fileBrowser:self openURLs:@[ [NSURL fileURLWithPath:[[url path] stringByAppendingPathComponent:@".tm_properties"] isDirectory:NO]] ];
}

- (IBAction)showOptionsPopUpMenu:(id)sender
{
	NSMenu* menu = [[NSMenu new] autorelease];
	iterate(it, DataSourceOptionsMap)
	{
		NSMenuItem* item = [menu addItemWithTitle:it->name action:@selector(toggleViewOption:) keyEquivalent:@""];
		[item setState:(((dataSourceOptions & it->flag) == it->flag) ? NSOnState : NSOffState)];
		[item setTarget:self];
		[item setTag:it->flag];
	}

	[menu addItem:[NSMenuItem separatorItem]];
	NSMenuItem* menuItem = [menu addItemWithTitle:@"Reload" action:@selector(reload:) keyEquivalent:@""];
	[menuItem setTarget:self];

	if([url isFileURL])
	{
		[menu addItem:[NSMenuItem separatorItem]];
		NSMenuItem* menuItem = [menu addItemWithTitle:@"Preferences…" action:@selector(showFolderSpecificPreferences:) keyEquivalent:@""];
		[menuItem setTarget:self];
	}

	[view displayMenu:menu fromHeaderColumn:fb::options selectedIndex:0 popup:NO];
}

// =======================
// = Header View Actions =
// =======================

- (void)updateView
{
	view.titleText          = outlineViewDelegate.dataSource.rootItem.name;
	view.titleImage         = outlineViewDelegate.dataSource.rootItem.icon;
	view.canGoBackward      = historyController.previousURL ? YES : NO;
	view.canGoForward       = historyController.nextURL     ? YES : NO;
	[view setNeedsDisplay:YES];
}

- (IBAction)goToComputer:(id)sender       { [self pushURL:kURLLocationComputer];  }
- (IBAction)goToHome:(id)sender           { [self pushURL:kURLLocationHome];      }
- (IBAction)goToDesktop:(id)sender        { [self pushURL:kURLLocationDesktop];   }
- (IBAction)goToFavorites:(id)sender      { [self pushURL:kURLLocationFavorites]; }

- (IBAction)goToSCMDataSource:(id)sender
{
	for(NSURL* selectedURL in self.selectedURLs)
	{
		if([selectedURL isFileURL] && path::is_directory([[selectedURL path] fileSystemRepresentation]))
			return [self pushURL:[FSSCMDataSource scmURLWithPath:[selectedURL path]]];
	}
	[self pushURL:[FSSCMDataSource scmURLWithPath:[url path]]];
}

- (IBAction)goBack:(id)sender             { if(historyController.previousURL) { [self setURL:historyController.previousURL]; [historyController retreat:self]; [outlineViewDelegate scrollToOffset:historyController.currentURLScrollOffset]; [self updateView]; } }
- (IBAction)goForward:(id)sender          { if(historyController.nextURL)     { [self setURL:historyController.nextURL];     [historyController advance:self]; [outlineViewDelegate scrollToOffset:historyController.currentURLScrollOffset]; [self updateView]; } }

- (IBAction)goToParentFolder:(id)sender
{
	[self pushURL:ParentForURL(url)];
}

- (IBAction)orderFrontGoToFolder:(id)sender
{
	NSOpenPanel* panel = [NSOpenPanel openPanel];
	[panel setCanChooseFiles:NO];
	[panel setCanChooseDirectories:YES];
	[panel setAllowsMultipleSelection:NO];
	[panel setDirectoryURL:[NSURL fileURLWithPath:self.location]];
	[panel beginSheetModalForWindow:view.window completionHandler:^(NSInteger result) {
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
	NSMenu* menu                   = [[NSMenu new] autorelease];
	NSMutableSet* visibleLocations = [NSMutableSet setWithObjects:kURLLocationComputer, kURLLocationHome, kURLLocationFavorites, nil];

	// Add path hierarchy
	for(NSURL* currentURL = url; currentURL; currentURL = ParentForURL(currentURL))
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:DisplayName(currentURL) action:@selector(takeURLFrom:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:currentURL];
		[menuItem setImage:IconImage(currentURL)];
		[visibleLocations addObject:currentURL];
	}

	// Add recent locations
	NSMutableArray* recentURLs = [NSMutableArray array];
	for(NSUInteger index = 0; index < historyController.recentLocations.count && index < 10; ++index)
	{
		NSURL* recentURL = [historyController.recentLocations objectAtIndex:index];
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

	[view displayMenu:menu fromHeaderColumn:fb::title selectedIndex:0 popup:YES];
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
	[self setURL:[historyController urlAtIndex:[sender tag]]];
	historyController.historyIndex = [sender tag];
	[outlineViewDelegate scrollToOffset:historyController.currentURLScrollOffset];
	[self updateView];
}

- (void)showHistoryItems:(NSArray*)urls fromHeaderColumn:(fb::header_column)columnTag
{
	// TODO DisplayName paths
	NSMutableSet* seenPaths = [NSMutableSet set];
	NSMenu* menu = [[NSMenu new] autorelease];
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
	[view displayMenu:menu fromHeaderColumn:columnTag selectedIndex:0 popup:NO];
}

- (IBAction)showBackMenu:(id)sender
{
	NSMutableArray* urls = [NSMutableArray array];
	for(NSInteger historyIndex = historyController.historyIndex - 1; historyIndex >= 0 && historyController.historyIndex - historyIndex <= 10; --historyIndex)
		[urls addObject:[NSURL URLWithString:[[[historyController urlAtIndex:historyIndex] absoluteString] stringByAppendingFormat:@"#%ld", historyIndex]]];
	[self showHistoryItems:urls fromHeaderColumn:fb::goBack];
}

- (IBAction)showForwardMenu:(id)sender
{
	NSMutableArray* urls = [NSMutableArray array];
	for(NSInteger historyIndex = historyController.historyIndex + 1; historyIndex < historyController.historyCount && historyIndex - historyController.historyIndex <= 10; ++historyIndex)
		[urls addObject:[NSURL URLWithString:[[[historyController urlAtIndex:historyIndex] absoluteString] stringByAppendingFormat:@"#%ld", historyIndex]]];
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

	NSMenu* menu = [[NSMenu new] autorelease];
	iterate(pair, urls)
	{
		NSMenuItem* menuItem = [menu addItemWithTitle:[NSString stringWithCxxString:pair->first] action:@selector(takeURLFrom:) keyEquivalent:@""];
		[menuItem setTarget:self];
		[menuItem setRepresentedObject:pair->second];
		[menuItem setImage:IconImage(pair->second)];
	}
	[view displayMenu:menu fromHeaderColumn:column selectedIndex:0 popup:NO];
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
			return ParentForURL(url) != nil;
	else if([item action] == @selector(goBack:))
			return historyController.previousURL ? YES : NO;
	else if([item action] == @selector(goForward:))
			return historyController.nextURL ? YES : NO;
	else if([item action] == @selector(delete:))
			return [view.outlineView numberOfSelectedRows] > 0;
	else if([item action] == @selector(undo:))
			return [self canUndo];
	else if([item action] == @selector(redo:))
			return [self canRedo];
	else	return YES;
}
@end
