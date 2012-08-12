#import "FFWindowController.h"
#import <oak/CocoaSTL.h>
#import <oak/oak.h>
#import <OakFoundation/OakFoundation.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardSelector.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/NSMenuItem Additions.h>
#import <OakAppKit/OakSubmenuController.h>
#import <OakFoundation/NSString Additions.h>
#import <document/collection.h>
#import <io/path.h>
#import <regexp/format_string.h>
#import "FFFilePathCell.h"
#import "FFFolderMenu.h"
#import "FFFindWindow.h"
#import <io/path.h>
#import <ns/ns.h>
#import <ns/attr_string.h>
#import <OakAppKit/OakStatusBar.h>
#import "scan_path.h"
#import "Strings.h"

OAK_DEBUG_VAR(Find_WindowController);

/*
	TODO Save proper window frame to defaults
	TODO Let next/previous advance the Find All results
	TODO Show/hide Find All Results refactoring
*/

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n)
{
	D(DBF_Find_WindowController, bug("%zu, %zu-%zu: ‘%s’\n", n, from, to, text.c_str()););

	ns::attr_string_t str;
	str.add(ns::style::line_break(NSLineBreakByTruncatingTail));
	str.add([NSColor darkGrayColor]);
	str.add([NSFont systemFontOfSize:11]);

	str.add(text::pad(++n, 4) + ": ");

	bool inMatch = false;
	size_t last = text.size();
	for(size_t it = 0; it != last; )
	{
		size_t eol = std::find(text.begin() + it, text.end(), '\n') - text.begin();

		if(oak::cap(it, from, eol) == from)
		{
			str.add(text.substr(it, from-it));
			it = from;
			inMatch = true;
		}

		if(inMatch)
		{
			str.add([NSFont boldSystemFontOfSize:11]);
			str.add([NSColor blackColor]);
		}

		if(inMatch && oak::cap(it, to, eol) == to)
		{
			str.add(text.substr(it, to-it));
			it = to;
			inMatch = false;

			str.add([NSColor darkGrayColor]);
			str.add([NSFont systemFontOfSize:11]);
		}

		str.add(text.substr(it, eol-it));

		if(eol != last)
		{
			str.add("¬");

			if(inMatch)
			{
				str.add([NSFont systemFontOfSize:11]);
				str.add([NSColor darkGrayColor]);
			}

			if(++eol == to)
				inMatch = false;

			if(eol != last)
				str.add("\n" + text::pad(++n, 4) + ": ");
		}

		it = eol;
	}

	return str;
}

enum MenuTags
{
	MenuItemDocument = 1,
	MenuItemSelection,
	MenuItemSelectedFolder,
	MenuItemProjectFolder,
	MenuItemOtherFolder,
	MenuItemOpenFiles,
	MenuItemRecentPlaces,
	MenuItemFirstRecentPlace,
};

NSString* const FFSearchInDocument      = @"FFSearchInDocument";
NSString* const FFSearchInSelection     = @"FFSearchInSelection";
NSString* const FFSearchInProjectFolder = @"FFSearchInProjectFolder";
NSString* const FFSearchInOtherFolder   = @"FFSearchInOtherFolder";
NSString* const FFSearchInOpenFiles     = @"FFSearchInOpenFiles";

@interface FFWindowController ()
- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(id)item;
- (void)openPanelDidEnd:(NSOpenPanel*)panel returnCode:(NSInteger)returnCode;

@property (nonatomic, assign) BOOL canSetFileTypes;
@property (nonatomic, assign) BOOL canSetWrapAround;
@property (nonatomic, assign) BOOL enableReplacementSelectionCheckboxes;
@property (nonatomic, readonly) BOOL showReplacementSelectionCheckboxes;
@property (nonatomic, readonly) BOOL isDirty;
@property (nonatomic, copy) NSString* resultsHeaderText;
@property (nonatomic, readonly) NSString* rootPath;

@property (nonatomic, retain) FFFindWindow* window;
@end

@implementation FFWindowController
@synthesize recentGlobs;
@synthesize resultsHeaderText;
@dynamic window;
@synthesize canSetFileTypes, canSetWrapAround, statusMessage, searchFolder, searchIn, findString, replaceString;
@synthesize findRegularExpression, findFullWords, findIgnoreWhitespace, followLinks, searchHiddenFolders;
@synthesize enableReplacementSelectionCheckboxes;
@synthesize searcher, delegate;

+ (void)initialize
{
	NSArray* defaultRecentPlaces = @[ NSHomeDirectory(), [NSHomeDirectory() stringByAppendingPathComponent:@"Documents"], [NSHomeDirectory() stringByAppendingPathComponent:@"Desktop"] ];
	[[NSUserDefaults standardUserDefaults] registerDefaults:
		[NSDictionary dictionaryWithObjectsAndKeys:
			defaultRecentPlaces,	@"findRecentPlaces",
		nil]];
}

+ (NSSet*)keyPathsForValuesAffectingCanSearch            { return [NSSet setWithObject:@"findString"];            }
+ (NSSet*)keyPathsForValuesAffectingFindFullWords        { return [NSSet setWithObject:@"findRegularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingFindIgnoreWhitespace { return [NSSet setWithObject:@"findRegularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingIsSearchingFolders   { return [NSSet setWithObject:@"searchIn"];              }

- (id)init
{
	if(self = [self initWithWindowNibName:@"Find"])
	{
		D(DBF_Find_WindowController, bug("\n"););

		canSetWrapAround = YES;
		canSetFileTypes = NO;

		recentGlobs   = [[OakHistoryList alloc] initWithName:@"Find in Folder Globs.default" stackSize:10 defaultItems:@"*", @"*.txt", @"*.{c,h}", nil];
		recentFolders = [[OakHistoryList alloc] initWithName:@"findRecentPlaces" stackSize:6];

		[self addObserver:self forKeyPath:@"showReplacementSelectionCheckboxes" options:0 context:NULL];
	}
	return self;
}

- (void)invalidate
{
	findController.content = nil;
	[[[findInPopUp menu] itemWithTag:MenuItemSelectedFolder] setRepresentedObject:nil]; // FIXME maybe try to find a cleaner solution for this
}

- (void)dealloc
{
	// FIXME Add missing releases
	[self removeObserver:self forKeyPath:@"showReplacementSelectionCheckboxes"];
	[self.window removeObserver:self forKeyPath:@"firstResponder"];
	self.searcher = nil;
	[recentGlobs release];
	[recentFolders release];
	[super dealloc];
}

- (void)copy:(id)sender
{
	NSIndexSet* selectedRows = [findAllResultsOutlineView selectedRowIndexes];
	int rowCount             = [selectedRows count];
	NSMutableArray* rows     = [NSMutableArray arrayWithCapacity:rowCount];

	iterate(it, selectedRows)
	{
		FFMatch* item = [findAllResultsOutlineView itemAtRow:*it];
		if([self outlineView:findAllResultsOutlineView isGroupItem:item])
				[rows addObject:[NSString stringWithFormat:@"%@:%lu\n", item.path, [item match].range.from.line + 1]];
		else	[rows addObject:[NSString stringWithFormat:@"%@:%lu\t%@", item.path, [item match].range.from.line + 1, [NSString stringWithCxxString:[item matchText]]]];
	}
	[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
	[[NSPasteboard generalPasteboard] setString:[rows componentsJoinedByString:@""] forType:NSStringPboardType];
}

- (IBAction)showWindow:(id)sender
{
	[super showWindow:sender];
	[[self window] makeFirstResponder:findStringField];
}

struct operation_t
{
	enum task { close, set_scope, perform_find } operation;
	id argument; // non-retained
	operation_t (task operation, id argument = nil) : operation(operation), argument(argument) {}
};

- (void)displayUnsavedChangesSheetForOperation:(operation_t*)operation
{
	D(DBF_Find_WindowController, bug("\n"););
	NSAlert* sheet = [[NSAlert alloc] init]; // released in didEndSelector
	[sheet setMessageText:@"Do you want to save the changes from your replace operation?"];
	[sheet setInformativeText:@"Your changes will be lost if you don’t save them."];
	[sheet addButtonWithTitle:@"Save All"];
	[sheet addButtonWithTitle:@"Cancel"];
	NSButton* discardButton = [sheet addButtonWithTitle:@"Discard"];
	[discardButton setKeyEquivalent:@"d"];
	[discardButton setKeyEquivalentModifierMask:NSCommandKeyMask];
	[sheet beginSheetModalForWindow:self.window modalDelegate:self didEndSelector:@selector(unsavedReplacementsSheetDidEnd:returnCode:contextInfo:) contextInfo:operation];
}

- (BOOL)windowShouldClose:(id)window
{
	D(DBF_Find_WindowController, bug("\n"););
	if(!self.window.isDocumentEdited)
		return YES;
	[self displayUnsavedChangesSheetForOperation:new operation_t(operation_t::close)];
	return NO;
}

- (void)unsavedReplacementsSheetDidEnd:(NSAlert*)alert returnCode:(NSInteger)returnCode contextInfo:(operation_t*)operation
{
	D(DBF_Find_WindowController, bug("\n"););
	[alert release];
	switch(returnCode)
	{
		case NSAlertFirstButtonReturn: // Save All
			[self saveAll:nil];
			break;
		case NSAlertSecondButtonReturn: // Cancel
			return;
		case NSAlertThirdButtonReturn: // Discard
			// FIXME Unimplmenented
			self.searcher = nil;
			break;
	}
	switch(operation->operation)
	{
		case operation_t::close:        [self close]; break;
		case operation_t::set_scope:    self.searchIn = operation->argument; break;
		case operation_t::perform_find: [self performFindAction:operation->argument];
	}
	delete operation;
}

- (void)updateResultsHeader
{
	NSImage* rightArrow        = [NSImage imageNamed:@"Right Arrow" inSameBundleAsClass:[self class]];
	NSImage* downArrow         = [NSImage imageNamed:@"Down Arrow" inSameBundleAsClass:[self class]];
	NSImage* pressedRightArrow = [NSImage imageNamed:@"Right Arrow Pressed" inSameBundleAsClass:[self class]];
	NSImage* pressedDownArrow  = [NSImage imageNamed:@"Down Arrow Pressed" inSameBundleAsClass:[self class]];
	NSImage* gear              = [NSImage imageNamed:@"Gear" inSameBundleAsClass:[self class]];

	sb::cell_t const cells[] = {
		expandCollapseAllIsExpanding ?
			sb::cell_t::button(rightArrow, @selector(expandFindAllResults:), self).pressed_image(pressedRightArrow).enabled(resultsHeaderEnabled) :
			sb::cell_t::button(downArrow, @selector(collapseFindAllResults:), self).pressed_image(pressedDownArrow).enabled(resultsHeaderEnabled),
		sb::cell_t::dropdown(gear, @selector(showFindInFolderOptionsDropdown:), self),
		sb::cell_t::dropdown(resultsHeaderText, @selector(showTableOfContentsDropdown:), self).size(30, CGFLOAT_MAX),
	};
	SetCells(findAllResultsHeaderView, cells);
}

- (void)setResultsHeaderText:(NSString*)text
{
	if(text != resultsHeaderText)
	{
		[resultsHeaderText release];
		resultsHeaderText = [text retain];
		[self updateResultsHeader];
	}
}

- (void)updateGoToMenu:(NSMenu*)menu
{
	if(searcher.allDocumentsWithMatches.count == 0)
	{
		[[menu addItemWithTitle:@"No Results" action:@selector(dummy:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		NSUInteger key = 1;
		for(FFMatch* match in searcher.allDocumentsWithMatches)
		{
			document::document_ptr doc = [match match].document;
			std::string const& title   = doc->path() == NULL_STR ? doc->display_name() : path::relative_to(doc->path(), [[self rootPath] UTF8String]);
			NSString* shortcut         = key < 11 ? [NSString stringWithFormat:@"%lu", (key++ % 10)] : @"";
			NSMenuItem* item           = [menu addItemWithTitle:[NSString stringWithCxxString:title] action:@selector(selectMatchingPath:) keyEquivalent:shortcut];
			[item setIconForFile:match.path];
			[item setRepresentedObject:match.path];
		}
	}
}

- (IBAction)showTableOfContentsDropdown:(id)sender
{
	NSMenu* menu = [[[NSMenu alloc] initWithTitle:@"TOC"] autorelease];
	[self updateGoToMenu:menu];
	[findAllResultsHeaderView showMenu:menu withSelectedIndex:0 forCellWithTag:[sender tag] font:[NSFont controlContentFontOfSize:12.0] popup:NO];
}

- (IBAction)selectMatchingPath:(id)sender
{
	if(NSString* path = [[OakSubmenuController sharedInstance] representedObjectForSender:sender])
	{
		for(FFMatch* match in self.searcher.allDocumentsWithMatches)
		{
			if([match.path isEqualToString:path])
			{
				NSUInteger row = [findAllResultsOutlineView rowForItem:match];
				[findAllResultsOutlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
				[[findAllResultsOutlineView window] makeFirstResponder:findAllResultsOutlineView];
				[findAllResultsOutlineView scrollRowToVisible:findAllResultsOutlineView.numberOfRows-1];
				[findAllResultsOutlineView scrollRowToVisible:row];
				break;
			}
		}
	}
}

- (IBAction)showFindInFolderOptionsDropdown:(id)sender
{
	NSMenu* menu = [[[NSMenu alloc] initWithTitle:@"Options"] autorelease];
	NSMenuItem* linksItem = [menu addItemWithTitle:@"Symbolic Links" action:NULL keyEquivalent:@""];
	[linksItem bind:@"value" toObject:self withKeyPath:@"followLinks" options:nil];
	NSMenuItem* hidddenItem = [menu addItemWithTitle:@"Hidden Folders" action:NULL keyEquivalent:@""];
	[hidddenItem bind:@"value" toObject:self withKeyPath:@"searchHiddenFolders" options:nil];
	NSMenuItem* showCheckboxesItem = [menu addItemWithTitle:@"Show Checkboxes" action:NULL keyEquivalent:@""];
	[showCheckboxesItem bind:@"value" toObject:self withKeyPath:@"enableReplacementSelectionCheckboxes" options:nil];
	[findAllResultsHeaderView showMenu:menu withSelectedIndex:0 forCellWithTag:[sender tag] font:[NSFont controlContentFontOfSize:[NSFont smallSystemFontSize]] popup:NO];
}

- (IBAction)collapseFindAllResults:(id)sender
{
	[findAllResultsOutlineView collapseItem:nil collapseChildren:YES];
	[findAllResultsOutlineView setNeedsDisplay:YES];
	expandCollapseAllIsExpanding = YES;
	[self updateResultsHeader];
}

- (IBAction)expandFindAllResults:(id)sender
{
	[findAllResultsOutlineView expandItem:nil expandChildren:YES];
	[findAllResultsOutlineView setNeedsDisplay:YES];
	expandCollapseAllIsExpanding = NO;
	[self updateResultsHeader];
}

- (void)windowDidLoad
{
	findAllResultsOutlineView.action          = @selector(didSingleClickFindAllResults:);
	findAllResultsOutlineView.doubleAction    = @selector(didDoubleClickFindAllResults:);
	findAllResultsOutlineView.target          = self;
	findAllResultsHeaderView.borderEdges      = sb::border::top|sb::border::bottom;
	resultsHeaderEnabled                      = NO;
	globField.font                            = [NSFont userFixedPitchFontOfSize:12];
	self.resultsHeaderText                    = @"Find All Results";
	self.enableReplacementSelectionCheckboxes = YES;
	[self.window addObserver:self forKeyPath:@"firstResponder" options:0 context:NULL];
}

- (NSString*)displayNameForFolder:(NSString*)path
{
	std::vector<std::string> paths;
	for(int i = 0; i < [recentFolders count]; ++i)
		paths.push_back([[recentFolders objectAtIndex:i] UTF8String]);
	if(self.isSearchingFolders)
		paths.push_back([self.searchFolder UTF8String]);
	paths.push_back([self.projectFolder UTF8String]);

	std::vector<std::string>::const_iterator it = std::find(paths.begin(), paths.end(), [path UTF8String]);
	if(it == paths.end())
		return nil;

	std::vector<std::string>::size_type index = it - paths.begin();
	std::vector<size_t> const& parents        = path::disambiguate(paths);
	return [NSString stringWithCxxString:path::display_name(*it, parents[index])];
}

- (void)buildRecentPlacesPopUpMenu
{
	// This assumes that the recent places list is at the bottom of the menu
	for(int i = findInPopUp.itemArray.count - 1; i >= MenuItemFirstRecentPlace; --i)
	{
		int index = [findInPopUp indexOfItemWithTag:i];
		if(index != -1)
			[findInPopUp removeItemAtIndex:index];
	}

	int tag = MenuItemFirstRecentPlace;
	NSMenu* menu = [findInPopUp menu];

	BOOL skipFirst = recentFolders.count > 0 && [[recentFolders objectAtIndex:0] isEqualToString:self.searchIn];
	for(int i = skipFirst ? 1 : 0; i < [recentFolders count]; ++i)
	{
		NSMenuItem* recentItem = [menu addItemWithTitle:[self displayNameForFolder:[recentFolders objectAtIndex:i]] action:NULL keyEquivalent:@""];
		[recentItem setTag:tag+i];
		[recentItem setIconForFile:[recentFolders objectAtIndex:i]];
	}
}

- (void)userDidSelectFolder:(NSString*)folder inMenu:(NSMenu*)menu
{
	self.searchIn = folder;
}

- (NSString*)searchFolder
{
	return [searchFolder isEqualToString:FFSearchInProjectFolder] ? self.projectFolder : searchFolder;
}

- (void)updateSelectedFolderItem
{
	[searchFolder release];
	searchFolder = [searchIn retain];

	[FFFolderMenu addFolderMenuAtPath:self.searchFolder toMenuItem:[[findInPopUp menu] itemWithTag:MenuItemSelectedFolder] withOwner:self];
	[self buildRecentPlacesPopUpMenu];

	NSMenuItem* currentItem = [[findInPopUp menu] itemWithTag:MenuItemSelectedFolder];
	[currentItem setTitle:[self displayNameForFolder:self.searchFolder]];
	[currentItem setIconForFile:self.searchFolder];
	[findInPopUp selectItem:currentItem];
}

- (IBAction)showFolderSelectionPanel:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.title = @"Find in Folder";
	openPanel.canChooseFiles = NO;
	openPanel.canChooseDirectories = YES;
	NSString* startPath = self.isSearchingFolders ? self.searchFolder : nil;
	openPanel.directoryURL = [NSURL fileURLWithPath:startPath];
	if([[self window] isVisible])
	{
		[openPanel beginSheetModalForWindow:[self window] completionHandler:^(NSInteger result) {
			[self openPanelDidEnd:openPanel returnCode:result];
		}];
	}
	else
	{
		[openPanel beginWithCompletionHandler:^(NSInteger result) {
			[self openPanelDidEnd:openPanel returnCode:result];
		}];
	}
}

/*
searchIn should contain one of the following values:

FFSearchInDocument/FFSearchInSelection:
	The expected result

FFSearchInProjectFolder:
	Searching in folder, self.projectFolder

Any other string:
	Searching in this folder.
	Note that FFSearchInOtherFolder will never be set as the value searchIn.
	If this value is given to the setter then the open sheet/panel will be displayed, and then the panel will set the appropartiate saerch folder as searchIn after
	This enables the sheet end handler to restore the previously selected menu item if file selector is canclled.
*/
- (void)setSearchIn:(NSString*)where
{
	D(DBF_Find_WindowController, bug("%s\n", where.UTF8String););
	[self window]; // force nib to be loaded

	if(searchIn != where && ![where isEqualToString:FFSearchInOtherFolder])
	{
		[searchIn release];
		searchIn = [where copy];
	}

	if(self.isDirty && ([where isEqualToString:FFSearchInSelection] || [where isEqualToString:FFSearchInDocument]))
		return [self displayUnsavedChangesSheetForOperation:new operation_t(operation_t::set_scope, where)];

	if([where isEqualToString:FFSearchInSelection])
	{
		[findInPopUp selectItemWithTag:MenuItemSelection];
		self.canSetWrapAround        = NO;
		self.canSetFileTypes         = NO;
		self.isShowingFindAllResults = NO;
	}
	else if([where isEqualToString:FFSearchInProjectFolder] || [where isEqualToString:self.projectFolder])
	{
		[findInPopUp selectItemWithTag:MenuItemSelectedFolder];
		self.canSetWrapAround        = NO;
		self.canSetFileTypes         = YES;
		self.isShowingFindAllResults = YES;
	}
	else if([where isEqualToString:FFSearchInOpenFiles])
	{
		self.canSetWrapAround        = NO;
		self.canSetFileTypes         = NO;
		self.isShowingFindAllResults = YES;
	}
	else if([where isEqualToString:FFSearchInOtherFolder])
	{
		[self showFolderSelectionPanel:self];
	}
	else if([where isEqualToString:FFSearchInDocument])
	{
		[findInPopUp selectItemWithTag:MenuItemDocument];
		self.canSetWrapAround        = YES;
		self.canSetFileTypes         = NO;
		// Hide the find all results list only if the current search results are not from a document.
		self.isShowingFindAllResults = (searcher.documentIdentifier != nil && self.window.isVisible);
	}
	else
	{
		[findInPopUp selectItemWithTag:MenuItemSelectedFolder];
		self.canSetWrapAround        = NO;
		self.canSetFileTypes         = YES;
		self.isShowingFindAllResults = YES;
		[recentFolders addObject:searchIn];
	}

	if(self.isSearchingFolders)
	{
		[self updateSelectedFolderItem];
		self.window.title = [NSString localizedStringWithFormat:MSG_FIND_IN_FOLDER_WINDOW_TITLE, [self.searchFolder stringByAbbreviatingWithTildeInPath]];
	}
	else
	{
		self.window.title = MSG_WINDOW_TITLE;
	}
}

// =============
// = Accessors =
// =============

- (BOOL)isSearchingFolders
{
	return (searchIn != FFSearchInDocument && searchIn != FFSearchInSelection && searchIn != FFSearchInOpenFiles);
}

- (BOOL)isBusy
{
	return isBusy;
}

- (void)setIsBusy:(BOOL)flag
{
	if(isBusy != flag)
	{
		NSRect oldFrame = [statusTextField frame];

		NSRect frame = [statusTextField frame];
		frame.origin.x   += flag ? +24.0 : -24.0;
		frame.size.width += flag ? -24.0 : +24.0;
		[statusTextField setFrame:frame];

		if(flag)
			[[statusTextField superview] setNeedsDisplayInRect:oldFrame];

		isBusy = flag;
	}
}

+ (NSSet*)keyPathsForValuesAffectingCanReplaceAll
{
	return [NSSet setWithObjects:@"isBusy", @"isSearchingFolders", @"searcher.countOfSelectedMatches", @"searcher.hasPerformedReplacement", nil];
}

- (BOOL)canReplaceAll
{
	if(searcher.hasPerformedReplacement)
		return NO;
	if(!self.isSearchingFolders)
			return !self.isShowingFindAllResults || (searcher.countOfSelectedMatches > 0);
	else	return !self.isBusy && (searcher.countOfSelectedMatches > 0);
}

+ (NSSet*)keyPathsForValuesAffectingCanReplaceSelected
{
	return [NSSet setWithObjects:@"canReplaceAll", @"isShowingFindAllResults", nil];
}

- (BOOL)canReplaceSelected
{
	return self.canReplaceAll && self.isShowingFindAllResults && (searcher.countOfSelectedMatches != searcher.countOfMatches);
}

+ (NSSet*)keyPathsForValuesAffectingIsDirty
{
	return [NSSet setWithObjects:@"searcher.documentIdentifier", @"searcher.hasPerformedReplacement", @"searcher.hasPerformedSave", nil];
}

- (BOOL)isDirty
{
	return searcher.documentIdentifier == nil && searcher.hasPerformedReplacement && !searcher.hasPerformedSave;
}

// =============================
// = Stupid Accessor Functions =
// =============================

- (NSString*)projectFolder { return projectFolder ?: @""; }

- (void)setProjectFolder:(NSString*)folder
{
	if(folder != projectFolder && ![folder isEqualToString:projectFolder])
	{
		[projectFolder release];
		projectFolder = [(folder ?: @"") retain];
		self.recentGlobs = [[[OakHistoryList alloc] initWithName:[NSString stringWithFormat:@"Find in Folder Globs.%@", projectFolder] stackSize:10 defaultItems:@"*", @"*.txt", @"*.{c,h}", nil] autorelease];
	}
}

- (BOOL)canSearch
{
	return NSNotEmptyString(self.findString);
}

- (void)setFindString:(NSString*)aString
{
	if(aString != findString && ![aString isEqualToString:findString])
	{
		[findString release];
		findString = [(aString ?: @"") retain];
	}
}

- (void)setReplaceString:(NSString*)aString
{
	if(aString != replaceString && ![aString isEqualToString:replaceString])
	{
		[replaceString release];
		replaceString = [(aString ?: @"") retain];

		if(previewReplacements)
			[findAllResultsOutlineView reloadData];
	}
}

- (NSString*)globString
{
	return recentGlobs.head;
}

- (BOOL)isShowingFindAllResults
{
	return self.window.isExpanded;
}

- (void)setIsShowingFindAllResults:(BOOL)flag
{
	self.window.isExpanded = flag;
	if(flag)
	{
		[findNextButton setKeyEquivalent:@""];
		[findAllButton setKeyEquivalent:@"\r"];
	}
	else
	{
		[findAllButton setKeyEquivalent:@""];
		[findNextButton setKeyEquivalent:@"\r"];
	}
}

- (void)setCanSetWrapAround:(BOOL)flag
{
	canSetWrapAround = flag;
	[wrapAroundField setHidden:!canSetWrapAround];
}

- (void)setCanSetFileTypes:(BOOL)flag
{
	canSetFileTypes = flag;
	[globField setHidden:!flag];
	[globFieldLabel setHidden:!flag];
}

- (BOOL)findIgnoreWhitespace { return findIgnoreWhitespace && !findRegularExpression; }
- (BOOL)findFullWords { return findFullWords && !findRegularExpression; }

+ (NSSet*)keyPathsForValuesAffectingShowReplacementSelectionCheckboxes
{
	return [NSSet setWithObjects:@"enableReplacementSelectionCheckboxes", @"searcher.hasPerformedReplacement", nil];
}

- (BOOL)showReplacementSelectionCheckboxes
{
	return enableReplacementSelectionCheckboxes && !searcher.hasPerformedReplacement;
}

- (NSString*)rootPath
{
	std::string const& rootPath = [searcher folderOptions].path == find::folder_scan_settings_t::open_files ? self.projectFolder.UTF8String : [searcher folderOptions].path;
	return [NSString stringWithCxxString:rootPath];
}

// =================
// = Notifications =
// =================

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"currentPath"])
	{
		id newValue = [change objectForKey:NSKeyValueChangeNewKey], oldValue = [change objectForKey:NSKeyValueChangeOldKey];
		std::string searchPath     = [newValue respondsToSelector:@selector(UTF8String)] ? [newValue UTF8String] : "";
		std::string lastSearchPath = [oldValue respondsToSelector:@selector(UTF8String)] ? [oldValue UTF8String] : "";

		// Show only the directory part unless the file name hasn’t changed since last poll of the scanner
		if(searchPath != lastSearchPath && !path::is_directory(searchPath))
			searchPath = path::parent(searchPath);

		std::string relative = path::relative_to(searchPath, [self rootPath].UTF8String);
		if(path::is_directory(searchPath))
			relative += "/";

		[self setStatusMessage:[NSString localizedStringWithFormat:MSG_SEARCHING_FOLDER_FMT, [NSString stringWithCxxString:relative]]];
	}
	else if([keyPath isEqualToString:@"hasPerformedReplacement"])
	{
		[findAllResultsOutlineView reloadData];
	}
	else if([keyPath isEqualToString:@"showReplacementSelectionCheckboxes"])
	{
		[findAllResultsOutlineView setOutlineTableColumn:[[findAllResultsOutlineView tableColumns] objectAtIndex:self.showReplacementSelectionCheckboxes ? 0 : 1]];
		[[[findAllResultsOutlineView tableColumns] objectAtIndex:0] setHidden:!self.showReplacementSelectionCheckboxes];
	}
	else if([keyPath isEqualToString:@"firstResponder"])
	{
		NSResponder* firstResponder = [self.window firstResponder];
		if(![firstResponder isKindOfClass:[NSTextView class]])
		{
			BOOL showPreviews = firstResponder == replaceStringField;
			if(showPreviews != previewReplacements)
			{
				previewReplacements = !previewReplacements;
				[self updateResultsHeader];
				[findAllResultsOutlineView reloadData];
			}
		}
	}
}

- (void)windowDidResignKey:(NSNotification*)aNotification
{
	[self commitEditing];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[searcher stop];
	[self commitEditing];
}

// ============
// = Find All =
// ============

- (void)setSearcher:(FFDocumentSearch*)aFolderSearch
{
	if(searcher != aFolderSearch)
	{
		if(searcher)
		{
			[searcher removeObserver:self forKeyPath:@"currentPath"];
			[searcher removeObserver:self forKeyPath:@"hasPerformedReplacement"];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:searcher];
			[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidFinishNotification object:searcher];

			for(FFMatch* fileMatch in [searcher allDocumentsWithMatches])
			{
				if(document::document_ptr doc = [fileMatch match].document)
					doc->remove_all_marks("search");
			}
		}
		[searcher release];
		searcher = [aFolderSearch retain];
		[findAllResultsOutlineView reloadData];
		if(searcher)
		{
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:searcher];
			[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:searcher];
			[searcher addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
			[searcher addObserver:self forKeyPath:@"hasPerformedReplacement" options:0 context:NULL];

			// UI setup
			self.isShowingFindAllResults = YES;

			[findAllResultsOutlineView deselectAll:nil];
			[self setStatusMessage:MSG_SEARCHING_FMT];
			[self setIsBusy:YES];

			resultsHeaderEnabled = YES;
			expandCollapseAllIsExpanding = NO;
			if([searcher folderOptions].path == find::folder_scan_settings_t::open_files)
			{
				self.resultsHeaderText = MSG_FIND_ALL_RESULTS_OPEN_FILES;
			}
			else if(searcher.documentIdentifier)
			{
				if(document::document_ptr doc = document::find(to_s(searcher.documentIdentifier)))
					self.resultsHeaderText = [NSString localizedStringWithFormat:MSG_FIND_ALL_IN_DOCUMENT_HEADER, [NSString stringWithCxxString:doc->display_name()]];
			}
			else
			{
				self.resultsHeaderText = [NSString localizedStringWithFormat:MSG_FIND_ALL_RESULTS, [NSString stringWithCxxString:path::with_tilde([searcher folderOptions].path)], [globField stringValue]];
			}
		}
		[searcher start];
	}
}

- (void)folderSearchDidReceiveResults:(NSNotification*)notification
{
	int first = [findAllResultsOutlineView numberOfRows];
	[findAllResultsOutlineView reloadData];
	int last = [findAllResultsOutlineView numberOfRows];
	while(last-- != first)
		[findAllResultsOutlineView expandItem:[findAllResultsOutlineView itemAtRow:last]];
}

- (void)folderSearchDidFinish:(NSNotification*)notification
{
	[self setIsBusy:NO];

	if(!searcher)
		return;

	for(FFMatch* fileMatch in [searcher allDocumentsWithMatches])
	{
		if(document::document_ptr doc = [fileMatch match].document)
		{
			for(FFMatch* match in [searcher allMatchesForDocumentIdentifier:[NSString stringWithCxxString:doc->identifier()]])
				doc->add_mark([match match].range, "search");
		}
	}

	NSUInteger totalMatches = [searcher countOfMatches];
	NSString* fmt = MSG_ZERO_MATCHES_FMT;
	switch(totalMatches)
	{
		case 0:  fmt = MSG_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_ONE_MATCH_FMT;        break;
		default: fmt = MSG_MULTIPLE_MATCHES_FMT; break;
	}

	NSNumberFormatter* formatter = [[NSNumberFormatter new] autorelease]; // FIXME we want to cache this as it is expensive
	[formatter setPositiveFormat:@"#,##0"];
	[formatter setLocalizesFormat:YES];

	NSString* msg = [NSString localizedStringWithFormat:fmt,
							  [searcher searchString],
							  [formatter stringFromNumber:@(totalMatches)]
						  ];

	if(!searcher.documentIdentifier)
	{
		msg = [msg stringByAppendingFormat:([searcher scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE),
					[formatter stringFromNumber:@([searcher scannedFileCount])],
					[searcher searchDuration]
				];
	}

	[self setStatusMessage:msg];
}

// ============================
// = Outline view data source =
// ============================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(id)item
{
	if(searcher.hasPerformedReplacement)
			return item ? 1 : [[searcher allDocumentsWithSelectedMatches] count];
	else	return [(item ? [searcher allMatchesForDocumentIdentifier:[item identifier]] : [searcher allDocumentsWithMatches]) count];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(id)item
{
	return [self outlineView:outlineView isGroupItem:item];
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(id)item
{
	if(item)
		return [[searcher allMatchesForDocumentIdentifier:[item identifier]] objectAtIndex:childIndex];
	else if(searcher.hasPerformedReplacement)
		return [[searcher allDocumentsWithSelectedMatches] objectAtIndex:childIndex];
	else
		return [[searcher allDocumentsWithMatches] objectAtIndex:childIndex];
}

- (id)outlineView:(NSOutlineView*)outlineView objectValueForTableColumn:(NSTableColumn*)tableColumn byItem:(id)item
{
	if([self outlineView:outlineView isGroupItem:item])
	{
		return item;
	}
	else if([[tableColumn identifier] isEqualToString:@"checkbox"])
	{
		return @(![self.searcher skipReplacementForMatch:item]);
	}
	else if([[tableColumn identifier] isEqualToString:@"match"])
	{
		if(searcher.hasPerformedReplacement)
		{
			NSUInteger count = [[searcher allSelectedMatchesForDocumentIdentifier:[item identifier]] count];
			return [NSString stringWithFormat:@"%lu occurence%s replaced.", count, count == 1 ? "" : "s"];
		}
		else if([(FFMatch*)item match].binary)
		{
			ns::attr_string_t res;
			res = ns::attr_string_t([NSColor darkGrayColor])
			    << ns::style::line_break(NSLineBreakByTruncatingTail)
			    << "(binary file)";
			return res.get();
		}
		else
		{
			find::match_t const& m = [(FFMatch*)item match];
			std::string str = [(FFMatch*)item matchText];

			size_t from = std::min<size_t>(m.first - m.bol_offset, str.size());
			size_t to   = std::min<size_t>(m.last  - m.bol_offset, str.size());

			std::string prefix = str.substr(0, from);
			std::string middle = str.substr(from, to - from);
			std::string suffix = str.substr(to);

			if(!suffix.empty() && suffix[suffix.size()-1] == '\n')
				suffix = suffix.substr(0, suffix.size()-1);

			if(utf8::is_valid(prefix.begin(), prefix.end()) && utf8::is_valid(middle.begin(), middle.end()) && utf8::is_valid(suffix.begin(), suffix.end()))
			{
				if(previewReplacements && ![self.searcher skipReplacementForMatch:item])
					middle = self.findRegularExpression ? format_string::expand(to_s(self.replaceString), m.captures) : to_s(self.replaceString);
				return AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.range.from.line);
			}
			else
			{
				ns::attr_string_t res;
				res = ns::attr_string_t([NSColor darkGrayColor])
				    << ns::style::line_break(NSLineBreakByTruncatingTail)
				    << text::format("%ld-%ld: (file has changed, re-run the search)", m.first, m.last);
				return res.get();
			}
		}
	}
	return nil;
}

- (void)outlineView:(NSOutlineView*)outlineView setObjectValue:(id)objectValue forTableColumn:(NSTableColumn*)tableColumn byItem:(id)item
{
	if(![tableColumn.identifier isEqualToString:@"checkbox"])
		return;

	if(OakIsAlternateKeyOrMouseEvent())
	{
		// Toggle all flags for the file
		for(FFMatch* match in [searcher allMatchesForDocumentIdentifier:[item identifier]])
			[searcher setSkipReplacement:![objectValue boolValue] forMatch:match];
		[outlineView reloadData];
	}
	else
	{
		[searcher setSkipReplacement:![objectValue boolValue] forMatch:item];
	}
}

- (void)outlineView:(NSOutlineView*)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	if([self outlineView:outlineView isGroupItem:item])
	{
		if(!tableColumn && [cell isKindOfClass:[FFFilePathCell class]])
		{
			FFFilePathCell* pathCell = (FFFilePathCell*)cell;
			pathCell.icon = [item icon];
			pathCell.path = [item path] ?: [NSString stringWithCxxString:[(FFMatch*)item match].document->display_name()];
			pathCell.base = [self rootPath];
			pathCell.count = [outlineView isItemExpanded:item] ? 0 : [[searcher allMatchesForDocumentIdentifier:[item identifier]] count];
		}
	}
	else if([[tableColumn identifier] isEqualToString:@"match"] && [cell isHighlighted])
	{
		id obj = [cell objectValue];
		if([obj isKindOfClass:[NSAttributedString class]])
		{
			NSMutableAttributedString* str = [[obj mutableCopy] autorelease];
			[str addAttribute:NSForegroundColorAttributeName value:[NSColor selectedTextColor] range:NSMakeRange(0, [str length])];
			[cell setAttributedStringValue:str];
		}
	}
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(id)item
{
	return [outlineView levelForItem:item] == 0;
}

- (CGFloat)outlineView:(NSOutlineView*)outlineView heightOfRowByItem:(id)item
{
	if([self outlineView:outlineView isGroupItem:item])
		return 22;

	size_t lines = 1;

	find::match_t const& m = [(FFMatch*)item match];
	if(!m.binary && !searcher.hasPerformedReplacement)
	{
		size_t firstLine = m.range.from.line;
		size_t lastLine = m.range.to.line;
		if(firstLine == lastLine || m.range.to.column != 0)
			++lastLine;
		lines = (lastLine - firstLine);
	}

	return lines * [outlineView rowHeight];
}

- (NSCell*)outlineView:(NSOutlineView*)outlineView dataCellForTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	if(tableColumn == nil && [self outlineView:outlineView isGroupItem:item])
		return [[FFFilePathCell new] autorelease];
	return [tableColumn dataCell];
}

// =================
// = GUI callbacks =
// =================

- (IBAction)searchInPopUpChanged:(id)sender
{
	NSInteger tag = [[findInPopUp selectedItem] tag];
	if(tag == MenuItemDocument)
		self.searchIn = FFSearchInDocument;
	else if(tag == MenuItemSelection)
		self.searchIn = FFSearchInSelection;
	else if(tag == MenuItemSelectedFolder)
		self.searchIn = self.searchFolder;
	else if(tag == MenuItemProjectFolder)
		self.searchIn = FFSearchInProjectFolder;
	else if(tag == MenuItemOtherFolder)
		self.searchIn = FFSearchInOtherFolder;
	else if(tag == MenuItemOpenFiles)
		self.searchIn = FFSearchInOpenFiles;
	else if(tag >= MenuItemFirstRecentPlace)
		self.searchIn = [recentFolders objectAtIndex:(tag-MenuItemFirstRecentPlace)];
}

// ==================
// = Action methods =
// ==================

- (void)didDoubleClickFindAllResults:(id)sender
{
	NSInteger clickedColumn = [findAllResultsOutlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[findAllResultsOutlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return;

	[[self window] orderOut:self];
}

- (void)didSingleClickFindAllResults:(id)sender
{
	if([findAllResultsOutlineView numberOfSelectedRows] == 1)
	{
		NSUInteger selectionIndex = [[findAllResultsOutlineView selectedRowIndexes] firstIndex];
		FFMatch* selectedMatch    = [findAllResultsOutlineView itemAtRow:selectionIndex];
		document::show([selectedMatch match].document, searcher.projectIdentifier ? oak::uuid_t(to_s(searcher.projectIdentifier)) : document::kCollectionNew, [selectedMatch match].range, false);
	}
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	[self didSingleClickFindAllResults:self];
}

- (BOOL)commitEditing
{
	id oldResponder = [[self window] firstResponder];
	id view = [oldResponder isKindOfClass:[NSTextView class]] ? [oldResponder delegate] : oldResponder;

	BOOL res = [findController commitEditing];

	if([[self window] firstResponder] != oldResponder && view)
		[[self window] makeFirstResponder:view];

	return res;
}

- (void)openPanelDidEnd:(NSOpenPanel*)panel returnCode:(NSInteger)returnCode
{
	[self.window performSelector:@selector(makeKeyWindow) withObject:nil afterDelay:0.0];
	if(returnCode == NSOKButton)
	{
		self.searchIn = [[[[panel URLs] lastObject] filePathURL] path];
		[self showWindow:self];
	}
	else
	{
		self.searchIn = self.searchIn; // Restore previously selected menu item
	}
}

- (IBAction)performFindAction:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);
	if(self.isDirty)
		return [self displayUnsavedChangesSheetForOperation:new operation_t(operation_t::perform_find, sender)];
	[self commitEditing];
	[self.delegate performFindAction:(FindActionTag)[sender tag] withWindowController:self];
}

- (IBAction)saveAll:(id)sender
{
	D(DBF_Find_WindowController, bug("\n"););
	NSUInteger savedFileCount = [self.searcher saveAllDocuments];
	self.statusMessage = [NSString stringWithFormat:@"%lu file%s saved.", savedFileCount, savedFileCount == 1 ? "" : "s"];
	[findAllResultsOutlineView reloadData]; // To refresh modified state icons
}

- (IBAction)goToParentFolder:(id)sender
{
	ASSERT(![[self.searchFolder stringByDeletingLastPathComponent] isEqualToString:self.searchFolder]);
	self.searchIn = [self.searchFolder stringByDeletingLastPathComponent];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
	if([item action] == @selector(goToParentFolder:))
			return self.isSearchingFolders && ![[self.searchFolder stringByDeletingLastPathComponent] isEqualToString:self.searchFolder];
	else	return YES;
}

// ===========
// = History =
// ===========

- (IBAction)showFindHistory:(id)sender
{
	if(![[[OakPasteboardSelector sharedInstance] window] isVisible])
		[[OakPasteboard pasteboardWithName:NSFindPboard] selectItemForControl:findStringField];
	// if the panel is visible it will automatically be hidden due to the mouse click
}

- (IBAction)showReplaceHistory:(id)sender
{
	if(![[[OakPasteboardSelector sharedInstance] window] isVisible])
		[[OakPasteboard pasteboardWithName:NSReplacePboard] selectItemForControl:replaceStringField];
	// if the panel is visible it will automatically be hidden due to the mouse click
}

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(control == findStringField && command == @selector(moveDown:))
	{
		[self showFindHistory:control];
		return YES;
	}
	else if(control == replaceStringField && command == @selector(moveDown:))
	{
		[self showReplaceHistory:control];
		return YES;
	}
	return NO;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(id)item
{
	NSInteger clickedColumn = [findAllResultsOutlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[findAllResultsOutlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return NO;

	if(clickedColumn == -1)
	{
		NSInteger clickedRow = [findAllResultsOutlineView clickedRow];
		if(clickedRow != -1 && [self outlineView:outlineView isGroupItem:item])
		{
			NSPoint point = [outlineView convertPoint:[[[outlineView window] currentEvent] locationInWindow] fromView:nil];
			NSRect cellFrame = [outlineView frameOfCellAtColumn:clickedColumn row:clickedRow];
			FFFilePathCell* cell = (FFFilePathCell*)[outlineView preparedCellAtColumn:clickedColumn row:clickedRow];
			if(NSMouseInRect(point, [cell iconFrameInCellFrame:cellFrame], [outlineView isFlipped]))
				return NO;
		}
	}

	return YES;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldTrackCell:(NSCell*)cell forTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	return YES;
}
@end
