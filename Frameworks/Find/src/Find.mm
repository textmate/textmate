#import "Find.h"
#import "FindWindowController.h"
#import "FFDocumentSearch.h"
#import "Strings.h"
#import "attr_string.h"
#import "FFFilePathCell.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <ns/ns.h>
#import <text/types.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <editor/editor.h>
#import <document/collection.h>
#import "Strings.h"

OAK_DEBUG_VAR(Find_Base);

NSString* const FFSearchInDocument  = @"FFSearchInDocument";
NSString* const FFSearchInSelection = @"FFSearchInSelection";
NSString* const FFSearchInOpenFiles = @"FFSearchInOpenFiles";

enum FindActionTag
{
	FindActionFindNext = 1,
	FindActionFindPrevious,
	FindActionCountMatches,
	FindActionFindAll,
	FindActionReplaceAll,
	FindActionReplaceAndFind,
	FindActionReplaceSelected,
};

@interface Find () <NSOutlineViewDataSource, NSOutlineViewDelegate>
@property (nonatomic, retain) FindWindowController* windowController;
@property (nonatomic, retain) FFDocumentSearch* documentSearch;
@property (nonatomic, assign) BOOL closeWindowOnSuccess;
@end

NSString* const FFFindWasTriggeredByEnter = @"FFFindWasTriggeredByEnter";

@implementation Find
+ (Find*)sharedInstance
{
	static Find* instance = [Find new];
	return instance;
}

- (FindWindowController*)windowController
{
	if(!_windowController)
	{
		self.windowController = [FindWindowController new];
		self.windowController.nextResponder = self;
		self.windowController.resultsOutlineView.action       = @selector(didSingleClickResultsOutlineView:);
		self.windowController.resultsOutlineView.doubleAction = @selector(didDoubleClickResultsOutlineView:);
		self.windowController.resultsOutlineView.target       = self;
		self.windowController.resultsOutlineView.dataSource   = self;
		self.windowController.resultsOutlineView.delegate     = self;

		[self.windowController.window addObserver:self forKeyPath:@"firstResponder" options:0 context:NULL];
	}
	return _windowController;
}

// ====================================
// = Actions for displaying the panel =
// ====================================

- (void)showFindWindowFor:(NSString*)searchScope
{
	self.windowController.searchIn = searchScope;
	[self updateActionButtons:self];
	[self.windowController showWindow:self];

	if(_documentSearch && _documentSearch.windowControllers.count == 0)
		[_documentSearch addWindowController:self.windowController];
}

- (IBAction)showFolderSelectionPanel:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.title = @"Find in Folder";
	openPanel.canChooseFiles = NO;
	openPanel.canChooseDirectories = YES;
	if(NSString* folder = self.searchFolder)
		openPanel.directoryURL = [NSURL fileURLWithPath:folder];
	if([self.windowController isWindowLoaded] && [self.windowController.window isVisible])
	{
		[openPanel beginSheetModalForWindow:self.windowController.window completionHandler:^(NSInteger result) {
			if(result == NSOKButton)
				[self showFindWindowFor:[[[[openPanel URLs] lastObject] filePathURL] path]];
			else if([self isVisible]) // Reset selected item in pop-up button
				self.windowController.searchIn = self.windowController.searchIn;
		}];
	}
	else
	{
		[openPanel beginWithCompletionHandler:^(NSInteger result) {
			if(result == NSOKButton)
				[self showFindWindowFor:[[[[openPanel URLs] lastObject] filePathURL] path]];
		}];
	}
}

// ================
// = Find actions =
// ================

- (void)updateActionButtons:(id)sender
{
	NSString* replaceAllTitle = @"Replace All";
	BOOL replaceAllEnabled = YES;

	if(self.windowController.showsResultsOutlineView)
	{
		if(_documentSearch.countOfSelectedMatches < _documentSearch.countOfMatches)
			replaceAllTitle = @"Replace Selected";
		if(_documentSearch.hasPerformedReplacement)
			replaceAllEnabled = NO;
	}

	self.windowController.replaceAllButton.title   = replaceAllTitle;
	self.windowController.replaceAllButton.enabled = replaceAllEnabled;
}

- (IBAction)countOccurrences:(id)sender   { [self performFindAction:FindActionCountMatches   withWindowController:self.windowController]; }
- (IBAction)findAll:(id)sender            { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findAllInSelection:(id)sender { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findNext:(id)sender           { [self performFindAction:FindActionFindNext       withWindowController:self.windowController]; }
- (IBAction)findPrevious:(id)sender       { [self performFindAction:FindActionFindPrevious   withWindowController:self.windowController]; }
- (IBAction)replaceAll:(id)sender         { [self performFindAction:FindActionReplaceAll     withWindowController:self.windowController]; }
- (IBAction)replaceAndFind:(id)sender     { [self performFindAction:FindActionReplaceAndFind withWindowController:self.windowController]; }

- (IBAction)stopSearch:(id)sender
{
	[self.documentSearch stop];
	self.windowController.statusString = @"Stopped.";
}

- (void)document:(NSDocument*)aDocument shouldClose:(BOOL)flag contextInfo:(FindActionTag*)context
{
	if(flag)
		[self performFindAction:*context withWindowController:[[aDocument windowControllers] lastObject]];
	delete context;
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FindWindowController*)controller
{
	[controller updateFindErrorString];
	if(controller.findErrorString != nil)
		return;

	if(_documentSearch && _documentSearch.isDocumentEdited)
		return [_documentSearch canCloseDocumentWithDelegate:self shouldCloseSelector:@selector(document:shouldClose:contextInfo:) contextInfo:new FindActionTag(action)];

	_findOptions = (controller.regularExpression ? find::regular_expression : find::none) | (controller.ignoreWhitespace ? find::ignore_whitespace : find::none) | (controller.fullWords ? find::full_words : find::none) | (controller.ignoreCase ? find::ignore_case : find::none) | (controller.wrapAround ? find::wrap_around : find::none);
	if(action == FindActionFindPrevious)
		_findOptions |= find::backwards;
	else if(action == FindActionCountMatches || action == FindActionFindAll || action == FindActionReplaceAll)
		_findOptions |= find::all_matches;

	NSString* folder = controller.searchFolder;
	if(folder || (action == FindActionFindAll && [controller.searchIn isEqualToString:FFSearchInDocument] && self.documentIdentifier))
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				FFDocumentSearch* folderSearch = [FFDocumentSearch new];
				folderSearch.searchString      = controller.findString;
				folderSearch.options           = _findOptions;
				folderSearch.projectIdentifier = self.projectIdentifier;
				if(self.documentIdentifier && [controller.searchIn isEqualToString:FFSearchInDocument])
				{
					folderSearch.documentIdentifier = self.documentIdentifier;
				}
				else
				{
					path::glob_list_t globs;
					if(![controller.searchIn isEqualToString:FFSearchInOpenFiles])
					{
						auto const settings = settings_for_path(NULL_STR, "", to_s(folder));
						globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFolderSearchKey, NULL_STR), path::kPathItemDirectory);
						globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey,               NULL_STR), path::kPathItemDirectory);
						globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInFolderSearchKey,       NULL_STR), path::kPathItemFile);
						globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey,                     NULL_STR), path::kPathItemFile);
						for(auto key : { kSettingsExcludeInFolderSearchKey, kSettingsExcludeKey, kSettingsBinaryKey })
							globs.add_exclude_glob(settings.get(key, NULL_STR));
						globs.add_include_glob(controller.searchHiddenFolders ? "{,.}*" : "*", path::kPathItemDirectory);
						globs.add_include_glob(to_s(controller.globString), path::kPathItemFile);
					}

					find::folder_scan_settings_t search([controller.searchIn isEqualToString:FFSearchInOpenFiles] ? find::folder_scan_settings_t::open_files : to_s(folder), globs, controller.followSymbolicLinks);
					[folderSearch setFolderOptions:search];
				}
				self.documentSearch = folderSearch;
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);
				for(FFMatch* fileMatch in [self.documentSearch allDocumentsWithSelectedMatches])
				{
					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					for(FFMatch* match in [self.documentSearch allSelectedMatchesForDocumentIdentifier:[fileMatch identifier]])
					{
						++replaceCount;
						replacements.insert(std::make_pair(std::make_pair([match match].first, [match match].last), controller.regularExpression ? format_string::expand(replaceString, [match match].captures) : replaceString));
					}

					if(document::document_ptr doc = [fileMatch match].document)
					{
						if(doc->is_open())
						{
							ng::editor_ptr editor = ng::editor_for_document(doc);
							doc->undo_manager().begin_undo_group(editor->ranges());
							editor->perform_replacements(replacements);
							doc->undo_manager().end_undo_group(editor->ranges());
						}
						else
						{
							doc->replace(replacements);
						}
					}

					++fileCount;
				}
				self.documentSearch.hasPerformedReplacement = YES;
				self.windowController.statusString = [NSString stringWithFormat:MSG_REPLACE_ALL_RESULTS, replaceCount, fileCount];
				self.windowController.disableResultsCheckBoxes = YES;
				self.windowController.window.documentEdited = replaceCount != 0;
			}
			break;

			case FindActionFindNext:     [self.windowController selectNextResult:self];     break;
			case FindActionFindPrevious: [self.windowController selectPreviousResult:self]; break;
		}
	}
	else
	{
		bool onlySelection = [controller.searchIn isEqualToString:FFSearchInSelection];
		switch(action)
		{
			case FindActionFindNext:
			case FindActionFindPrevious:
			case FindActionFindAll:        _findOperation = onlySelection ? kFindOperationFindInSelection       : kFindOperationFind;       break;
			case FindActionCountMatches:   _findOperation = onlySelection ? kFindOperationCountInSelection      : kFindOperationCount;      break;
			case FindActionReplaceAll:     _findOperation = onlySelection ? kFindOperationReplaceAllInSelection : kFindOperationReplaceAll; break;
			case FindActionReplaceAndFind: _findOperation = kFindOperationReplaceAndFind;                                                   break;
		}

		self.closeWindowOnSuccess = action == FindActionFindNext && [[NSApp currentEvent] type] == NSKeyDown && to_s([NSApp currentEvent]) == utf8::to_s(NSCarriageReturnCharacter);
		[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = nil;
		[NSApp sendAction:@selector(performFindOperation:) to:nil from:self];
	}
	[self updateActionButtons:self];
}

- (NSString*)findString    { return self.windowController.findString    ?: @""; }
- (NSString*)replaceString { return self.windowController.replaceString ?: @""; }

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap
{
	static std::string const formatStrings[4][3] = {
		{ "No more occurrences of “${found}”.", "Found “${found}”${line:+ at line ${line}, column ${column}}.",               "${count} occurrences of “${found}”." },
		{ "No more matches for “${found}”.",    "Found one match for “${found}”${line:+ at line ${line}, column ${column}}.", "${count} matches for “${found}”."    },
	};

	std::map<std::string, std::string> variables;
	variables["count"]  = std::to_string(aNumber);
	variables["found"]  = to_s(aFindString);
	variables["line"]   = aPosition ? std::to_string(aPosition.line + 1)   : NULL_STR;
	variables["column"] = aPosition ? std::to_string(aPosition.column + 1) : NULL_STR;
	self.windowController.statusString = [NSString stringWithCxxString:format_string::expand(formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][std::min<size_t>(aNumber, 2)], variables)];

	if(self.closeWindowOnSuccess && aNumber != 0)
		return [self.windowController close];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	self.windowController.statusString = [NSString stringWithFormat:format, aFindString, aNumber];
}

// =============
// = Accessors =
// =============

- (void)setProjectFolder:(NSString*)folder { self.windowController.projectFolder = folder; }
- (NSString*)projectFolder                 { return self.windowController.projectFolder; }
- (NSString*)searchFolder                  { return self.windowController.searchFolder; }
- (BOOL)isVisible                          { return self.windowController.window.isVisible; }

// ===========
// = Options =
// ===========

- (IBAction)takeFindOptionToToggleFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);

	find::options_t option = find::options_t([sender tag]);
	switch(option)
	{
		case find::full_words:         self.windowController.fullWords         = !self.windowController.fullWords;         break;
		case find::ignore_case:        self.windowController.ignoreCase        = !self.windowController.ignoreCase;        break;
		case find::ignore_whitespace:  self.windowController.ignoreWhitespace  = !self.windowController.ignoreWhitespace;  break;
		case find::regular_expression: self.windowController.regularExpression = !self.windowController.regularExpression; break;
		case find::wrap_around:        self.windowController.wrapAround        = !self.windowController.wrapAround;        break;
		default:
			ASSERTF(false, "Unknown find option tag %d\n", option);
	}

	if([[[[OakPasteboard pasteboardWithName:NSFindPboard] current] string] isEqualToString:self.windowController.findString])
		[self.windowController commitEditing]; // update the options on the pasteboard immediately if the find string has not been changed
}

// ====================
// = Search in Folder =
// ====================

- (void)setDocumentSearch:(FFDocumentSearch*)newSearcher
{
	if(_documentSearch)
	{
		[_documentSearch removeWindowController:self.windowController];
		[_documentSearch removeObserver:self forKeyPath:@"currentPath"];
		[_documentSearch removeObserver:self forKeyPath:@"hasPerformedReplacement"];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidFinishNotification object:_documentSearch];

		for(FFMatch* fileMatch in [_documentSearch allDocumentsWithMatches])
		{
			if(document::document_ptr doc = [fileMatch match].document)
				doc->remove_all_marks("search");
		}

		_documentSearch = nil;
	}

	[self.windowController.resultsOutlineView reloadData];
	[self.windowController.resultsOutlineView deselectAll:nil];
	self.windowController.showResultsCollapsed = NO;

	if(_documentSearch = newSearcher)
	{
		self.windowController.busy                     = YES;
		self.windowController.statusString             = MSG_SEARCHING_FMT;
		self.windowController.showsResultsOutlineView  = YES;
		self.windowController.disableResultsCheckBoxes = _documentSearch.documentIdentifier != nil;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addWindowController:self.windowController];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		[_documentSearch addObserver:self forKeyPath:@"hasPerformedReplacement" options:0 context:NULL];
		[_documentSearch start];
	}

	[self updateActionButtons:self];
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	NSInteger first = [self.windowController.resultsOutlineView numberOfRows];
	[self.windowController.resultsOutlineView reloadData];
	NSInteger last = [self.windowController.resultsOutlineView numberOfRows];
	while(last-- != first)
		[self.windowController.resultsOutlineView expandItem:[self.windowController.resultsOutlineView itemAtRow:last]];
	[self.windowController.resultsOutlineView sizeLastColumnToFit];
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	self.windowController.busy = NO;
	if(!_documentSearch)
		return;

	NSMutableArray* documents = [NSMutableArray array];
	for(FFMatch* fileMatch in [_documentSearch allDocumentsWithMatches])
	{
		if(document::document_ptr doc = [fileMatch match].document)
		{
			for(FFMatch* match in [_documentSearch allMatchesForDocumentIdentifier:[NSString stringWithCxxString:doc->identifier()]])
				doc->add_mark([match match].range, "search");

			NSArray* matches = [_documentSearch allMatchesForDocumentIdentifier:[NSString stringWithCxxString:doc->identifier()]];
			if([matches firstObject] && [matches lastObject])
			{
				FFMatch* firstMatch = [matches firstObject];
				FFMatch* lastMatch  = [matches lastObject];
				[documents addObject:@{
					@"identifier"      : [NSString stringWithCxxString:doc->identifier()],
					@"firstMatchRange" : [NSString stringWithCxxString:[firstMatch match].range],
					@"lastMatchRange"  : [NSString stringWithCxxString:[lastMatch match].range],
				}];
			}
		}
	}
	[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : documents };

	NSUInteger totalMatches = [_documentSearch countOfMatches];
	NSString* fmt = MSG_ZERO_MATCHES_FMT;
	switch(totalMatches)
	{
		case 0:  fmt = MSG_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_ONE_MATCH_FMT;        break;
		default: fmt = MSG_MULTIPLE_MATCHES_FMT; break;
	}

	NSString* msg = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(totalMatches) numberStyle:NSNumberFormatterDecimalStyle]];
	if(!_documentSearch.documentIdentifier)
		msg = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE), [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle], [_documentSearch searchDuration]];

	self.windowController.statusString = msg;
}

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

		std::string relative = path::relative_to(searchPath, to_s(self.searchFolder));
		if(path::is_directory(searchPath))
			relative += "/";

		self.windowController.statusString = [NSString localizedStringWithFormat:MSG_SEARCHING_FOLDER_FMT, [NSString stringWithCxxString:relative]];
	}
	else if([keyPath isEqualToString:@"hasPerformedReplacement"])
	{
		[self.windowController.resultsOutlineView reloadData];
	}
}

// ============================
// = Outline view data source =
// ============================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(id)item
{
	if(_documentSearch.hasPerformedReplacement)
			return item ? 1 : [[_documentSearch allDocumentsWithSelectedMatches] count];
	else	return [(item ? [_documentSearch allMatchesForDocumentIdentifier:[item identifier]] : [_documentSearch allDocumentsWithMatches]) count];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(id)item
{
	return [self outlineView:outlineView isGroupItem:item];
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(id)item
{
	if(item)
		return [[_documentSearch allMatchesForDocumentIdentifier:[item identifier]] objectAtIndex:childIndex];
	else if(_documentSearch.hasPerformedReplacement)
		return [[_documentSearch allDocumentsWithSelectedMatches] objectAtIndex:childIndex];
	else
		return [[_documentSearch allDocumentsWithMatches] objectAtIndex:childIndex];
}

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n)
{
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

- (id)outlineView:(NSOutlineView*)outlineView objectValueForTableColumn:(NSTableColumn*)tableColumn byItem:(id)item
{
	if([self outlineView:outlineView isGroupItem:item])
	{
		return item;
	}
	else if([[tableColumn identifier] isEqualToString:@"checkbox"])
	{
		return @(![_documentSearch skipReplacementForMatch:item]);
	}
	else if([[tableColumn identifier] isEqualToString:@"match"])
	{
		if(_documentSearch.hasPerformedReplacement)
		{
			NSUInteger count = [[_documentSearch allSelectedMatchesForDocumentIdentifier:[item identifier]] count];
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
				if(self.windowController.showReplacementPreviews && ![_documentSearch skipReplacementForMatch:item])
					middle = self.windowController.regularExpression ? format_string::expand(to_s(self.replaceString), m.captures) : to_s(self.replaceString);
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
		for(FFMatch* match in [_documentSearch allMatchesForDocumentIdentifier:[item identifier]])
			[_documentSearch setSkipReplacement:![objectValue boolValue] forMatch:match];
		[outlineView reloadData];
	}
	else
	{
		[_documentSearch setSkipReplacement:![objectValue boolValue] forMatch:item];
	}

	[self updateActionButtons:self];
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
			pathCell.base = self.searchFolder;
			pathCell.count = [outlineView isItemExpanded:item] ? 0 : [[_documentSearch allMatchesForDocumentIdentifier:[item identifier]] count];
		}
	}
	else if([[tableColumn identifier] isEqualToString:@"match"] && [cell isHighlighted])
	{
		id obj = [cell objectValue];
		if([obj isKindOfClass:[NSAttributedString class]])
		{
			NSMutableAttributedString* str = [obj mutableCopy];
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
	if(!m.binary && !_documentSearch.hasPerformedReplacement)
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
		return [FFFilePathCell new];
	return [tableColumn dataCell];
}

// ======================
// = Should Select Item =
// ======================

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(id)item
{
	if([self outlineView:outlineView isGroupItem:item])
		return NO;

	NSInteger clickedColumn = [outlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[outlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return NO;

	return YES;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldTrackCell:(NSCell*)cell forTableColumn:(NSTableColumn*)tableColumn item:(id)item
{
	return YES;
}

// =============================
// = Selecting Results Actions =
// =============================

- (void)didDoubleClickResultsOutlineView:(id)sender
{
	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	NSInteger clickedColumn = [outlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[outlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return;

	[self.windowController close];
}

- (void)openDocumentForSelectedRow:(id)sender
{
	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	if([outlineView numberOfSelectedRows] == 1)
	{
		NSUInteger selectionIndex = [[outlineView selectedRowIndexes] firstIndex];
		FFMatch* selectedMatch    = [outlineView itemAtRow:selectionIndex];
		auto doc = [selectedMatch match].document;
		if(!doc->is_open())
			doc->set_recent_tracking(false);
		document::show(doc, _documentSearch.projectIdentifier ? oak::uuid_t(to_s(_documentSearch.projectIdentifier)) : document::kCollectionAny, [selectedMatch match].range, false);
	}
}

- (void)didSingleClickResultsOutlineView:(id)sender
{
	[self openDocumentForSelectedRow:self];
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	if([[NSApp currentEvent] type] != NSLeftMouseUp)
		[self openDocumentForSelectedRow:self];
}

// ==================
// = Go to… Submenu =
// ==================

- (IBAction)takeSelectedPathFrom:(id)sender
{
	if(NSString* path = [sender representedObject])
	{
		for(FFMatch* match in _documentSearch.allDocumentsWithMatches)
		{
			if([match.path isEqualToString:path])
			{
				NSOutlineView* outlineView = self.windowController.resultsOutlineView;
				NSUInteger row = [outlineView rowForItem:match];
				if(![outlineView isItemExpanded:match])
					[outlineView expandItem:match];
				NSUInteger firstMatch = row + ([outlineView isItemExpanded:match] ? 1 : 0);
				[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:firstMatch] byExtendingSelection:NO];
				[outlineView scrollRowToVisible:outlineView.numberOfRows-1];
				[outlineView scrollRowToVisible:row];
				[outlineView.window makeFirstResponder:outlineView];
				break;
			}
		}
	}
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(_documentSearch.allDocumentsWithMatches.count == 0)
	{
		[[aMenu addItemWithTitle:@"No Results" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		char key = 0;
		for(FFMatch* match in _documentSearch.allDocumentsWithMatches)
		{
			document::document_ptr doc = [match match].document;
			NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:doc->path() == NULL_STR ? doc->display_name() : path::relative_to(doc->path(), to_s(self.searchFolder))] action:@selector(takeSelectedPathFrom:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
			[item setImage:match.icon];
			[item setRepresentedObject:match.path];
		}
	}
}

// =====================
// = Copy Find Results =
// =====================

- (void)copyEntireLines:(BOOL)entireLines withFilename:(BOOL)withFilename
{
	std::vector<std::string> res;

	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	NSIndexSet* selectedRows = [outlineView numberOfSelectedRows] == 0 ? [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, [outlineView numberOfRows])] : [outlineView selectedRowIndexes];
	for(NSUInteger index = [selectedRows firstIndex]; index != NSNotFound; index = [selectedRows indexGreaterThanIndex:index])
	{
		FFMatch* item = [outlineView itemAtRow:index];
		if([self outlineView:outlineView isGroupItem:item])
			continue;

		find::match_t const& m = [item match];
		std::string str = [item matchText];
		size_t from = std::min<size_t>(m.first - m.bol_offset, str.size());
		size_t to   = std::min<size_t>(m.last  - m.bol_offset, str.size());

		if(!entireLines)
			str = str.substr(from, to - from);
		else if(str.size() && str[str.size()-1] == '\n')
			str.erase(str.size()-1);

		if(withFilename)
			str = text::format("%s:%lu\t", [item.path UTF8String], m.range.from.line + 1) + str;

		res.push_back(str);
	}

	[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
	[[NSPasteboard generalPasteboard] setString:[NSString stringWithCxxString:text::join(res, "\n")] forType:NSStringPboardType];
}

- (void)copy:(id)sender                          { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyMatchingParts:(id)sender             { [self copyEntireLines:NO  withFilename:NO ]; }
- (void)copyMatchingPartsWithFilename:(id)sender { [self copyEntireLines:NO  withFilename:YES]; }
- (void)copyEntireLines:(id)sender               { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyEntireLinesWithFilename:(id)sender   { [self copyEntireLines:YES withFilename:YES]; }
@end
