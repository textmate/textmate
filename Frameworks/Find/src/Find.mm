#import "Find.h"
#import "FFResultNode.h"
#import "FindWindowController.h"
#import "FFResultsViewController.h"
#import "FFDocumentSearch.h"
#import "Strings.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <ns/ns.h>
#import <text/types.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <editor/editor.h>
#import <document/collection.h>

OAK_DEBUG_VAR(Find_Base);

NSString* const FFSearchInDocument  = @"FFSearchInDocument";
NSString* const FFSearchInSelection = @"FFSearchInSelection";
NSString* const FFSearchInOpenFiles = @"FFSearchInOpenFiles";

static std::string const kSearchMarkIdentifier = "search";

enum FindActionTag
{
	FindActionFindNext = 1,
	FindActionFindPrevious,
	FindActionCountMatches,
	FindActionFindAll,
	FindActionReplaceAll,
	FindActionReplaceAndFind,
	FindActionReplaceSelected,
	FindActionReplace,
};

@interface Find () <OakFindServerProtocol>
@property (nonatomic) FindWindowController* windowController;
@property (nonatomic) FFDocumentSearch* documentSearch;
@property (nonatomic) FFResultNode* results;
@property (nonatomic) NSUInteger countOfMatches;
@property (nonatomic) NSUInteger countOfExcludedMatches;
@property (nonatomic) BOOL closeWindowOnSuccess;
@property (nonatomic) BOOL performingFolderSearch;

// =========================
// = OakFindProtocolServer =
// =========================

@property (nonatomic) find_operation_t findOperation;
@property (nonatomic) find::options_t  findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
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
		_windowController = [FindWindowController new];
		_windowController.nextResponder = self;

		_windowController.resultsViewController.selectResultAction      = @selector(didSelectResult:);
		_windowController.resultsViewController.removeResultAction      = @selector(didRemoveResult:);
		_windowController.resultsViewController.doubleClickResultAction = @selector(didDoubleClickResult:);
		_windowController.resultsViewController.target                  = self;

		[_windowController.replaceAllButton bind:@"title" toObject:self withKeyPath:@"replaceAllButtonTitle" options:nil];
		[_windowController.replaceAllButton bind:@"enabled2" toObject:self withKeyPath:@"canReplaceAll" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:_windowController.window];
	}
	return _windowController;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self stopSearch:self];
}

// ====================================
// = Actions for displaying the panel =
// ====================================

- (void)showFindWindowFor:(NSString*)searchScope
{
	self.windowController.searchIn = searchScope;
	[self.windowController showWindow:self];
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

+ (NSSet*)keyPathsForValuesAffectingCanReplaceAll         { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"windowController.showsResultsOutlineView" ]]; }
+ (NSSet*)keyPathsForValuesAffectingReplaceAllButtonTitle { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"windowController.showsResultsOutlineView" ]]; }

- (BOOL)canReplaceAll                { return _windowController.showsResultsOutlineView ? (_countOfExcludedMatches < _countOfMatches) : YES; }
- (NSString*)replaceAllButtonTitle   { return _windowController.showsResultsOutlineView && _countOfExcludedMatches && self.canReplaceAll ? @"Replace Selected" : @"Replace All"; }

- (IBAction)countOccurrences:(id)sender   { [self performFindAction:FindActionCountMatches   withWindowController:self.windowController]; }
- (IBAction)findAll:(id)sender            { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findAllInSelection:(id)sender { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findNext:(id)sender           { [self performFindAction:FindActionFindNext       withWindowController:self.windowController]; }
- (IBAction)findPrevious:(id)sender       { [self performFindAction:FindActionFindPrevious   withWindowController:self.windowController]; }
- (IBAction)replaceAll:(id)sender         { [self performFindAction:FindActionReplaceAll     withWindowController:self.windowController]; }
- (IBAction)replaceAndFind:(id)sender     { [self performFindAction:FindActionReplaceAndFind withWindowController:self.windowController]; }
- (IBAction)replace:(id)sender            { [self performFindAction:FindActionReplace        withWindowController:self.windowController]; }

- (IBAction)stopSearch:(id)sender
{
	if(_performingFolderSearch)
	{
		[_documentSearch stop];
		self.windowController.statusString = @"Stopped.";
	}
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FindWindowController*)controller
{
	[controller updateFindErrorString];
	if(controller.findErrorString != nil)
		return;

	_findOptions = (controller.regularExpression ? find::regular_expression : find::none) | (controller.ignoreWhitespace ? find::ignore_whitespace : find::none) | (controller.fullWords ? find::full_words : find::none) | (controller.ignoreCase ? find::ignore_case : find::none) | (controller.wrapAround ? find::wrap_around : find::none);
	if(action == FindActionFindPrevious)
		_findOptions |= find::backwards;
	else if(action == FindActionCountMatches || action == FindActionFindAll || action == FindActionReplaceAll)
		_findOptions |= find::all_matches;

	NSString* folder = controller.searchFolder;
	if(folder || [controller.searchIn isEqualToString:FFSearchInOpenFiles] || (action == FindActionFindAll && [controller.searchIn isEqualToString:FFSearchInDocument] && self.documentIdentifier))
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				[self clearMatches];

				FFDocumentSearch* folderSearch = [FFDocumentSearch new];
				folderSearch.searchString = controller.findString;
				folderSearch.options      = _findOptions;

				if(self.documentIdentifier && [controller.searchIn isEqualToString:FFSearchInDocument])
				{
					folderSearch.documentIdentifier = self.documentIdentifier;
				}
				else if([controller.searchIn isEqualToString:FFSearchInOpenFiles])
				{
					folderSearch.directory = [NSString stringWithCxxString:find::kSearchOpenFiles];
				}
				else
				{
					auto const settings = settings_for_path(NULL_STR, "", to_s(folder));

					path::glob_list_t globs;
					globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFolderSearchKey, NULL_STR), path::kPathItemDirectory);
					globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey,               NULL_STR), path::kPathItemDirectory);
					globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInFolderSearchKey,       NULL_STR), path::kPathItemFile);
					globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey,                     NULL_STR), path::kPathItemFile);
					for(auto key : { kSettingsExcludeInFolderSearchKey, kSettingsExcludeKey, kSettingsBinaryKey })
						globs.add_exclude_glob(settings.get(key, NULL_STR));
					globs.add_include_glob(controller.searchHiddenFolders ? "{,.}*" : "*", path::kPathItemDirectory);
					globs.add_include_glob(to_s(controller.globString), path::kPathItemFile);

					folderSearch.directory         = folder;
					folderSearch.globList          = globs;
					folderSearch.searchFolderLinks = controller.searchFolderLinks;
					folderSearch.searchFileLinks   = controller.searchFileLinks;
					folderSearch.searchBinaryFiles = controller.searchBinaryFiles;
				}

				self.documentSearch = folderSearch;
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);

				for(FFResultNode* parent in _results.children)
				{
					if(parent.countOfExcluded == parent.countOfLeafs)
						continue;

					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					for(FFResultNode* child in parent.children)
					{
						if(child.excluded)
							continue;
						child.replaceString = controller.replaceString;
						replacements.emplace(std::make_pair(child.match.first, child.match.last), controller.regularExpression ? format_string::expand(replaceString, child.match.captures) : replaceString);
					}

					if(document::document_ptr doc = parent.document)
					{
						if(doc->is_open())
						{
							ng::editor_ptr editor = ng::editor_for_document(doc);
							doc->undo_manager().begin_undo_group(editor->ranges());
							editor->perform_replacements(replacements);
							doc->undo_manager().end_undo_group(editor->ranges());
							doc->set_revision(doc->buffer().revision());
						}
						else
						{
							if(!doc->replace(replacements, parent.match.crc32))
							{
								[parent.children setValue:nil forKey:@"replaceString"];
								continue;
							}
							doc->sync_save(kCFRunLoopDefaultMode);
							doc->set_content(NULL_STR);
						}

						parent.ignored = YES;
						replaceCount += replacements.size();
						++fileCount;
					}
				}
				self.windowController.statusString = [NSString stringWithFormat:MSG_REPLACE_ALL_RESULTS, [NSNumberFormatter localizedStringFromNumber:@(replaceCount) numberStyle:NSNumberFormatterDecimalStyle], [NSNumberFormatter localizedStringFromNumber:@(fileCount) numberStyle:NSNumberFormatterDecimalStyle]];
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
			case FindActionReplace:        _findOperation = kFindOperationReplace;                                                          break;
		}

		self.closeWindowOnSuccess = action == FindActionFindNext && [[NSApp currentEvent] type] == NSKeyDown && to_s([NSApp currentEvent]) == utf8::to_s(NSCarriageReturnCharacter);
		[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = nil;
		[NSApp sendAction:@selector(performFindOperation:) to:nil from:self];
	}
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

	NSResponder* keyView = [[NSApp keyWindow] firstResponder];
	id element = [keyView respondsToSelector:@selector(cell)] ? [keyView performSelector:@selector(cell)] : keyView;
	if([element respondsToSelector:@selector(accessibilityIsIgnored)] && ![element accessibilityIsIgnored])
		NSAccessibilityPostNotificationWithUserInfo(element, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey : self.windowController.statusString });

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

- (void)clearMatches
{
	if(_results)
	{
		for(FFResultNode* parent in _results.children)
		{
			if(document::document_ptr doc = parent.document)
				doc->remove_all_marks(kSearchMarkIdentifier);
		}

		[self unbind:@"countOfMatches"];
		[self unbind:@"countOfExcludedMatches"];
	}

	_windowController.resultsViewController.results = _results = [FFResultNode new];
	[self bind:@"countOfMatches" toObject:_results withKeyPath:@"countOfLeafs" options:nil];
	[self bind:@"countOfExcludedMatches" toObject:_results withKeyPath:@"countOfExcluded" options:nil];
}

- (void)setDocumentSearch:(FFDocumentSearch*)newSearcher
{
	if(_documentSearch)
	{
		[_documentSearch removeObserver:self forKeyPath:@"currentPath"];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidFinishNotification object:_documentSearch];
	}

	if(_documentSearch = newSearcher)
	{
		self.windowController.busy                    = YES;
		self.windowController.statusString            = MSG_SEARCHING_FMT;
		self.windowController.showsResultsOutlineView = YES;

		self.windowController.resultsViewController.hideCheckBoxes = _documentSearch.documentIdentifier != nil;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		self.performingFolderSearch = YES;
		[_documentSearch start];
	}
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	NSUInteger countOfExistingItems = _results.children.count;

	NSArray* matches = [aNotification userInfo][@"matches"];
	FFResultNode* parent = nil;
	for(FFMatch* match in matches)
	{
		find::match_t const& m = [match match];
		if(document::document_ptr doc = m.document)
			doc->add_mark(m.range.from, kSearchMarkIdentifier);

		FFResultNode* node = [FFResultNode resultNodeWithMatch:m];
		if(!parent || parent.document->identifier() != node.document->identifier())
			[_results addResultNode:(parent = [FFResultNode resultNodeWithMatch:m baseDirectory:_documentSearch.directory])];
		[parent addResultNode:node];
	}

	[_windowController.resultsViewController insertItemsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(countOfExistingItems, _results.children.count - countOfExistingItems)]];
}

- (void)addResultsToPasteboard:(id)sender
{
	NSMutableArray* documents = [NSMutableArray array];
	for(FFResultNode* parent in _results.children)
	{
		[documents addObject:@{
			@"identifier"      : [NSString stringWithCxxString:parent.firstResultNode.document->identifier()],
			@"firstMatchRange" : [NSString stringWithCxxString:parent.firstResultNode.match.range],
			@"lastMatchRange"  : [NSString stringWithCxxString:parent.lastResultNode.match.range],
		}];
	}
	[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : documents };
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	self.performingFolderSearch = NO;
	self.windowController.busy = NO;
	if(!_documentSearch)
		return;

	[self addResultsToPasteboard:self];

	NSString* fmt = MSG_ZERO_MATCHES_FMT;
	switch(self.countOfMatches)
	{
		case 0:  fmt = MSG_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_ONE_MATCH_FMT;        break;
		default: fmt = MSG_MULTIPLE_MATCHES_FMT; break;
	}

	NSString* msg = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
	if(_documentSearch.documentIdentifier)
	{
		self.windowController.statusString = msg;
	}
	else
	{
		NSNumberFormatter* formatter = [NSNumberFormatter new];
		formatter.numberStyle = NSNumberFormatterDecimalStyle;
		formatter.maximumFractionDigits = 1;
		NSString* seconds = [formatter stringFromNumber:@([_documentSearch searchDuration])];

		self.windowController.statusString          = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE), [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle], seconds];
		self.windowController.alternateStatusString = [msg stringByAppendingFormat:MSG_SEARCHED_BYTES, [NSString stringWithCxxString:text::format_size([_documentSearch scannedByteCount])], seconds];
	}

	__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSFindPboard] queue:nil usingBlock:^(NSNotification*){
		if(_results)
		{
			for(FFResultNode* parent in _results.children)
			{
				if(document::document_ptr doc = parent.document)
					doc->remove_all_marks(kSearchMarkIdentifier);
			}
		}
		[[NSNotificationCenter defaultCenter] removeObserver:observerId];
	}];
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
}

// =============================
// = Selecting Results Actions =
// =============================

- (void)didSelectResult:(FFResultNode*)item
{
	auto doc = item.document;
	if(!doc->is_open())
		doc->set_recent_tracking(false);
	document::show(doc, self.projectIdentifier ? oak::uuid_t(to_s(self.projectIdentifier)) : document::kCollectionAny, item.match.range, false);
}

- (void)didDoubleClickResult:(FFResultNode*)item
{
	[self.windowController close];
}

- (void)didRemoveResult:(FFResultNode*)item
{
	if(OakIsAlternateKeyOrMouseEvent())
	{
		if(item.document->path() != NULL_STR)
		{
			std::string path = path::relative_to(item.document->path(), to_s(_documentSearch.directory));
			NSString* newGlob = [_windowController.globString stringByAppendingFormat:@"~%@", [NSString stringWithCxxString:path]];
			_windowController.globString = newGlob;
		}
	}

	if(document::document_ptr doc = item.document)
		doc->remove_all_marks(kSearchMarkIdentifier);

	[self addResultsToPasteboard:self];

	NSString* fmt = MSG_SHOWING_ZERO_MATCHES_FMT;
	switch(self.countOfMatches)
	{
		case 0:  fmt = MSG_SHOWING_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_SHOWING_ONE_MATCH_FMT;        break;
		default: fmt = MSG_SHOWING_MULTIPLE_MATCHES_FMT; break;
	}
	_windowController.statusString = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
}

// ==================
// = Go to… Submenu =
// ==================

- (IBAction)takeSelectedPathFrom:(id)sender
{
	FFResultNode* item = [sender representedObject];
	if([item isKindOfClass:[FFResultNode class]])
		[_windowController.resultsViewController showResultNode:item.firstResultNode];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.countOfMatches == 0)
	{
		[[aMenu addItemWithTitle:@"No Results" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		char key = 0;
		for(FFResultNode* parent in _results.children)
		{
			if(document::document_ptr doc = parent.document)
			{
				NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:doc->path() == NULL_STR ? doc->display_name() : path::relative_to(doc->path(), to_s(self.searchFolder))] action:@selector(takeSelectedPathFrom:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
				[item setImage:parent.icon];
				[item setRepresentedObject:parent];
			}
		}
	}
}

// =====================
// = Copy Find Results =
// =====================

- (void)copyEntireLines:(BOOL)entireLines withFilename:(BOOL)withFilename
{
	std::vector<std::string> res;

	for(FFResultNode* item in _windowController.resultsViewController.selectedResults)
	{
		find::match_t const& m = item.match;
		std::string str = m.excerpt;

		if(!entireLines)
			str = str.substr(m.first - m.excerpt_offset, m.last - m.first);
		else if(str.size() && str.back() == '\n')
			str.erase(str.size()-1);

		if(withFilename)
			str = text::format("%s:%lu\t", [item.path UTF8String], m.line_number + 1) + str;

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

// =====================
// = Check/Uncheck All =
// =====================

- (void)allMatchesSetExclude:(BOOL)exclude
{
	_results.excluded = exclude;
}

- (IBAction)checkAll:(id)sender
{
	[self allMatchesSetExclude:NO];
}

- (IBAction)uncheckAll:(id)sender
{
	[self allMatchesSetExclude:YES];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	static std::set<SEL> const copyActions = { @selector(copy:), @selector(copyMatchingParts:), @selector(copyMatchingPartsWithFilename:), @selector(copyEntireLines:), @selector(copyEntireLinesWithFilename:) };
	if(copyActions.find(aMenuItem.action) != copyActions.end())
		return [_results countOfLeafs] != 0;
	else if(aMenuItem.action == @selector(checkAll:))
		return self.countOfExcludedMatches > 0;
	else if(aMenuItem.action == @selector(uncheckAll:) )
		return self.countOfExcludedMatches < self.countOfMatches;
	return YES;
}
@end
