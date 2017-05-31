#import "Find.h"
#import "FFResultNode.h"
#import "FindWindowController.h"
#import "FFResultsViewController.h"
#import "FFDocumentSearch.h"
#import "CommonAncestor.h"
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
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <settings/settings.h>

OAK_DEBUG_VAR(Find_Base);

static NSString* const kUserDefaultsKeepSearchResultsOnDoubleClick = @"keepSearchResultsOnDoubleClick";
static NSString* const kSearchMarkIdentifier = @"search";

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
@property (nonatomic) NSUInteger countOfReadOnlyMatches;
@property (nonatomic) NSUInteger countOfExcludedReadOnlyMatches;
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
+ (instancetype)sharedInstance
{
	static Find* sharedInstance = [self new];
	return sharedInstance;
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

		[_windowController.replaceAllButton bind:NSTitleBinding toObject:self withKeyPath:@"replaceAllButtonTitle" options:nil];
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

- (void)showWindow:(id)sender
{
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
			if(result == NSFileHandlingPanelOKButton)
			{
				self.windowController.otherFolder = [[[[openPanel URLs] lastObject] filePathURL] path];
				self.windowController.searchTarget = FFSearchTargetOther;
			}
			else if([self isVisible]) // Reset selected item in pop-up button
				self.windowController.searchTarget = self.windowController.searchTarget;
		}];
	}
	else
	{
		[openPanel beginWithCompletionHandler:^(NSInteger result) {
			if(result == NSFileHandlingPanelOKButton)
			{
				self.windowController.otherFolder = [[[[openPanel URLs] lastObject] filePathURL] path];
				self.windowController.searchTarget = FFSearchTargetOther;
				[self showWindow:self];
			}
		}];
	}
}

// ================
// = Find actions =
// ================

+ (NSSet*)keyPathsForValuesAffectingCanReplaceAll         { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"countOfReadOnlyMatches", @"countOfExcludedReadOnlyMatches", @"windowController.showsResultsOutlineView" ]]; }
+ (NSSet*)keyPathsForValuesAffectingReplaceAllButtonTitle { return [NSSet setWithArray:@[ @"countOfMatches", @"countOfExcludedMatches", @"countOfReadOnlyMatches", @"countOfExcludedReadOnlyMatches", @"windowController.showsResultsOutlineView" ]]; }

- (BOOL)canReplaceAll                { return _windowController.showsResultsOutlineView ? (_countOfExcludedMatches - _countOfExcludedReadOnlyMatches < _countOfMatches - _countOfReadOnlyMatches) : YES; }
- (NSString*)replaceAllButtonTitle   { return _windowController.showsResultsOutlineView && (_countOfExcludedMatches || _countOfReadOnlyMatches && _countOfReadOnlyMatches != _countOfMatches) ? @"Replace Selected" : @"Replace All"; }

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
		[self folderSearchDidFinish:nil];
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

	FFSearchTarget searchTarget = controller.searchTarget;
	if(searchTarget != FFSearchTargetSelection && (searchTarget != FFSearchTargetDocument || action == FindActionFindAll && self.documentIdentifier))
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				if(searchTarget == FFSearchTargetDocument && self.documentIdentifier)
				{
					if(OakDocument* document = [OakDocumentController.sharedInstance findDocumentWithIdentifier:self.documentIdentifier])
					{
						self.documentSearch = nil;
						self.windowController.showsResultsOutlineView              = YES;
						self.windowController.resultsViewController.hideCheckBoxes = YES;
						[self acceptMatches:[document matchesForString:controller.findString options:_findOptions]];
						[self folderSearchDidFinish:nil];
					}
				}
				else if(searchTarget == FFSearchTargetOpenFiles)
				{
					self.documentSearch = nil;
					self.windowController.showsResultsOutlineView              = YES;
					self.windowController.resultsViewController.hideCheckBoxes = NO;
					for(OakDocument* document in [OakDocumentController.sharedInstance openDocuments])
						[self acceptMatches:[document matchesForString:controller.findString options:_findOptions]];
					[self folderSearchDidFinish:nil];
				}
				else
				{
					NSArray* paths;
					if(searchTarget == FFSearchTargetProject)
						paths = @[ self.projectFolder ];
					else if(searchTarget == FFSearchTargetFileBrowserItems)
						paths = self.fileBrowserItems;
					else // searchTarget == FFSearchTargetOther
						paths = @[ _windowController.otherFolder ];

					FFDocumentSearch* folderSearch = [FFDocumentSearch new];
					folderSearch.searchBinaryFiles   = YES;
					folderSearch.searchString        = controller.findString;
					folderSearch.options             = _findOptions;
					folderSearch.paths               = paths;
					folderSearch.glob                = controller.globString;
					folderSearch.searchFolderLinks   = controller.searchFolderLinks;
					folderSearch.searchFileLinks     = controller.searchFileLinks;
					folderSearch.searchHiddenFolders = controller.searchHiddenFolders;
					folderSearch.searchBinaryFiles   = controller.searchBinaryFiles;

					self.documentSearch = folderSearch;
				}
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

					if(OakDocument* doc = parent.document)
					{
						if(doc.isLoaded)
						{
							[doc performReplacements:replacements checksum:parent.match.checksum];
						}
						else
						{
							if(![doc performReplacements:replacements checksum:parent.match.checksum])
							{
								[parent.children setValue:nil forKey:@"replaceString"];
								continue;
							}

							[doc saveModalForWindow:self.windowController.window completionHandler:^(OakDocumentIOResult result, NSString* errorMessage, oak::uuid_t const& filterUUID){
								// TODO Indicate failure when result != OakDocumentIOResultSuccess
								if(!doc.isLoaded) // Ensure document is still closed
									doc.content = nil;
							}];
						}

						parent.readOnly = YES;
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
		bool onlySelection = searchTarget == FFSearchTargetSelection;
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

- (void)setSearchTarget:(FFSearchTarget)newTarget { self.windowController.searchTarget = newTarget; }
- (FFSearchTarget)searchTarget                    { return self.windowController.searchTarget; }
- (void)setProjectFolder:(NSString*)folder        { self.windowController.projectFolder = folder; }
- (NSString*)projectFolder                        { return self.windowController.projectFolder; }
- (void)setFileBrowserItems:(NSArray*)items       { self.windowController.fileBrowserItems = items; }
- (NSArray*)fileBrowserItems                      { return self.windowController.fileBrowserItems; }
- (NSString*)searchFolder                         { return self.windowController.searchFolder; }
- (BOOL)isVisible                                 { return self.windowController.window.isVisible; }

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
			[parent.document removeAllMarksOfType:kSearchMarkIdentifier];

		[self unbind:@"countOfMatches"];
		[self unbind:@"countOfExcludedMatches"];
		[self unbind:@"countOfReadOnlyMatches"];
		[self unbind:@"countOfExcludedReadOnlyMatches"];

		// Update UI dependent on “count of matches”
		self.countOfMatches = self.countOfExcludedMatches = self.countOfReadOnlyMatches = self.countOfExcludedReadOnlyMatches = 0;
	}

	_windowController.resultsViewController.results = _results = [FFResultNode new];
}

- (void)setDocumentSearch:(FFDocumentSearch*)newSearcher
{
	[self clearMatches];

	if(_documentSearch)
	{
		[_documentSearch removeObserver:self forKeyPath:@"currentPath"];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch stop];
	}

	if(_documentSearch = newSearcher)
	{
		self.windowController.busy                    = YES;
		self.windowController.statusString            = MSG_SEARCHING_FMT;
		self.windowController.showsResultsOutlineView = YES;
		self.windowController.resultsViewController.hideCheckBoxes = NO;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		self.performingFolderSearch = YES;
		[_documentSearch start];
	}
}

- (void)acceptMatches:(NSArray<OakDocumentMatch*>*)matches
{
	NSUInteger countOfExistingItems = _results.children.count;

	FFResultNode* parent = nil;
	for(OakDocumentMatch* match in matches)
	{
		[match.document setMarkOfType:kSearchMarkIdentifier atPosition:match.range.from content:nil];

		FFResultNode* node = [FFResultNode resultNodeWithMatch:match];
		if(!parent || ![parent.document isEqual:node.document])
			[_results addResultNode:(parent = [FFResultNode resultNodeWithMatch:match baseDirectory:CommonAncestor(_documentSearch.paths)])];
		[parent addResultNode:node];
	}

	[_windowController.resultsViewController insertItemsAtIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(countOfExistingItems, _results.children.count - countOfExistingItems)]];
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	[self acceptMatches:[aNotification userInfo][@"matches"]];
}

- (void)addResultsToPasteboard:(id)sender
{
	NSMutableArray* documents = [NSMutableArray array];
	for(FFResultNode* parent in _results.children)
	{
		[documents addObject:@{
			@"identifier"      : parent.firstResultNode.document.identifier.UUIDString,
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
	if(!_results)
		return;

	[self bind:@"countOfMatches" toObject:_results withKeyPath:@"countOfLeafs" options:nil];
	[self bind:@"countOfExcludedMatches" toObject:_results withKeyPath:@"countOfExcluded" options:nil];
	[self bind:@"countOfReadOnlyMatches" toObject:_results withKeyPath:@"countOfReadOnly" options:nil];
	[self bind:@"countOfExcludedReadOnlyMatches" toObject:_results withKeyPath:@"countOfExcludedReadOnly" options:nil];

	[self addResultsToPasteboard:self];

	NSString* fmt = MSG_ZERO_MATCHES_FMT;
	switch(self.countOfMatches)
	{
		case 0:  fmt = MSG_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_ONE_MATCH_FMT;        break;
		default: fmt = MSG_MULTIPLE_MATCHES_FMT; break;
	}

	NSString* searchString = [_documentSearch searchString] ?: self.windowController.findString;
	NSString* msg = [NSString stringWithFormat:fmt, searchString, [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
	if(_documentSearch)
	{
		NSNumberFormatter* formatter = [NSNumberFormatter new];
		formatter.numberStyle = NSNumberFormatterDecimalStyle;
		formatter.maximumFractionDigits = 1;
		NSString* seconds = [formatter stringFromNumber:@([_documentSearch searchDuration])];

		self.windowController.statusString          = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE), seconds, [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle]];
		self.windowController.alternateStatusString = [msg stringByAppendingFormat:MSG_SEARCHED_BYTES, seconds, [NSString stringWithCxxString:text::format_size([_documentSearch scannedByteCount])]];
	}
	else
	{
		self.windowController.statusString = msg;
	}

	__weak __block id observerId = [[NSNotificationCenter defaultCenter] addObserverForName:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSFindPboard] queue:nil usingBlock:^(NSNotification*){
		for(FFResultNode* parent in _results.children)
			[parent.document removeAllMarksOfType:kSearchMarkIdentifier];
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
	OakDocument* doc = item.document;
	if(!doc.isOpen)
		doc.recentTrackingDisabled = YES;
	[OakDocumentController.sharedInstance showDocument:doc andSelect:item.match.range inProject:self.projectIdentifier bringToFront:NO];
}

- (void)didDoubleClickResult:(FFResultNode*)item
{
	if([[[NSUserDefaults standardUserDefaults] objectForKey:kUserDefaultsKeepSearchResultsOnDoubleClick] boolValue])
		return;
	[self.windowController close];
}

- (void)didRemoveResult:(FFResultNode*)item
{
	if(OakIsAlternateKeyOrMouseEvent())
	{
		if(item.document.path)
		{
			std::string path = path::relative_to(to_s(item.document.path), to_s(CommonAncestor(_documentSearch.paths)));
			NSString* newGlob = [_windowController.globString stringByAppendingFormat:@"~%@", [NSString stringWithCxxString:path]];
			_windowController.globString = newGlob;
		}
	}

	[item.document removeAllMarksOfType:kSearchMarkIdentifier];
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

// =======================
// = Select Tab… Submenu =
// =======================

- (IBAction)takeSelectedPathFrom:(id)sender
{
	FFResultNode* item = [sender representedObject];
	if([item isKindOfClass:[FFResultNode class]])
		[_windowController.resultsViewController showResultNode:item.firstResultNode];
}

- (void)updateSelectTabMenu:(NSMenu*)aMenu
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
			if(OakDocument* doc = parent.document)
			{
				NSMenuItem* item = [aMenu addItemWithTitle:(doc.path ? to_ns(path::relative_to(to_s(doc.path), to_s(self.searchFolder))) : doc.displayName) action:@selector(takeSelectedPathFrom:) keyEquivalent:key < 9 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
				if(aMenu.propertiesToUpdate & NSMenuPropertyItemImage)
					[item setImage:parent.document.icon];
				[item setRepresentedObject:parent];
			}
		}
	}
}

// =====================
// = Copy Find Results =
// =====================

- (void)copyReplacements:(id)sender
{
	NSMutableArray* array = [NSMutableArray array];

	std::string const replacementString = to_s(_windowController.replaceString);
	for(FFResultNode* item in _windowController.resultsViewController.selectedResults)
	{
		auto const& captures = item.match.captures;
		[array addObject:captures.empty() ? _windowController.replaceString : to_ns(format_string::expand(replacementString, captures))];
	}

	[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
	[[NSPasteboard generalPasteboard] setString:[array componentsJoinedByString:@"\n"] forType:NSStringPboardType];
}

- (void)copyEntireLines:(BOOL)entireLines withFilename:(BOOL)withFilename
{
	std::vector<std::string> res;

	for(FFResultNode* item in _windowController.resultsViewController.selectedResults)
	{
		OakDocumentMatch* m = item.match;
		std::string str = to_s(m.excerpt);

		if(!entireLines)
			str = str.substr(m.first - m.excerptOffset, m.last - m.first);
		else if(str.size() && str.back() == '\n')
			str.erase(str.size()-1);

		if(withFilename)
			str = text::format("%s:%lu\t", [item.path UTF8String], m.lineNumber + 1) + str;

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
	static std::set<SEL> const copyActions = { @selector(copy:), @selector(copyReplacements:), @selector(copyMatchingParts:), @selector(copyMatchingPartsWithFilename:), @selector(copyEntireLines:), @selector(copyEntireLinesWithFilename:) };
	if(copyActions.find(aMenuItem.action) != copyActions.end())
		return [_results countOfLeafs] != 0;
	else if(aMenuItem.action == @selector(checkAll:))
		return _countOfExcludedMatches > _countOfExcludedReadOnlyMatches;
	else if(aMenuItem.action == @selector(uncheckAll:) )
		return _countOfExcludedMatches - _countOfExcludedReadOnlyMatches < _countOfMatches - _countOfReadOnlyMatches;
	return YES;
}
@end
