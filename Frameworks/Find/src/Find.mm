#import "Find.h"
#import "FindWindowController.h"
#import "FFDocumentSearch.h"
#import "Strings.h"
#import "attr_string.h"
#import "FFFilePathCell.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakPasteboard.h>
#import <ns/ns.h>
#import <text/types.h>
#import <text/format.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <editor/editor.h>
#import <document/collection.h>

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

@interface FFResultNode : NSObject <NSCopying>
{
	document::document_t::callback_t* _callback;
}
@property (nonatomic, weak) FFResultNode* parent;
@property (nonatomic, weak) FFResultNode* next;
@property (nonatomic, weak) FFResultNode* previous;

@property (nonatomic) FFMatch* match;
@property (nonatomic) NSMutableArray* matches;
@property (nonatomic) BOOL exclude;
@property (nonatomic) BOOL replacementDone;
@property (nonatomic) NSImage* icon;

@property (nonatomic, readonly) document::document_ptr document;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic, readonly) NSString* identifier;
@end

@implementation FFResultNode
+ (FFResultNode*)resultNodeWithMatch:(FFMatch*)aMatch
{
	FFResultNode* res = [FFResultNode new];
	res.match = aMatch;
	return res;
}

- (void)dealloc
{
	if(_callback)
	{
		self.document->remove_callback(_callback);
		delete _callback;
	}
}

- (id)copyWithZone:(NSZone*)zone
{
	return self;
}

- (void)addResultNode:(FFResultNode*)aMatch
{
	if(!_matches)
		_matches = [NSMutableArray array];

	aMatch.previous      = self.lastResultNode;
	aMatch.previous.next = aMatch;
	aMatch.parent        = self;

	[_matches addObject:aMatch];
}

- (void)removeFromParent
{
	_next.previous = _previous;
	_previous.next = _next;
	[_parent.matches removeObject:self];
}

- (FFResultNode*)firstResultNode   { return [_matches firstObject]; }
- (FFResultNode*)lastResultNode    { return [_matches lastObject]; }
- (document::document_ptr)document { return [_match match].document; }
- (NSString*)path                  { return [NSString stringWithCxxString:self.document->path()]; }
- (NSString*)identifier            { return [NSString stringWithCxxString:self.document->identifier()]; }

- (NSUInteger)lineSpan
{
	text::pos_t const from = [_match match].range.from;
	text::pos_t const to   = [_match match].range.to;
	return to.line - from.line + (from == to || to.column != 0 ? 1 : 0);
}

- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString
{
	find::match_t const& m = [_match match];

	size_t from = m.first - m.excerpt_offset;
	size_t to   = m.last  - m.excerpt_offset;

	std::string prefix = m.excerpt.substr(0, from);
	std::string middle = m.excerpt.substr(from, to - from);
	std::string suffix = m.excerpt.substr(to);

	if(replacementString)
		middle = m.captures.empty() ? to_s(replacementString) : format_string::expand(to_s(replacementString), m.captures);

	if(!utf8::is_valid(prefix.begin(), prefix.end()) && utf8::is_valid(middle.begin(), middle.end()) && utf8::is_valid(suffix.begin(), suffix.end()))
	{
		ns::attr_string_t res;
		res = ns::attr_string_t([NSColor darkGrayColor])
		    << ns::style::line_break(NSLineBreakByTruncatingTail)
		    << text::format("%zu-%zu: Range is not valid UTF-8, please contact: http://macromates.com/support", m.first, m.last);
		return res.get();
	}

	return AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.line_number);
}

- (NSImage*)icon
{
	struct document_callback_t : document::document_t::callback_t
	{
		WATCH_LEAKS(document_callback_t);
		document_callback_t (FFResultNode* self) : _self(self) {}
		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(event == did_change_modified_status)
				_self.icon = nil;
		}

	private:
		__weak FFResultNode* _self;
	};

	if(!_icon)
		_icon = [OakFileIconImage fileIconImageWithPath:self.path isModified:self.document->is_modified()];
	if(!_callback)
		self.document->add_callback(_callback = new document_callback_t(self));

	return _icon;
}
@end

@interface Find () <NSOutlineViewDataSource, NSOutlineViewDelegate>
@property (nonatomic) FindWindowController* windowController;
@property (nonatomic) FFDocumentSearch* documentSearch;
@property (nonatomic) FFResultNode* results;
@property (nonatomic) NSUInteger countOfMatches;
@property (nonatomic) NSUInteger countOfExcludedMatches;
@property (nonatomic) BOOL closeWindowOnSuccess;
@property (nonatomic) BOOL performingFolderSearch;
@property (nonatomic) BOOL performedReplaceAll;
@property (nonatomic) BOOL performedSaveAll;
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
		_windowController.resultsOutlineView.action       = @selector(didSingleClickResultsOutlineView:);
		_windowController.resultsOutlineView.doubleAction = @selector(didDoubleClickResultsOutlineView:);
		_windowController.resultsOutlineView.target       = self;
		_windowController.resultsOutlineView.dataSource   = self;
		_windowController.resultsOutlineView.delegate     = self;

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
	[self updateActionButtons:self];
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

- (void)updateActionButtons:(id)sender
{
	NSString* replaceAllTitle = @"Replace All";
	BOOL replaceAllEnabled = !self.windowController.showsResultsOutlineView || !self.performedReplaceAll;

	if(self.windowController.showsResultsOutlineView)
	{
		replaceAllEnabled = self.countOfExcludedMatches < self.countOfMatches;
		if(replaceAllEnabled && self.countOfExcludedMatches)
			replaceAllTitle = @"Replace Selected";
	}

	self.windowController.replaceAllButton.title   = replaceAllTitle;
	self.windowController.replaceAllButton.enabled = replaceAllEnabled;

	[self.windowController setDocumentEdited:self.performedReplaceAll != self.performedSaveAll];
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
	if(_performingFolderSearch)
	{
		[_documentSearch stop];
		self.windowController.statusString = @"Stopped.";
	}
}

// These are disabled via menu validation
- (IBAction)saveDocument:(id)sender   { }
- (IBAction)saveDocumentAs:(id)sender { }

- (IBAction)saveAllDocuments:(id)sender
{
	NSUInteger fileCount = 0;
	std::vector<document::document_ptr> failedDocs;

	for(FFResultNode* parent in _results.matches)
	{
		for(FFResultNode* child in parent.matches)
		{
			if(!child.replacementDone)
				continue;

			if(document::document_ptr doc = child.document)
			{
				if(doc->sync_save(kCFRunLoopDefaultMode))
						++fileCount;
				else	failedDocs.push_back(doc);
			}
			break;
		}
	}

	if(failedDocs.empty())
	{
		self.windowController.statusString = [NSString stringWithFormat:@"%lu file%s saved.", fileCount, fileCount == 1 ? "" : "s"];
		self.performedSaveAll = YES;
		[self updateActionButtons:self];
	}
	else
	{
		NSBeep();
		self.windowController.statusString = [NSString stringWithFormat:@"%zu file%s failed to save.", failedDocs.size(), failedDocs.size() == 1 ? "" : "s"];
	}
}

- (void)showSaveWarningWithCompletionHandler:(void(^)())callback
{
	NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Do you want to save the changes from your replace operation?" informativeText:@"Your changes will be lost if you don’t save them." buttons:@"Save All", @"Cancel", @"Don’t Save", nil];
	OakShowAlertForWindow(alert, _windowController.window, ^(NSInteger returnCode){
		if(returnCode == NSAlertFirstButtonReturn) // Save All
		{
			[self saveAllDocuments:self];
		}
		else if(returnCode == NSAlertThirdButtonReturn) // Discard
		{
			// FIXME Undo replacements
			self.performedReplaceAll = NO;
			[self updateActionButtons:self];
		}
		else if(returnCode == NSAlertSecondButtonReturn) // Cancel
			;

		if(self.performedReplaceAll == self.performedSaveAll)
			callback();
	});
}

- (BOOL)windowShouldClose:(id)sender
{
	if(self.performedReplaceAll == self.performedSaveAll)
		return YES;

	[self showSaveWarningWithCompletionHandler:^{ [_windowController close]; }];
	return NO;
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FindWindowController*)controller
{
	[controller updateFindErrorString];
	if(controller.findErrorString != nil)
		return;

	if(self.performedReplaceAll != self.performedSaveAll)
		return [self showSaveWarningWithCompletionHandler:^{ [self performFindAction:action withWindowController:controller]; }];

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

					folderSearch.directory   = folder;
					folderSearch.globList    = globs;
					folderSearch.followLinks = controller.followSymbolicLinks;
				}

				self.documentSearch = folderSearch;
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);

				for(FFResultNode* parent in _results.matches)
				{
					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					for(FFResultNode* child in parent.matches)
					{
						if(child.exclude || child.replacementDone)
							continue;
						child.replacementDone = YES;
						_countOfExcludedMatches += 1;
						replacements.emplace(std::make_pair([child.match match].first, [child.match match].last), controller.regularExpression ? format_string::expand(replaceString, [child.match match].captures) : replaceString);
					}

					if(replacements.empty())
						continue;

					if(document::document_ptr doc = parent.document)
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

						replaceCount += replacements.size();
						++fileCount;
					}
				}
				self.windowController.statusString = [NSString stringWithFormat:MSG_REPLACE_ALL_RESULTS, replaceCount, fileCount];
				self.performedReplaceAll = YES;
				[self updateActionButtons:self];
				[self.windowController.resultsOutlineView reloadData];
			}
			break;

			case FindActionFindNext:     [self selectNextResult:self];     break;
			case FindActionFindPrevious: [self selectPreviousResult:self]; break;
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
	for(FFResultNode* parent in _results.matches)
	{
		if(document::document_ptr doc = parent.document)
			doc->remove_all_marks("search");
	}

	_results                = [FFResultNode new];
	_countOfMatches         = 0;
	_countOfExcludedMatches = 0;

	[_windowController.resultsOutlineView reloadData];
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
		self.performedReplaceAll = NO;
		self.performedSaveAll    = NO;

		self.windowController.busy                     = YES;
		self.windowController.statusString             = MSG_SEARCHING_FMT;
		self.windowController.showsResultsOutlineView  = YES;
		self.windowController.disableResultsCheckBoxes = _documentSearch.documentIdentifier != nil;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		self.performingFolderSearch = YES;
		[_documentSearch start];
	}

	[self updateActionButtons:self];
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	NSArray* matches = [aNotification userInfo][@"matches"];
	FFResultNode* parent = nil;
	for(FFMatch* match in matches)
	{
		if(document::document_ptr doc = [match match].document)
			doc->add_mark([match match].range, "search");

		FFResultNode* node = [FFResultNode resultNodeWithMatch:match];
		if(!parent || parent.document->identifier() != node.document->identifier())
			[_results addResultNode:(parent = [FFResultNode resultNodeWithMatch:match])];
		[parent addResultNode:node];
	}
	_countOfMatches += [matches count];

	NSInteger first = [self.windowController.resultsOutlineView numberOfRows];
	[self.windowController.resultsOutlineView reloadData];
	NSInteger last = [self.windowController.resultsOutlineView numberOfRows];
	while(last-- != first)
		[self.windowController.resultsOutlineView expandItem:[self.windowController.resultsOutlineView itemAtRow:last]];
	[self.windowController.resultsOutlineView sizeLastColumnToFit];

	[self updateActionButtons:self];
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	self.performingFolderSearch = NO;
	self.windowController.busy = NO;
	if(!_documentSearch)
		return;

	NSMutableArray* documents = [NSMutableArray array];
	for(FFResultNode* parent in _results.matches)
	{
		[documents addObject:@{
			@"identifier"      : [NSString stringWithCxxString:parent.firstResultNode.document->identifier()],
			@"firstMatchRange" : [NSString stringWithCxxString:[parent.firstResultNode.match match].range],
			@"lastMatchRange"  : [NSString stringWithCxxString:[parent.lastResultNode.match match].range],
		}];
	}
	[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : documents };

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
		self.windowController.statusString          = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE), [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle], [_documentSearch searchDuration]];
		self.windowController.alternateStatusString = [msg stringByAppendingFormat:MSG_SEARCHED_BYTES, [NSString stringWithCxxString:text::format_size([_documentSearch scannedByteCount])], [_documentSearch searchDuration]];
	}
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

// ============================
// = Outline view data source =
// ============================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(FFResultNode*)item
{
	return [(item ?: _results).matches count];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item];
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(FFResultNode*)item
{
	return [(item ?: _results).matches objectAtIndex:childIndex];
}

- (id)outlineView:(NSOutlineView*)outlineView objectValueForTableColumn:(NSTableColumn*)tableColumn byItem:(FFResultNode*)item
{
	if([self outlineView:outlineView isGroupItem:item])
		return item;
	else if([[tableColumn identifier] isEqualToString:@"checkbox"])
		return @(!item.exclude);
	else if([[tableColumn identifier] isEqualToString:@"match"])
		return [item excerptWithReplacement:(item.replacementDone || !item.exclude && self.windowController.showReplacementPreviews) ? self.replaceString : nil];
	return nil;
}

- (void)outlineView:(NSOutlineView*)outlineView setObjectValue:(id)objectValue forTableColumn:(NSTableColumn*)tableColumn byItem:(FFResultNode*)item
{
	if(![tableColumn.identifier isEqualToString:@"checkbox"])
		return;

	BOOL exclude = ![objectValue boolValue];

	NSArray* items = OakIsAlternateKeyOrMouseEvent() ? item.parent.matches : @[ item ];
	for(FFResultNode* node in items)
	{
		if(node.exclude != exclude)
			_countOfExcludedMatches += (node.exclude = exclude) ? +1 : -1;
	}

	[self updateActionButtons:self];
	if(OakIsAlternateKeyOrMouseEvent())
		[outlineView reloadData];
}

- (void)outlineView:(NSOutlineView*)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
{
	if([self outlineView:outlineView isGroupItem:item])
	{
		if(!tableColumn && [cell isKindOfClass:[FFFilePathCell class]])
		{
			FFFilePathCell* pathCell = (FFFilePathCell*)cell;
			pathCell.icon = [item icon];
			pathCell.path = [item path] ?: [NSString stringWithCxxString:item.document->display_name()];
			pathCell.base = self.searchFolder ?: self.projectFolder;
			pathCell.count = [outlineView isItemExpanded:item] ? 0 : [item.matches count];
		}
	}
	else if([[tableColumn identifier] isEqualToString:@"checkbox"])
	{
		[cell setEnabled:!self.performedReplaceAll];
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

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(FFResultNode*)item
{
	return [outlineView levelForItem:item] == 0;
}

- (CGFloat)outlineView:(NSOutlineView*)outlineView heightOfRowByItem:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item] ? 22 : item.lineSpan * [outlineView rowHeight];
}

- (NSCell*)outlineView:(NSOutlineView*)outlineView dataCellForTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
{
	if(tableColumn == nil && [self outlineView:outlineView isGroupItem:item])
		return [FFFilePathCell new];
	return [tableColumn dataCell];
}

// ======================
// = Should Select Item =
// ======================

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(FFResultNode*)item
{
	if([self outlineView:outlineView isGroupItem:item])
		return NO;

	NSInteger clickedColumn = [outlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[outlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return NO;

	return YES;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldTrackCell:(NSCell*)cell forTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
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
		NSUInteger selectionIndex  = [[outlineView selectedRowIndexes] firstIndex];
		FFResultNode* selectedNode = [outlineView itemAtRow:selectionIndex];
		auto doc = selectedNode.document;
		if(!doc->is_open())
			doc->set_recent_tracking(false);
		document::show(doc, self.projectIdentifier ? oak::uuid_t(to_s(self.projectIdentifier)) : document::kCollectionAny, [selectedNode.match match].range, false);
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

// ===========================
// = Expand/Collapse Results =
// ===========================

- (BOOL)resultsCollapsed
{
	NSUInteger expanded = 0;
	for(FFResultNode* parent in _results.matches)
		expanded += [_windowController.resultsOutlineView isItemExpanded:parent] ? 1 : 0;
	return [_results.matches count] && 2 * expanded <= [_results.matches count];
}

- (IBAction)takeLevelToFoldFrom:(id)sender
{
	if(self.resultsCollapsed)
			[_windowController.resultsOutlineView expandItem:nil expandChildren:YES];
	else	[_windowController.resultsOutlineView collapseItem:nil collapseChildren:YES];

	[_windowController.resultsOutlineView setNeedsDisplay:YES];
}

// ==================
// = Go to… Submenu =
// ==================

- (void)showResultNode:(FFResultNode*)aResultNode
{
	if(!aResultNode)
		return;

	NSOutlineView* outlineView = _windowController.resultsOutlineView;
	if(![outlineView isItemExpanded:aResultNode.parent])
		[outlineView expandItem:aResultNode.parent];
	[outlineView scrollRowToVisible:[outlineView rowForItem:aResultNode.parent]];

	NSInteger row = [outlineView rowForItem:aResultNode];
	if(row != -1)
	{
		[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[outlineView scrollRowToVisible:row];
		[outlineView.window makeFirstResponder:outlineView];
	}
}

- (IBAction)takeSelectedPathFrom:(id)sender
{
	FFResultNode* item = [sender representedObject];
	if([item isKindOfClass:[FFResultNode class]])
		[self showResultNode:item.firstResultNode];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(_results.matches.count == 0)
	{
		[[aMenu addItemWithTitle:@"No Results" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		char key = 0;
		for(FFResultNode* parent in _results.matches)
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

	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	NSIndexSet* selectedRows = [outlineView numberOfSelectedRows] == 0 ? [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, [outlineView numberOfRows])] : [outlineView selectedRowIndexes];
	for(NSUInteger index = [selectedRows firstIndex]; index != NSNotFound; index = [selectedRows indexGreaterThanIndex:index])
	{
		FFResultNode* item = [outlineView itemAtRow:index];
		if([item.matches count])
			continue;

		find::match_t const& m = [item.match match];
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
	for(FFResultNode* child = _results.firstResultNode.firstResultNode; child; child = child.next ?: child.parent.next.firstResultNode)
	{
		if(child.exclude != exclude)
			_countOfExcludedMatches += (child.exclude = exclude) ? +1 : -1;
	}
	[_windowController.resultsOutlineView reloadData];
	[self updateActionButtons:self];
}

- (IBAction)checkAll:(id)sender
{
	[self allMatchesSetExclude:NO];
}

- (IBAction)uncheckAll:(id)sender
{
	[self allMatchesSetExclude:YES];
}

- (IBAction)selectNextResult:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];

	item = item ? (item.next ?: item.parent.next.firstResultNode) : _results.firstResultNode.firstResultNode;
	if(!item && _windowController.wrapAround)
		item = _results.firstResultNode.firstResultNode;

	[self showResultNode:item];
}

- (IBAction)selectPreviousResult:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];

	item = item ? (item.previous ?: item.parent.previous.lastResultNode) : _results.lastResultNode.lastResultNode;
	if(!item && _windowController.wrapAround)
		item = _results.lastResultNode.lastResultNode;

	[self showResultNode:item];
}

- (IBAction)selectNextTab:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];
	[self showResultNode:item.parent.next.firstResultNode ?: _results.firstResultNode.firstResultNode];
}

- (IBAction)selectPreviousTab:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];
	[self showResultNode:item.parent.previous.firstResultNode ?: _results.lastResultNode.firstResultNode];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	static std::set<SEL> const copyActions = { @selector(copy:), @selector(copyMatchingParts:), @selector(copyMatchingPartsWithFilename:), @selector(copyEntireLines:), @selector(copyEntireLinesWithFilename:) };
	if(copyActions.find(aMenuItem.action) != copyActions.end())
		return [self.windowController.resultsOutlineView numberOfRows] != 0;
	else if(aMenuItem.action == @selector(saveAllDocuments:))
		return self.performedReplaceAll && !self.performedSaveAll;
	else if(aMenuItem.action == @selector(saveDocument:) || aMenuItem.action == @selector(saveDocumentAs:))
		return NO;
	else if(aMenuItem.action == @selector(takeLevelToFoldFrom:) && aMenuItem.tag == -1)
		[aMenuItem setTitle:self.resultsCollapsed ? @"Expand Results" : @"Collapse Results"];
	else if(aMenuItem.action == @selector(checkAll:) || aMenuItem.action == @selector(uncheckAll:) )
		return [_results.matches count];
	return YES;
}
@end
