#import "Find.h"
#import "FFWindowController.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakPasteboard.h>
#import <ns/ns.h>
#import <text/types.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <editor/editor.h>
#import "Strings.h"

OAK_DEBUG_VAR(Find_Base);

@interface Find ()
{
	FFWindowController* windowController;

	NSString* projectIdentifier;
	NSString* documentIdentifier;

	std::map<std::string, find::folder_scan_settings_t> folderSettings;

	// OakFindProtocolServer
	find_operation_t findOperation;
	find::options_t  findOptions;
	BOOL closeWindowOnSuccess;
}
@property (nonatomic, retain) FFWindowController* windowController;
- (void)findClipboardDidChange:(NSNotification*)aNotification;
- (void)replaceClipboardDidChange:(NSNotification*)aNotification;
@end

static Find* SharedInstance;

NSString* const FFFindWasTriggeredByEnter = @"FFFindWasTriggeredByEnter";
NSString* const FolderOptionsDefaultsKey  = @"Folder Search Options";

@implementation Find
@synthesize projectIdentifier, documentIdentifier, windowController;
@synthesize findOperation, findOptions;

+ (Find*)sharedInstance
{
	return SharedInstance ?: [[Find new] autorelease];
}

+ (NSSet*)keyPathsForValuesAffectingFindFullWords        { return [NSSet setWithObject:@"findRegularExpression"]; }
+ (NSSet*)keyPathsForValuesAffectingFindIgnoreWhitespace { return [NSSet setWithObject:@"findRegularExpression"]; }

- (id)init
{
	if(SharedInstance)
	{
		[self release];
	}
	else if(self = SharedInstance = [[super init] retain])
	{
		D(DBF_Find_Base, bug("\n"););
		self.windowController = [[FFWindowController new] autorelease];
		[self.windowController setNextResponder:self];

		self.windowController.projectFolder = NSHomeDirectory();
		self.windowController.searchIn      = FFSearchInProjectFolder;

		if(NSDictionary* options = [[NSUserDefaults standardUserDefaults] objectForKey:FolderOptionsDefaultsKey])
		{
			self.windowController.followLinks         = [options objectForKey:@"followLinks"] != nil;
			self.windowController.searchHiddenFolders = [options objectForKey:@"searchHiddenFolders"] != nil;
		}

		// setup find/replace strings/options
		[self findClipboardDidChange:nil];
		[self replaceClipboardDidChange:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(findClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSFindPboard]];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(replaceClipboardDidChange:) name:OakPasteboardDidChangeNotification object:[OakPasteboard pasteboardWithName:NSReplacePboard]];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:nil];
	}
	return SharedInstance;
}

- (void)setWindowController:(FFWindowController*)controller
{
	if(controller != self.windowController)
	{
		if(windowController)
		{
			[windowController removeObserver:self forKeyPath:@"window"];
			[windowController removeObserver:self forKeyPath:@"searchIn"];
			[windowController release];
		}
		if(windowController = [controller retain])
		{
			self.windowController.delegate = self;
			[windowController addObserver:self forKeyPath:@"window" options:0 context:NULL];
			[windowController addObserver:self forKeyPath:@"searchIn" options:0 context:NULL];
		}
	}
}

- (void)applicationWillTerminate:(NSNotification*)notification
{
	NSMutableDictionary* options = [NSMutableDictionary dictionary];
	if(self.windowController.followLinks)
		[options setObject:@YES forKey:@"followLinks"];
	if(self.windowController.searchHiddenFolders)
		[options setObject:@YES forKey:@"searchHiddenFolders"];
	[[NSUserDefaults standardUserDefaults] setObject:options forKey:FolderOptionsDefaultsKey];

	// An autorelease pool is required here because the default pool will not be destroyed before termination,
	// so objects cleaned up here would not be properly destroyed, and would report as leaking.
	@autoreleasepool {
		[windowController invalidate];
		self.windowController = nil;
	}
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"window"] && object == self.windowController)
	{
		// Add notification observer for the find panel
		// We can’t do this at -init as the window is loaded lazily by the window controller
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowDidResignKey:) name:NSWindowDidResignKeyNotification object:self.windowController.window];
	}
	else if([keyPath isEqualToString:@"searchIn"])
	{
		if(self.windowController.isSearchingFolders)
		{
			// Attempt to restore folder-search settings
			std::string const& path = self.windowController.searchFolder.UTF8String;
			std::map<std::string, find::folder_scan_settings_t>::const_iterator const& it = folderSettings.find(path);
			if(it != folderSettings.end())
				self.windowController.followLinks = it->second.follow_links;
		}
	}
}

// ====================================
// = Actions for displaying the panel =
// ====================================

- (IBAction)showFindPanel:(id)sender
{
	[windowController showWindow:self];
}

- (IBAction)showFolderSelectionPanel:(id)sender
{
	[windowController showFolderSelectionPanel:sender];
}

// ================
// = Find actions =
// ================

- (void)conditionallyStoreFindSettingsOnPasteboard
{
	NSString* newString      = windowController.findString;
	NSString* newReplacement = windowController.replaceString;

	NSMutableDictionary* newOptions = [NSMutableDictionary dictionary];
	if(windowController.findIgnoreWhitespace)  [newOptions setObject:@YES forKey:OakFindIgnoreWhitespaceOption];
	if(windowController.findFullWords)         [newOptions setObject:@YES forKey:OakFindFullWordsOption];
	if(windowController.findRegularExpression) [newOptions setObject:@YES forKey:OakFindRegularExpressionOption];

	OakPasteboardEntry* oldEntry = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	if(!oldEntry || ![oldEntry.string isEqualToString:newString])
	{
		D(DBF_Find_Base, bug("new entry: %s → %s\n", [oldEntry.string UTF8String], [newString UTF8String]););
		[[OakPasteboard pasteboardWithName:NSFindPboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:newString andOptions:newOptions]];
	}
	else if(![oldEntry.options isEqualToDictionary:newOptions])
	{
		D(DBF_Find_Base, bug("update options:\n%s\n\t→\n%s\n", [[oldEntry.options description] UTF8String], [[newOptions description] UTF8String]););
		oldEntry.options = newOptions;
	}

	NSString* oldReplacement = [[[OakPasteboard pasteboardWithName:NSReplacePboard] current] string];
	if(!oldReplacement || ![oldReplacement isEqualToString:newReplacement])
	{
		D(DBF_Find_Base, bug("new replace string: %s → %s\n", [oldReplacement UTF8String], [newReplacement UTF8String]););
		[[OakPasteboard pasteboardWithName:NSReplacePboard] addEntry:[OakPasteboardEntry pasteboardEntryWithString:newReplacement]];
	}
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	FFDocumentSearch* obj = [aNotification object];
	NSMutableArray* documents = [NSMutableArray array];
	for(FFMatch* fileMatch in [obj allDocumentsWithMatches])
	{
		if(document::document_ptr doc = [fileMatch match].document)
		{
			NSArray* matches    = [obj allMatchesForDocumentIdentifier:[NSString stringWithCxxString:doc->identifier()]];
			FFMatch* firstMatch = [matches firstObject];
			FFMatch* lastMatch  = [matches lastObject];
			if(firstMatch && lastMatch)
			{
				[documents addObject:
					[NSDictionary dictionaryWithObjectsAndKeys:
						firstMatch.path,                                         @"path",
						[NSString stringWithCxxString:doc->identifier()],        @"identifier",
						[NSString stringWithCxxString:[firstMatch match].range], @"firstMatchRange",
						[NSString stringWithCxxString:[lastMatch match].range],  @"lastMatchRange",
					nil]];
			}
		}
	}
	[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : documents };
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FFWindowController*)controller
{
	if(controller.findRegularExpression)
	{
		std::string const& error = regexp::validate(to_s(controller.findString));
		if(error != NULL_STR)
		{
			controller.statusMessage = [NSString stringWithCxxString:text::format("Invalid regular expression: %s.", error.c_str())];
			return NSBeep();
		}
	}

	[self conditionallyStoreFindSettingsOnPasteboard];

	findOptions = (controller.findRegularExpression ? find::regular_expression : find::none) | (controller.findIgnoreWhitespace ? find::ignore_whitespace : find::none) | (controller.findFullWords ? find::full_words : find::none);
	findOptions |= [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] ? find::ignore_case        : find::none;
	findOptions |= [[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround] ? find::wrap_around        : find::none;

	if(action == FindActionFindPrevious)
		findOptions |= find::backwards;
	else if(action == FindActionCountMatches || action == FindActionFindAll || action == FindActionReplaceAll)
		findOptions |= find::all_matches;

	if(controller.isSearchingFolders || (self.documentIdentifier && action == FindActionFindAll) || action == FindActionReplaceSelected)
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				FFDocumentSearch* folderSearch = [[FFDocumentSearch new] autorelease];
				folderSearch.searchString      = controller.findString;
				folderSearch.options           = findOptions;
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
						auto const settings = settings_for_path(NULL_STR, "", to_s(controller.searchFolder));
						globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFolderSearchKey, NULL_STR), path::kPathItemDirectory);
						globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey,               NULL_STR), path::kPathItemDirectory);
						globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInFolderSearchKey,       NULL_STR), path::kPathItemFile);
						globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey,                     NULL_STR), path::kPathItemFile);
						for(auto key : { kSettingsExcludeInFolderSearchKey, kSettingsExcludeKey, kSettingsBinaryKey })
							globs.add_exclude_glob(settings.get(key, NULL_STR));
						globs.add_include_glob(controller.searchHiddenFolders ? "{,.}*" : "*", path::kPathItemDirectory);
						globs.add_include_glob(to_s(controller.globString), path::kPathItemFile);
					}

					find::folder_scan_settings_t search([controller.searchIn isEqualToString:FFSearchInOpenFiles] ? find::folder_scan_settings_t::open_files : to_s(controller.searchFolder), globs, controller.followLinks);
					[folderSearch setFolderOptions:search];
					folderSettings[controller.searchFolder.UTF8String] = search;
				}
				controller.searcher = folderSearch;
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);
				for(FFMatch* fileMatch in [controller.searcher allDocumentsWithSelectedMatches])
				{
					std::multimap<text::range_t, std::string> replacements;
					for(FFMatch* match in [controller.searcher allSelectedMatchesForDocumentIdentifier:[fileMatch identifier]])
					{
						++replaceCount;
						replacements.insert(std::make_pair([match match].range, controller.findRegularExpression ? format_string::expand(replaceString, [match match].captures) : replaceString));
					}

					if(document::document_ptr doc = [fileMatch match].document)
					{
						if(doc->is_open())
								ng::editor_for_document(doc)->perform_replacements(replacements);
						else	doc->replace(replacements);
					}

					++fileCount;
				}
				controller.searcher.hasPerformedReplacement = YES;
				windowController.statusMessage = [NSString stringWithFormat:MSG_REPLACE_ALL_RESULTS, replaceCount, fileCount];
			}
			break;

			case FindActionFindNext:     break; // TODO FindActionFindNext for folder searches
			case FindActionFindPrevious: break; // TODO FindActionFindPrevious for folder searches
		}
	}
	else
	{
		bool onlySelection = [controller.searchIn isEqualToString:FFSearchInSelection];
		switch(action)
		{
			case FindActionFindNext:
			case FindActionFindPrevious:
			case FindActionFindAll:       findOperation = onlySelection ? kFindOperationFindInSelection    : kFindOperationFind;    break;
			case FindActionCountMatches:  findOperation = onlySelection ? kFindOperationCountInSelection   : kFindOperationCount;   break;
			case FindActionReplaceAll:    findOperation = onlySelection ? kFindOperationReplaceInSelection : kFindOperationReplace; break;
		}

		closeWindowOnSuccess = action == FindActionFindNext && [[NSApp currentEvent] type] == NSKeyDown && to_s([NSApp currentEvent]) == utf8::to_s(NSCarriageReturnCharacter);
		[NSApp sendAction:@selector(performFindOperation:) to:nil from:self];
	}
}

- (NSString*)findString    { return windowController.findString;    }
- (NSString*)replaceString { return windowController.replaceString; }

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition
{
	static std::string const formatStrings[4][3] = {
		{ "No more occurrences of “${found}”.", "Found “${found}”${line:+ at line ${line}, column ${column}}.",               "${count} occurrences of “${found}”." },
		{ "No more matches for “${found}”.",    "Found one match for “${found}”${line:+ at line ${line}, column ${column}}.", "${count} matches for “${found}”."    },
	};

	format_string::string_map_t variables;
	variables["count"]  = text::format("%lu", aNumber);
	variables["found"]  = to_s(aFindString);
	variables["line"]   = aPosition ? text::format("%zu", aPosition.line + 1)   : NULL_STR;
	variables["column"] = aPosition ? text::format("%zu", aPosition.column + 1) : NULL_STR;
	windowController.statusMessage = [NSString stringWithCxxString:format_string::expand(formatStrings[(findOptions & find::regular_expression) ? 1 : 0][std::min<size_t>(aNumber, 2)], variables)];

	if(closeWindowOnSuccess && aNumber != 0)
		return [windowController close];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	windowController.statusMessage = [NSString stringWithFormat:format, aFindString, aNumber];
}

- (IBAction)findNext:(id)sender     { [self performFindAction:FindActionFindNext     withWindowController:self.windowController]; }
- (IBAction)findPrevious:(id)sender { [self performFindAction:FindActionFindPrevious withWindowController:self.windowController]; }

// =================
// = Notifications =
// =================

- (void)windowDidResignKey:(NSNotification*)notification
{
	[self conditionallyStoreFindSettingsOnPasteboard];
}

- (void)findClipboardDidChange:(NSNotification*)aNotification
{
	OakPasteboardEntry* entry              = [[OakPasteboard pasteboardWithName:NSFindPboard] current];
	windowController.findString            = entry.string ?: @"";
	windowController.findFullWords         = entry.fullWordMatch;
	windowController.findIgnoreWhitespace  = entry.ignoreWhitespace;
	windowController.findRegularExpression = entry.regularExpression;
}

- (void)replaceClipboardDidChange:(NSNotification*)aNotification
{
	windowController.replaceString = [[[OakPasteboard pasteboardWithName:NSReplacePboard] current] string] ?: @"";
}

// =============
// = Accessors =
// =============

- (NSString*)projectFolder
{
	return windowController.projectFolder;
}

- (void)setProjectFolder:(NSString*)folder
{
	windowController.projectFolder = folder;
}

- (NSString*)searchFolder
{
	return windowController.searchFolder;
}

- (void)setSearchFolder:(NSString*)folder
{
	windowController.searchIn = folder;
}

- (int)searchScope
{
	if([windowController.searchIn isEqualToString:FFSearchInSelection])
		return find::in::selection;
	else if([windowController.searchIn isEqualToString:FFSearchInDocument])
		return find::in::document;
	else if([windowController.searchIn isEqualToString:FFSearchInOpenFiles])
		return find::in::open_files;
	return find::in::folder;
}

- (void)setSearchScope:(int)newSearchScope
{
	switch(newSearchScope)
	{
		case find::in::selection:  windowController.searchIn = FFSearchInSelection;           break;
		case find::in::document:   windowController.searchIn = FFSearchInDocument;            break;
		case find::in::folder:     windowController.searchIn = windowController.searchFolder; break;
		case find::in::open_files: windowController.searchIn = FFSearchInOpenFiles;           break;
		default:
			ASSERTF(false, "Unknown search scope tag %d\n", newSearchScope);
	}
}

- (BOOL)isVisible
{
	return windowController.window.isVisible;
}

// ===========
// = Options =
// ===========

- (IBAction)takeFindOptionToToggleFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);

	find::options_t option = find::options_t([sender tag]);
	switch(option)
	{
		case find::ignore_whitespace:  windowController.findIgnoreWhitespace  = !windowController.findIgnoreWhitespace;  break;
		case find::ignore_case:        [[NSUserDefaults standardUserDefaults] setObject:([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindIgnoreCase] ? @NO : @YES) forKey:kUserDefaultsFindIgnoreCase]; break;
		case find::full_words:         windowController.findFullWords         = !windowController.findFullWords;         break;
		case find::wrap_around:        [[NSUserDefaults standardUserDefaults] setObject:([[NSUserDefaults standardUserDefaults] boolForKey:kUserDefaultsFindWrapAround] ? @NO : @YES) forKey:kUserDefaultsFindWrapAround]; break;
		case find::regular_expression: windowController.findRegularExpression = !windowController.findRegularExpression; break;
		default:
			ASSERTF(false, "Unknown find option tag %d\n", option);
	}

	if([[[[OakPasteboard pasteboardWithName:NSFindPboard] current] string] isEqualToString:windowController.findString])
		[self conditionallyStoreFindSettingsOnPasteboard]; // update the options on the pasteboard immediately if the find string has not been changed
}
@end
