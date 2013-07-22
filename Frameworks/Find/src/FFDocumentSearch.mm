#import "FFDocumentSearch.h"
#import "FindWindowController.h"
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/NSImage Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakTimer.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <scope/scope.h>
#import <oak/oak.h>

NSString* const FFDocumentSearchDidReceiveResultsNotification = @"FFDocumentSearchDidReceiveResultsNotification";
NSString* const FFDocumentSearchDidFinishNotification         = @"FFDocumentSearchDidFinishNotification";

static std::string range_from_document (document::document_ptr const& document, off_t from, off_t to)
{
	std::string res = "";
	if(document::document_t::reader_ptr reader = document->create_reader())
	{
		off_t pos = 0;
		while(io::bytes_ptr const& data = reader->next())
		{
			off_t len = data->size();
			if(from < pos + len)
			{
				off_t skip     = std::max(pos, from)     - pos;
				off_t limit    = std::min(pos + len, to) - pos;
				char const* buf = data->get();
				std::copy(buf + skip, buf + limit, back_inserter(res));

				if(to <= pos + len)
					break;
			}
			pos += len;
		}
	}
	ASSERTF(res.size() == to - from, "Wrong result size (%zu) for %s (%llu-%llu)", res.size(), document->display_name().c_str(), from, to);
	return res;
}

@interface FFMatch ()
{
	OBJC_WATCH_LEAKS(FFMatch);

	std::string matchText;
	find::match_t match;
	document::document_t::callback_t* callback;
	NSImage* icon;
}
- (void)updateIcon;
@property (nonatomic, retain, readwrite) NSImage* icon;
@end

struct document_callback_t : document::document_t::callback_t
{
	WATCH_LEAKS(document_callback_t);
	document_callback_t (FFMatch* self) : self(self) {}
	void handle_document_event (document::document_ptr document, event_t event)
	{
		if(event != did_change_modified_status)
			return;
		[self updateIcon];
	}
private:
	FFMatch* self;
};

@implementation FFMatch
@synthesize icon;

- (id)initWithMatch:(find::match_t const&)aMatch;
{
	if(self = [self init])
	{
		match = aMatch;
	}
	return self;
}

- (id)initWithDocument:(document::document_ptr)aDocument
{
	return [self initWithMatch:find::match_t(aDocument)];
}

- (id)copyWithZone:(NSZone*)zone
{
	return self;
}

- (void)dealloc
{
	if(callback)
	{
		match.document->remove_callback(callback);
		delete callback;
		callback = NULL;
	}
}

- (find::match_t const&)match
{
	return match;
}

- (NSImage*)icon
{
	if(!icon)
		self.icon = [OakFileIconImage fileIconImageWithPath:[NSString stringWithCxxString:match.document->path()] isModified:match.document->is_modified()];
	if(!callback)
		match.document->add_callback(callback = new document_callback_t(self));
	return icon;
}

- (void)updateIcon
{
	self.icon = nil;
}

- (NSString*)path
{
	return [NSString stringWithCxxString:match.document->path()];
}

- (NSString*)identifier
{
	return [NSString stringWithCxxString:match.document->identifier()];
}

- (std::string const&)matchText
{
	if(matchText.empty())
		matchText = range_from_document(match.document, match.bol_offset, match.eol_offset);
	return matchText;
}
@end

@interface FFDocumentSearch ()
{
	OBJC_WATCH_LEAKS(FFDocumentSearch);

	std::string searchString;
	find::options_t options;
	find::folder_scan_settings_t folderOptions;
	NSString* projectIdentifier;
	NSString* documentIdentifier;

	NSMutableArray* matchingDocuments; // FFMatches in order of searching, containing document
	NSMutableDictionary* matchInfo;    // Document identifier → array of FFMatch instances
	NSMutableSet* replacementMatchesToSkip;

	BOOL hasPerformedReplacement;
	BOOL hasPerformedSave;

	scan_path_ptr scanner;
	OakTimer* scannerProbeTimer;
	oak::duration_t timer;

	NSString* currentPath;
}
@property (nonatomic, retain) OakTimer* scannerProbeTimer;
@property (nonatomic, retain, readwrite) NSString* currentPath;
@end

OAK_DEBUG_VAR(Find_FolderSearch);

@implementation FFDocumentSearch
// ==============
// = Public API =
// ==============

@synthesize options, scannerProbeTimer, currentPath, projectIdentifier, documentIdentifier;
@synthesize hasPerformedReplacement, hasPerformedSave;

- (NSArray*)allDocumentsWithMatches
{
	return matchingDocuments;
}

- (NSArray*)allDocumentsWithSelectedMatches
{
	NSMutableArray* documents = [NSMutableArray array];
	for(FFMatch* document in matchingDocuments)
	{
		if([[self allSelectedMatchesForDocumentIdentifier:document.identifier] count] > 0)
			[documents addObject:document];
	}
	return documents;
}

- (NSArray*)allSelectedMatchesForDocumentIdentifier:(NSString*)identifier
{
	NSMutableArray* matches = [NSMutableArray array];
	for(FFMatch* match in [self allMatchesForDocumentIdentifier:identifier])
	{
		if(![self skipReplacementForMatch:match])
			[matches addObject:match];
	}
	return matches;
}

- (NSString*)searchString
{
	return [NSString stringWithCxxString:searchString];
}

- (void)setSearchString:(NSString*)string
{
	searchString = [string UTF8String];
}

- (find::folder_scan_settings_t const&)folderOptions
{
	return folderOptions;
}

- (void)setFolderOptions:(find::folder_scan_settings_t const&)newFolderOptions
{
	folderOptions = newFolderOptions;
}

- (void)start
{
	D(DBF_Find_FolderSearch, bug("folder: %s searchString: %s documentIdentifier: %s\n", folderOptions.path.c_str(), searchString.c_str(), to_s(self.documentIdentifier).c_str()););
	scanner.reset(new find::scan_path_t);
	scanner->set_folder_options(folderOptions);
	scanner->set_string(searchString);
	scanner->set_file_options(options);

	timer.reset();
	self.scannerProbeTimer = [OakTimer scheduledTimerWithTimeInterval:0.3 target:self selector:@selector(updateMatches:) userInfo:NULL repeats:YES];

	if(self.documentIdentifier)
	{
		if(document::document_ptr doc = document::find(to_s(self.documentIdentifier)))
			scanner->scan_document(doc);
		[self stop];
	}
	else
	{
		scanner->start();
	}
}

- (NSArray*)allMatchesForDocumentIdentifier:(NSString*)identifier
{
	return [matchInfo objectForKey:identifier];
}

- (double)searchDuration
{
	return timer.duration();
}

- (NSUInteger)countOfMatches
{
	return [[[matchInfo allValues] valueForKeyPath:@"@sum.@count"] intValue];
}

+ (NSSet*)keyPathsForValuesAffectingSelectedMatchCount
{
	return [NSSet setWithObject:@"countOfMatches"];
}

- (NSUInteger)countOfSelectedMatches
{
	return self.countOfMatches - replacementMatchesToSkip.count;
}

- (NSUInteger)scannedFileCount
{
	return scanner->get_scanned_file_count();
}

- (BOOL)skipReplacementForMatch:(FFMatch*)aMatch
{
	return [replacementMatchesToSkip containsObject:aMatch];
}

- (void)setSkipReplacement:(BOOL)flag forMatch:(FFMatch*)aMatch
{
	[self willChangeValueForKey:@"countOfSelectedMatches"];
	if(flag)
			[replacementMatchesToSkip addObject:aMatch];
	else	[replacementMatchesToSkip removeObject:aMatch];
	[self didChangeValueForKey:@"countOfSelectedMatches"];
}

- (void)setHasPerformedReplacement:(BOOL)flag
{
	ASSERTF(!hasPerformedReplacement || !flag, "Replacement has already been performed");
	hasPerformedReplacement = flag;
	[self updateChangeCount:NSChangeDone];
}

- (void)setHasPerformedSave:(BOOL)flag
{
	ASSERTF(!hasPerformedSave || !flag, "Save has already been performed");
	hasPerformedSave = flag;
	[self updateChangeCount:NSChangeCleared];
}

- (IBAction)saveAllDocuments:(id)sender
{
	NSUInteger fileCount = 0;
	std::vector<document::document_ptr> failedDocs;
	for(FFMatch* fileMatch in [self allDocumentsWithSelectedMatches])
	{
		if(document::document_ptr doc = [fileMatch match].document)
		{
			if(doc->save())
					++fileCount;
			else	failedDocs.push_back(doc);
		}
	}

	FindWindowController* fwc = [[self windowControllers] lastObject];
	if(failedDocs.empty())
	{
		fwc.statusString = [NSString stringWithFormat:@"%lu file%s saved.", fileCount, fileCount == 1 ? "" : "s"];
		[fwc.resultsOutlineView reloadData];
		self.hasPerformedSave = YES;
	}
	else
	{
		NSBeep();
		fwc.statusString = [NSString stringWithFormat:@"%zu file%s failed to save.", failedDocs.size(), failedDocs.size() == 1 ? "" : "s"];
	}
}

// ===================
// = Scanner Probing =
// ===================

- (void)updateMatches:(NSTimer*)timer
{
	D(DBF_Find_FolderSearch, bug("\n"););

	BOOL scannerIsStopped = !scanner->is_running();

	find::scan_path_matches_t const& matches = scanner->accept_matches();
	if(!matches.empty())
	{
		[self willChangeValueForKey:@"countOfMatches"];
		iterate(pair, matches)
		{
			NSString* uuid = [NSString stringWithCxxString:pair->first->identifier()];
			if(![matchInfo objectForKey:uuid])
			{
				[matchInfo setObject:[NSMutableArray array] forKey:uuid];
				[matchingDocuments addObject:[[FFMatch alloc] initWithDocument:pair->first]];
			}
			FFMatch* match = [[FFMatch alloc] initWithMatch:pair->second];
			[[matchInfo objectForKey:uuid] addObject:match];
			if(match.match.binary)
				[replacementMatchesToSkip addObject:match];
		}
		[self didChangeValueForKey:@"countOfMatches"];
		[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidReceiveResultsNotification object:self];
	}

	self.currentPath = [NSString stringWithCxxString:scanner->get_current_path()];

	if(scannerIsStopped)
		[self stop];
}

- (void)stop
{
	D(DBF_Find_FolderSearch, bug("\n"););
	if(scanner)
		scanner->stop();

	if(self.scannerProbeTimer)
	{
		[self.scannerProbeTimer invalidate];
		self.scannerProbeTimer = nil;
		[self updateMatches:nil];
		[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidFinishNotification object:self];
	}
}

// ==================
// = Setup/Teardown =
// ==================

- (id)init
{
	if(self = [super init])
	{
		D(DBF_Find_FolderSearch, bug("\n"););
		matchingDocuments        = [NSMutableArray new];
		matchInfo                = [NSMutableDictionary new];
		replacementMatchesToSkip = [NSMutableSet new];

		self.fileType = @"search-results";
	}
	return self;
}

- (void)close
{
	[self stop];
}

- (void)canCloseDocumentWithDelegate:(id)delegate shouldCloseSelector:(SEL)shouldCloseSelector contextInfo:(void*)contextInfo
{
	if(![self isDocumentEdited])
		return [super canCloseDocumentWithDelegate:delegate shouldCloseSelector:shouldCloseSelector contextInfo:contextInfo];

	NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Do you want to save the changes from your replace operation?" informativeText:@"Your changes will be lost if you don’t save them." buttons:@"Save All", @"Cancel", @"Don’t Save", nil];
	OakShowAlertForWindow(alert, [[self.windowControllers lastObject] window], ^(NSInteger returnCode){
		BOOL canClose = YES;
		if(returnCode == NSAlertFirstButtonReturn) // Save All
		{
			[self saveAllDocuments:self];
			canClose = self.hasPerformedSave;
		}
		else if(returnCode == NSAlertThirdButtonReturn) // Discard
			[self updateChangeCount:NSChangeCleared]; // FIXME Undo replacements
		else if(returnCode == NSAlertSecondButtonReturn) // Cancel
			canClose = NO;

		((void(*)(id, SEL, NSDocument*, BOOL, void*))[delegate methodForSelector:shouldCloseSelector])(delegate, shouldCloseSelector, self, canClose, contextInfo);
	});
}

- (BOOL)validateUserInterfaceItem:(id <NSValidatedUserInterfaceItem>)anItem
{
	if(anItem.action == @selector(saveAllDocuments:))
		return self.hasPerformedReplacement && !self.hasPerformedSave;
	return [super validateUserInterfaceItem:anItem];
}

- (void)dealloc
{
	D(DBF_Find_FolderSearch, bug("\n"););
	[self stop];
}
@end
