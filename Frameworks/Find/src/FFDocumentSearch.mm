#import "FFDocumentSearch.h"
#import <OakAppKit/OakFileIconImage.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakTimer.h>
#import <ns/ns.h>
#import <oak/oak.h>

NSString* const FFDocumentSearchDidReceiveResultsNotification = @"FFDocumentSearchDidReceiveResultsNotification";
NSString* const FFDocumentSearchDidFinishNotification         = @"FFDocumentSearchDidFinishNotification";

@interface FFMatch ()
{
	OBJC_WATCH_LEAKS(FFMatch);

	find::match_t match;
	document::document_t::callback_t* callback;
	NSImage* icon;
}
- (void)updateIcon;
@end

@implementation FFMatch
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
		__weak FFMatch* self;
	};

	if(!icon)
		icon = [OakFileIconImage fileIconImageWithPath:[NSString stringWithCxxString:match.document->path()] isModified:match.document->is_modified()];
	if(!callback)
		match.document->add_callback(callback = new document_callback_t(self));
	return icon;
}

- (void)updateIcon
{
	icon = nil;
}

- (NSString*)path
{
	return [NSString stringWithCxxString:match.document->path()];
}

- (NSString*)identifier
{
	return [NSString stringWithCxxString:match.document->identifier()];
}

- (NSString*)description
{
	return [NSString stringWithFormat:@"Match in ‘%@’ at line %zu: %@", [NSString stringWithCxxString:match.document->display_name()], match.line_number, [NSString stringWithCxxString:match.excerpt]];
}
@end

@interface FFDocumentSearch ()
{
	OBJC_WATCH_LEAKS(FFDocumentSearch);

	scan_path_ptr  _scanner;
	OakTimer*      _scannerProbeTimer;
	NSDate*        _searchStartDate;
	NSTimeInterval _searchDuration;
}
@property (nonatomic, readwrite) NSString* currentPath;
@end

OAK_DEBUG_VAR(Find_FolderSearch);

@implementation FFDocumentSearch
- (void)start
{
	D(DBF_Find_FolderSearch, bug("folder: %s searchString: %s documentIdentifier: %s\n", self.folderOptions.path.c_str(), to_s(self.searchString).c_str(), to_s(self.documentIdentifier).c_str()););
	_scanner = std::make_shared<find::scan_path_t>();
	_scanner->set_folder_options(self.folderOptions);
	_scanner->set_search_string(to_s(self.searchString));
	_scanner->set_file_options(self.options);

	_searchStartDate   = [NSDate new];
	_scannerProbeTimer = [OakTimer scheduledTimerWithTimeInterval:0.3 target:self selector:@selector(updateMatches:) userInfo:NULL repeats:YES];

	if(self.documentIdentifier)
	{
		if(document::document_ptr doc = document::find(to_s(self.documentIdentifier)))
			_scanner->scan_document(doc);
		[self stop];
	}
	else
	{
		_scanner->start();
	}
}

- (NSUInteger)scannedFileCount
{
	return _scanner->scanned_file_count();
}

// ===================
// = Scanner Probing =
// ===================

- (void)updateMatches:(NSTimer*)timer
{
	D(DBF_Find_FolderSearch, bug("\n"););

	BOOL scannerIsStopped = !_scanner->is_running();

	std::vector<find::match_t> const& matches = _scanner->accept_matches();
	if(!matches.empty())
	{
		NSMutableArray* newMatches = [NSMutableArray array];
		for(auto const& match : matches)
			[newMatches addObject:[[FFMatch alloc] initWithMatch:match]];
		[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidReceiveResultsNotification object:self userInfo:@{ @"matches" : newMatches }];
	}

	self.currentPath = [NSString stringWithCxxString:_scanner->current_path()];

	if(scannerIsStopped)
		[self stop];
}

- (void)stop
{
	D(DBF_Find_FolderSearch, bug("\n"););
	if(_scanner)
		_scanner->stop();

	_searchDuration = [[NSDate date] timeIntervalSinceDate:_searchStartDate];

	if(_scannerProbeTimer)
	{
		[_scannerProbeTimer invalidate];
		_scannerProbeTimer = nil;
		[self updateMatches:nil];
		[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidFinishNotification object:self];
	}
}

// ==================
// = Setup/Teardown =
// ==================

- (void)dealloc
{
	D(DBF_Find_FolderSearch, bug("\n"););
	[self stop];
}
@end
