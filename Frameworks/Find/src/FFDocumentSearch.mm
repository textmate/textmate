#import "FFDocumentSearch.h"
#import "scan_path.h"
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
}
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

- (find::match_t const&)match
{
	return match;
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
	_scanner->set_path(to_s(_directory));
	_scanner->set_follow_links(_followLinks);
	_scanner->set_glob_list(_globList);
	_scanner->set_search_string(to_s(_searchString));
	_scanner->set_options(_options);

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

- (NSUInteger)scannedByteCount
{
	return _scanner->scanned_byte_count();
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
