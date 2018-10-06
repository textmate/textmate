#import "FFDocumentSearch.h"
#import "CommonAncestor.h"
#import <OakFoundation/NSString Additions.h>
#import <document/OakDocumentController.h>
#import <document/OakDocument.h>
#import <settings/settings.h>
#import <ns/ns.h>
#import <oak/oak.h>

NSString* const FFDocumentSearchDidReceiveResultsNotification = @"FFDocumentSearchDidReceiveResultsNotification";
NSString* const FFDocumentSearchDidFinishNotification         = @"FFDocumentSearchDidFinishNotification";

@interface FFDocumentSearch ()
{
	OBJC_WATCH_LEAKS(FFDocumentSearch);

	BOOL           _searching;
	NSUInteger     _lastSearchToken;

	NSUInteger     _scannedFileCount;
	NSUInteger     _scannedByteCount;

	NSTimer*       _pollTimer;
	CGFloat        _pollInterval;
	NSTimeInterval _searchDuration;

	NSMutableArray<OakDocumentMatch*>* _matches;
}
@property (nonatomic, readwrite) NSString* currentPath;
@property NSString* lastDocumentPath;
@end

OAK_DEBUG_VAR(Find_FolderSearch);

static NSDictionary* GlobOptionsForPath (std::string const& path, NSString* glob, BOOL searchBinaryFiles, BOOL searchHiddenFolders)
{
	static std::map<std::string, NSString*> const map = {
		{ kSettingsExcludeDirectoriesInFolderSearchKey, kSearchExcludeDirectoryGlobsKey },
		{ kSettingsExcludeDirectoriesKey,               kSearchExcludeDirectoryGlobsKey },
		{ kSettingsExcludeFilesInFolderSearchKey,       kSearchExcludeFileGlobsKey      },
		{ kSettingsExcludeFilesKey,                     kSearchExcludeFileGlobsKey      },
		{ kSettingsExcludeInFolderSearchKey,            kSearchExcludeGlobsKey          },
		{ kSettingsExcludeKey,                          kSearchExcludeGlobsKey          },
	};

	NSDictionary* res = @{
		kSearchExcludeDirectoryGlobsKey: [NSMutableArray array],
		kSearchExcludeFileGlobsKey:      [NSMutableArray array],
		kSearchExcludeGlobsKey:          [NSMutableArray array],
		kSearchDirectoryGlobsKey:        [NSMutableArray arrayWithObject:searchHiddenFolders ? @"{,.}*" : @"*"],
		kSearchFileGlobsKey:             [NSMutableArray arrayWithArray:glob ? @[ glob ] : @[ ]],
		kSearchGlobsKey:                 [NSMutableArray array],
	};

	settings_t const settings = settings_for_path(NULL_STR, "", path);
	for(auto const& pair : map)
	{
		if(NSString* glob = to_ns(settings.get(pair.first)))
			[res[pair.second] addObject:glob];
	}

	if(!searchBinaryFiles)
	{
		if(NSString* glob = to_ns(settings.get(kSettingsBinaryKey)))
			[res[kSearchExcludeFileGlobsKey] addObject:glob];
	}

	return res;
}

@implementation FFDocumentSearch
- (void)start
{
	D(DBF_Find_FolderSearch, bug("folders %s, searchString ‘%s’\n", [[_paths description] UTF8String], [_searchString UTF8String]););
	[self stop];
	_matches = [NSMutableArray array];

	if(_searching)
		++_lastSearchToken;

	_searching    = YES;
	_pollInterval = 0.2;
	_pollTimer    = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(updateMatches:) userInfo:NULL repeats:NO];

	NSUInteger searchToken = _lastSearchToken;
	NSDate* searchStartDate = [NSDate date];

	NSMutableDictionary* options = [GlobOptionsForPath(to_s(CommonAncestor(_paths)), _glob, _searchBinaryFiles, _searchHiddenFolders) mutableCopy];
	options[kSearchFollowFileLinksKey]      = @(_searchFileLinks);
	options[kSearchFollowDirectoryLinksKey] = @(_searchFolderLinks);
	options[kSearchDepthFirstSearchKey]     = @YES;

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		[OakDocumentController.sharedInstance enumerateDocumentsAtPaths:_paths options:options usingBlock:^(OakDocument* document, BOOL* stop){
			if(*stop = searchToken != _lastSearchToken)
				return;

			self.lastDocumentPath = document.path;
			NSUInteger bufferSize = 0;
			NSArray* newMatches = [document matchesForString:_searchString options:_options bufferSize:&bufferSize];
			_scannedByteCount += bufferSize;
			_scannedFileCount += 1;

			if(newMatches.count)
			{
				@synchronized(self) {
					[_matches addObjectsFromArray:newMatches];
				}
			}
		}];

		dispatch_async(dispatch_get_main_queue(), ^{
			if(searchToken == _lastSearchToken)
			{
				_searching = NO;
				_searchDuration = [[NSDate date] timeIntervalSinceDate:searchStartDate];
				[self updateMatches:nil];
				[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidFinishNotification object:self];
			}
		});
	});
}

// ===================
// = Scanner Probing =
// ===================

- (void)updateMatches:(NSTimer*)timer
{
	D(DBF_Find_FolderSearch, bug("\n"););

	self.currentPath = [self.lastDocumentPath stringByDeletingLastPathComponent];
	@synchronized(self) {
		if(_matches.count)
		{
			[[NSNotificationCenter defaultCenter] postNotificationName:FFDocumentSearchDidReceiveResultsNotification object:self userInfo:@{ @"matches": _matches }];
			[_matches removeAllObjects];
		}
	}

	if(_searching)
			_pollTimer = [NSTimer scheduledTimerWithTimeInterval:_pollInterval target:self selector:@selector(updateMatches:) userInfo:NULL repeats:NO];
	else	[self stop];
}

- (void)stop
{
	D(DBF_Find_FolderSearch, bug("\n"););
	if(std::exchange(_searching, NO))
		++_lastSearchToken;

	[_pollTimer invalidate];
	_pollTimer = nil;

	@synchronized(self) {
		[_matches removeAllObjects];
	}
}
@end
