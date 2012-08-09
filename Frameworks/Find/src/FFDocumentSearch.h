#import "scan_path.h"
#import <text/types.h>
#import <oak/duration.h>

extern NSString* const FFDocumentSearchDidReceiveResultsNotification;
extern NSString* const FFDocumentSearchDidFinishNotification;

// This is largely an Obj-C wrapper around match_t so that it can be used as an NSCell’s value
// It may be better off moved to the window controller in future.
@interface FFMatch : NSObject <NSCopying>
{
	OBJC_WATCH_LEAKS(FFMatch);

	std::string matchText;
	find::match_t match;
	document::document_t::callback_t* callback;
	NSImage* icon;
}
- (id)initWithMatch:(find::match_t const&)aMatch;
- (find::match_t const&)match;
@property (nonatomic, readonly) NSString*            path;
@property (nonatomic, readonly) NSString*            identifier;
@property (nonatomic, readonly, retain) NSImage*     icon;
- (std::string const&)matchText;
@end

@class OakTimer;

@interface FFDocumentSearch : NSObject
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
// Set up the search with these options
@property (nonatomic, assign) find::options_t options;
@property (nonatomic, copy) NSString* searchString;

// Either folderOptions _or_ a documentIdentifier should be provided
// If a documentIdentifier is set only that document will be searched.
- (find::folder_scan_settings_t const&)folderOptions;
- (void)setFolderOptions:(find::folder_scan_settings_t const&)newFolderOptions;
@property (nonatomic, retain) NSString* documentIdentifier;

@property (nonatomic, retain) NSString* projectIdentifier;

// Start the search, observing the currentPath, and prematurely stop it if desired.
- (void)start;
@property (nonatomic, retain, readonly) NSString* currentPath;
- (void)stop;

// Attain matches during/after the search
- (NSArray*)allDocumentsWithMatches;
- (NSArray*)allMatchesForDocumentIdentifier:(NSString*)identifier;

- (NSArray*)allDocumentsWithSelectedMatches;
- (NSArray*)allSelectedMatchesForDocumentIdentifier:(NSString*)identifier;

- (BOOL)skipReplacementForMatch:(FFMatch*)aMatch;
- (void)setSkipReplacement:(BOOL)flag forMatch:(FFMatch*)aMatch;

- (NSUInteger)saveAllDocuments;

@property (nonatomic, assign) BOOL hasPerformedReplacement;
@property (nonatomic, assign) BOOL hasPerformedSave;

// Scan information
- (NSUInteger)countOfMatches;
- (NSUInteger)countOfSelectedMatches;
- (double)searchDuration;
- (NSUInteger)scannedFileCount;
@end
