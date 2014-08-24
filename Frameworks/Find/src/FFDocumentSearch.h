#import "scan_path.h"
#import <text/types.h>
#import <oak/duration.h>

extern NSString* const FFDocumentSearchDidReceiveResultsNotification;
extern NSString* const FFDocumentSearchDidFinishNotification;

// This is an Obj-C wrapper around match_t so that we can put it in an NSArray
@interface FFMatch : NSObject
- (find::match_t const&)match;
@end

@interface FFDocumentSearch : NSObject
// Set up the search with these options
@property (nonatomic, copy) NSString* searchString;
@property (nonatomic) find::options_t options;

// Either directory _or_ a documentIdentifier should be provided
// If a documentIdentifier is set only that document will be searched.
@property (nonatomic) NSString* directory;
@property (nonatomic) NSString* documentIdentifier;

@property (nonatomic) BOOL followLinks;
@property (nonatomic) path::glob_list_t globList;

// Start the search, observing the currentPath, and prematurely stop it if desired.
- (void)start;
- (void)stop;

@property (nonatomic, readonly) NSString*      currentPath;
@property (nonatomic, readonly) NSTimeInterval searchDuration;
@property (nonatomic, readonly) NSUInteger     scannedFileCount;
@property (nonatomic, readonly) NSUInteger     scannedByteCount;
@end
