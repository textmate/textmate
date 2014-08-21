#import "scan_path.h"
#import <text/types.h>
#import <oak/duration.h>

extern NSString* const FFDocumentSearchDidReceiveResultsNotification;
extern NSString* const FFDocumentSearchDidFinishNotification;

// This is largely an Obj-C wrapper around match_t so that it can be used as an NSCell’s value
// It may be better off moved to the window controller in future.
@interface FFMatch : NSObject <NSCopying>
- (id)initWithMatch:(find::match_t const&)aMatch;
- (find::match_t const&)match;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) NSImage*  icon;
@property (nonatomic) BOOL exclude;
@property (nonatomic) BOOL replacementDone;
@end

@interface FFDocumentSearch : NSObject
// Set up the search with these options
@property (nonatomic) find::options_t options;
@property (nonatomic, copy) NSString* searchString;

// Either folderOptions _or_ a documentIdentifier should be provided
// If a documentIdentifier is set only that document will be searched.
@property (nonatomic) find::folder_scan_settings_t folderOptions;
@property (nonatomic) NSString* documentIdentifier;

// Start the search, observing the currentPath, and prematurely stop it if desired.
- (void)start;
- (void)stop;

@property (nonatomic, readonly) NSString*      currentPath;
@property (nonatomic, readonly) NSTimeInterval searchDuration;
@property (nonatomic, readonly) NSUInteger     scannedFileCount;
@end
