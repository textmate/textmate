#import <regexp/src/find.h>

extern NSNotificationName const FFDocumentSearchDidReceiveResultsNotification;
extern NSNotificationName const FFDocumentSearchDidFinishNotification;

@interface FFDocumentSearch : NSObject
// Set up the search with these options
@property (nonatomic, copy) NSString* searchString;
@property (nonatomic) find::options_t options;

@property (nonatomic) NSArray* paths;

@property (nonatomic) BOOL searchFolderLinks;
@property (nonatomic) BOOL searchFileLinks;
@property (nonatomic) BOOL searchBinaryFiles;
@property (nonatomic) BOOL searchHiddenFolders;
@property (nonatomic) NSString* glob;

// Start the search, observing the currentPath, and prematurely stop it if desired.
- (void)start;
- (void)stop;

@property (nonatomic, readonly) NSString*      currentPath;
@property (nonatomic, readonly) NSTimeInterval searchDuration;
@property (nonatomic, readonly) NSUInteger     scannedFileCount;
@property (nonatomic, readonly) NSUInteger     scannedByteCount;
@end
