#import <text/src/types.h>

@class OakDocument;

extern NSString* kSearchFollowDirectoryLinksKey;
extern NSString* kSearchFollowFileLinksKey;
extern NSString* kSearchDepthFirstSearchKey;
extern NSString* kSearchIgnoreOrderingKey;
extern NSString* kSearchExcludeDirectoryGlobsKey;
extern NSString* kSearchExcludeFileGlobsKey;
extern NSString* kSearchExcludeGlobsKey;
extern NSString* kSearchDirectoryGlobsKey;
extern NSString* kSearchFileGlobsKey;
extern NSString* kSearchGlobsKey;

@interface OakDocumentController : NSObject
@property (class, readonly) OakDocumentController* sharedInstance;

- (OakDocument*)untitledDocument;
- (OakDocument*)documentWithPath:(NSString*)aPath;
- (OakDocument*)findDocumentWithIdentifier:(NSUUID*)anUUID;
- (NSArray<OakDocument*>*)documents;
- (NSArray<OakDocument*>*)openDocuments;

- (NSInteger)lruRankForDocument:(OakDocument*)aDocument;
- (void)didTouchDocument:(OakDocument*)aDocument;

- (void)enumerateDocumentsAtPath:(NSString*)aDirectory options:(NSDictionary*)someOptions usingBlock:(void(^)(OakDocument* document, BOOL* stop))block;
- (void)enumerateDocumentsAtPaths:(NSArray*)items options:(NSDictionary*)someOptions usingBlock:(void(^)(OakDocument* document, BOOL* stop))block;

// For use by OakDocument
- (void)register:(OakDocument*)aDocument;
- (void)unregister:(OakDocument*)aDocument;
- (void)update:(OakDocument*)aDocument;
- (NSUInteger)firstAvailableUntitledCount;

// Wrappers for OakDocumentWindowControllerCategory
- (void)showDocument:(OakDocument*)aDocument;
- (void)showDocument:(OakDocument*)aDocument inProject:(NSUUID*)identifier bringToFront:(BOOL)bringToFront;
@end

@interface OakDocumentController (OakDocumentWindowControllerCategory)
- (void)showDocument:(OakDocument*)aDocument andSelect:(text::range_t const&)selection inProject:(NSUUID*)identifier bringToFront:(BOOL)bringToFront;
- (void)showDocuments:(NSArray<OakDocument*>*)someDocument;
- (void)showFileBrowserAtPath:(NSString*)aPath;
@end
