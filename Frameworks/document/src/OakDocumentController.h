#import <oak/misc.h>

@class OakDocument;

PUBLIC extern NSString* kSearchFollowDirectoryLinksKey;
PUBLIC extern NSString* kSearchFollowFileLinksKey;
PUBLIC extern NSString* kSearchDepthFirstSearchKey;
PUBLIC extern NSString* kSearchExcludeDirectoryGlobsKey;
PUBLIC extern NSString* kSearchExcludeFileGlobsKey;
PUBLIC extern NSString* kSearchExcludeGlobsKey;
PUBLIC extern NSString* kSearchDirectoryGlobsKey;
PUBLIC extern NSString* kSearchFileGlobsKey;
PUBLIC extern NSString* kSearchGlobsKey;

PUBLIC @interface OakDocumentController : NSObject
+ (OakDocumentController*)sharedInstance;

- (OakDocument*)documentWithPath:(NSString*)aPath;
- (OakDocument*)findDocumentWithIdentifier:(NSUUID*)anUUID;
- (NSArray<OakDocument*>*)documents;
- (NSArray<OakDocument*>*)openDocuments;

- (NSInteger)lruRankForDocument:(OakDocument*)aDocument;
- (void)didTouchDocument:(OakDocument*)aDocument;

- (void)enumerateDocumentsAtPath:(NSString*)aDirectory options:(NSDictionary*)someOptions usingBlock:(void(^)(OakDocument* document, BOOL* stop))block;

// For use by OakDocument
- (void)register:(OakDocument*)aDocument;
- (void)unregister:(OakDocument*)aDocument;
- (void)update:(OakDocument*)aDocument;
- (NSUInteger)firstAvailableUntitledCount;
@end
