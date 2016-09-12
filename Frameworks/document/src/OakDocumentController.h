#import <oak/misc.h>

@class OakDocument;

PUBLIC @interface OakDocumentController : NSObject
+ (OakDocumentController*)sharedInstance;

- (OakDocument*)documentWithPath:(NSString*)aPath;
- (OakDocument*)findDocumentWithIdentifier:(NSUUID*)anUUID;
- (NSArray<OakDocument*>*)documents;

- (NSInteger)lruRankForDocument:(OakDocument*)aDocument;
- (void)didTouchDocument:(OakDocument*)aDocument;

// For use by OakDocument
- (void)register:(OakDocument*)aDocument;
- (void)unregister:(OakDocument*)aDocument;
- (void)update:(OakDocument*)aDocument;
- (NSUInteger)firstAvailableUntitledCount;
@end
