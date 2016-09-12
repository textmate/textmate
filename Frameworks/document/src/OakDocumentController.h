@class OakDocument;

@interface OakDocumentController : NSObject
+ (OakDocumentController*)sharedInstance;

- (OakDocument*)documentWithPath:(NSString*)aPath;
- (OakDocument*)findDocumentWithIdentifier:(NSUUID*)anUUID;
- (NSArray<OakDocument*>*)documents;

// For use by OakDocument
- (void)register:(OakDocument*)aDocument;
- (void)unregister:(OakDocument*)aDocument;
- (void)update:(OakDocument*)aDocument;
- (NSUInteger)firstAvailableUntitledCount;
@end
