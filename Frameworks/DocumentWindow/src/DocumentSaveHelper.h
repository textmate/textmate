#import <document/document.h>

@interface DocumentSaveHelper : NSObject
+ (void)trySaveDocuments:(std::vector<document::document_ptr> const&)someDocuments forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder completionHandler:(void(^)(BOOL success))callback;
+ (void)trySaveDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow defaultDirectory:(NSString*)aFolder completionHandler:(void(^)(BOOL success))callback;
@end

NSString* DefaultSaveNameForDocument (document::document_ptr const& aDocument);
