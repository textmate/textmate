#import <document/document.h>

@interface DocumentOpenHelper : NSObject
- (void)tryOpenDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow completionHandler:(void(^)(std::string const& error, oak::uuid_t const& filterUUID))aCompletionHandler;
@end
