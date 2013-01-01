#import <document/document.h>

@protocol DocumentOpenHelperDelegate;

@interface DocumentOpenHelper : NSObject
@property (nonatomic, weak) id <DocumentOpenHelperDelegate> delegate;
- (void)tryOpenDocument:(document::document_ptr const&)aDocument forWindow:(NSWindow*)aWindow delegate:(id <DocumentOpenHelperDelegate>)aDelegate;
@end

@protocol DocumentOpenHelperDelegate <NSObject>
@optional
- (void)documentOpenHelper:(DocumentOpenHelper*)documentOpenHelper didOpenDocument:(document::document_ptr const&)aDocument;
- (void)documentOpenHelper:(DocumentOpenHelper*)documentOpenHelper failedToOpenDocument:(document::document_ptr const&)aDocument error:(std::string const&)aMessage usingFilter:(oak::uuid_t const&)filterUUID;
@end
