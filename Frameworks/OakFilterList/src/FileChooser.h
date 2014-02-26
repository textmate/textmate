#import "OakChooser.h"
#import <document/document.h>
#import <scm/scm.h>

extern NSUInteger const kFileChooserAllSourceIndex;
extern NSUInteger const kFileChooserOpenDocumentsSourceIndex;
extern NSUInteger const kFileChooserUncommittedChangesSourceIndex;

PUBLIC @interface FileChooser : OakChooser
@property (nonatomic) NSString* path;
@property (nonatomic) std::vector<document::document_ptr> const& openDocuments;
@property (nonatomic) oak::uuid_t const& currentDocument;
@property (nonatomic) NSUInteger sourceIndex;

+ (instancetype)sharedInstance;
@end
