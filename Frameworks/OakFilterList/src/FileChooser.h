#import "OakChooser.h"
#import <document/document.h>
#import <scm/scm.h>

PUBLIC @interface FileChooser : OakChooser
@property (nonatomic) NSString* path;
@property (nonatomic) std::vector<document::document_ptr> const& openDocuments;
@property (nonatomic) oak::uuid_t const& currentDocument;

@property (nonatomic) BOOL onlyShowOpenDocuments;

+ (instancetype)sharedInstance;
@end
