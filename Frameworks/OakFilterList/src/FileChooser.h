#import "OakChooser.h"
#import <document/document.h>
#import <scm/scm.h>

extern NSUInteger const kFileChooserAllSourceIndex;
extern NSUInteger const kFileChooserOpenDocumentsSourceIndex;
extern NSUInteger const kFileChooserUncommittedChangesSourceIndex;

PUBLIC @interface FileChooser : OakChooser
@property (nonatomic) NSString* path;
- (void)setOpenCppDocuments:(std::vector<document::document_ptr> const&)newDocuments;
- (void)setCurrentDocument:(oak::uuid_t const&)identifier;
@property (nonatomic) NSUInteger sourceIndex;

+ (instancetype)sharedInstance;
@end
