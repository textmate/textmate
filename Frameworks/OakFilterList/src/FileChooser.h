#import <document/document.h>
#import <scm/scm.h>

PUBLIC @interface FileChooser : NSObject
@property (nonatomic) NSString* path;
@property (nonatomic) scm::info_ptr const& scmInfo;

@property (nonatomic) std::vector<document::document_ptr> const& openDocuments;
@property (nonatomic) oak::uuid_t const& currentDocument;

@property (nonatomic) BOOL onlyShowOpenDocuments;

@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;
@property (nonatomic) BOOL allowsMultipleSelection;

@property (nonatomic) NSString* filterString;
@property (nonatomic, readonly) NSArray* selectedItems;

- (void)showWindow:(id)sender;
- (void)showWindowRelativeToWindow:(NSWindow*)parentWindow;
- (void)close;
@end
