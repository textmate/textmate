#import <OakFoundation/OakFindProtocol.h>

namespace find_tags
{
	enum { in_document = 1, in_selection, in_project, in_folder};
}

PUBLIC extern NSString* const FFSearchInDocument;
PUBLIC extern NSString* const FFSearchInSelection;
PUBLIC extern NSString* const FFSearchInOpenFiles;

PUBLIC @interface Find : NSResponder
@property (nonatomic, copy) NSString* projectFolder;
@property (nonatomic, copy) NSString* projectIdentifier;
@property (nonatomic, copy) NSString* documentIdentifier;

@property (nonatomic, readonly) BOOL      isVisible;
@property (nonatomic, readonly) NSString* searchFolder;

+ (Find*)sharedInstance;

- (void)showFindWindowFor:(NSString*)searchScope;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;
@end
