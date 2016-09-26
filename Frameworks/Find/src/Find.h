#import <OakFoundation/OakFindProtocol.h>

namespace find_tags
{
	enum { in_document = 1, in_selection, in_project, in_folder};
}

typedef NS_ENUM(NSInteger, FFSearchTarget) {
	FFSearchTargetDocument = 0,
	FFSearchTargetSelection,
	FFSearchTargetOpenFiles,
	FFSearchTargetProject,
	FFSearchTargetFileBrowserItems,
	FFSearchTargetOther,
};

PUBLIC @interface Find : NSResponder
@property (nonatomic) FFSearchTarget searchTarget;

@property (nonatomic) NSString* projectFolder;
@property (nonatomic) NSUUID* projectIdentifier;
@property (nonatomic) NSArray* fileBrowserItems;
@property (nonatomic) NSUUID* documentIdentifier;

@property (nonatomic, readonly) BOOL isVisible;

+ (instancetype)sharedInstance;
- (void)showWindow:(id)sender;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;
@end
