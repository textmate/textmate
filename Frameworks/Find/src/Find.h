#import <OakFoundation/OakFindProtocol.h>
#import <text/types.h>

@class OakDocument;

@interface FindMatch : NSObject
@property (nonatomic, readonly) NSUUID* UUID;
@property (nonatomic, readonly) text::range_t firstRange;
@property (nonatomic, readonly) text::range_t lastRange;
- (instancetype)initWithUUID:(NSUUID*)uuid firstRange:(text::range_t const&)firstRange lastRange:(text::range_t const&)lastRange;
@end

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

@protocol FindDelegate <NSObject>
- (void)selectRange:(text::range_t const&)range inDocument:(OakDocument*)aDocument;
- (void)bringToFront;
@end

PUBLIC @interface Find : NSResponder
@property (nonatomic) FFSearchTarget searchTarget;

@property (nonatomic, weak) id <FindDelegate> delegate;
@property (nonatomic) NSString* projectFolder;
@property (nonatomic) NSArray* fileBrowserItems;
@property (nonatomic) NSUUID* documentIdentifier;

@property (nonatomic, readonly) BOOL isVisible;

@property (nonatomic) NSArray<FindMatch*>* findMatches;

+ (instancetype)sharedInstance;
- (void)showWindow:(id)sender;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;
@end
