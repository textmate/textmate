#import <OakFoundation/src/OakFindProtocol.h>
#import <text/src/types.h>

@class OakDocument;

@interface FindMatch : NSObject
@property (nonatomic, readonly) NSUUID* UUID;
@property (nonatomic, readonly) text::range_t firstRange;
@property (nonatomic, readonly) text::range_t lastRange;
- (instancetype)initWithUUID:(NSUUID*)uuid firstRange:(text::range_t const&)firstRange lastRange:(text::range_t const&)lastRange;
@end

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

@interface Find : NSWindowController
@property (class, readonly) Find* sharedInstance;

@property (nonatomic) FFSearchTarget searchTarget;

@property (nonatomic, weak) id <FindDelegate> delegate;
@property (nonatomic) NSString* projectFolder;
@property (nonatomic) NSArray* fileBrowserItems;
@property (nonatomic) NSUUID* documentIdentifier;

@property (nonatomic, readonly, getter = isVisible) BOOL visible;

@property (nonatomic) NSArray<FindMatch*>* findMatches;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;
@end
