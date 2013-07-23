#import <OakFoundation/OakFindProtocol.h>

PUBLIC extern NSString* const FFSearchInDocument;
PUBLIC extern NSString* const FFSearchInSelection;
extern NSString* const FFSearchInOpenFiles;

PUBLIC @interface Find : NSResponder <OakFindServerProtocol>
@property (nonatomic, copy) NSString* projectFolder;
@property (nonatomic, copy) NSString* projectIdentifier;
@property (nonatomic, copy) NSString* documentIdentifier;

@property (nonatomic, readonly) BOOL      isVisible;
@property (nonatomic, readonly) NSString* searchFolder;

+ (Find*)sharedInstance;

- (void)showFindWindowFor:(NSString*)searchScope;
- (IBAction)showFolderSelectionPanel:(id)sender;
- (IBAction)takeFindOptionToToggleFrom:(id)sender;

// =========================
// = OakFindProtocolServer =
// =========================

@property (nonatomic, readonly) find_operation_t findOperation;
@property (nonatomic, readonly) NSString*        findString;
@property (nonatomic, readonly) NSString*        replaceString;
@property (nonatomic, readonly) find::options_t  findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
@end
