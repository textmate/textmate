@class FFResultNode;

@interface FFResultsViewController : NSViewController
@property (nonatomic) SEL selectResultAction;
@property (nonatomic) SEL removeResultAction;
@property (nonatomic) SEL doubleClickResultAction;
@property (nonatomic) id target;

@property (nonatomic) FFResultNode* results;
@property (nonatomic, readonly) NSArray* selectedResults;

@property (nonatomic) BOOL hideCheckBoxes;

@property (nonatomic) BOOL showReplacementPreviews;
@property (nonatomic) NSString* replaceString;

- (void)insertItemsAtIndexes:(NSIndexSet*)anIndexSet;
- (void)showResultNode:(FFResultNode*)aResultNode;

- (void)selectNextResultWrapAround:(BOOL)wrapAround;
- (void)selectPreviousResultWrapAround:(BOOL)wrapAround;

- (IBAction)selectNextDocument:(id)sender;
- (IBAction)selectPreviousDocument:(id)sender;
- (IBAction)toggleCollapsedState:(id)sender;
@end
