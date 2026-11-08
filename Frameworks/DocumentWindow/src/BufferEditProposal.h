#import <Cocoa/Cocoa.h>
@class OakDocument;

@interface EditReviewWindowController : NSWindowController <NSWindowDelegate>
@property (nonatomic, readonly) NSScrollView* diffScrollView;
- (instancetype)initWithTitle:(NSString*)title documentName:(NSString*)name applyTitle:(NSString*)applyTitle completion:(void (^)(NSModalResponse))completion;
- (IBAction)apply:(id)sender;
- (IBAction)discard:(id)sender;
@end

// One immutable buffer edit proposal. Proposals never write to disk or apply
// themselves: only the review window's Apply action calls applyToDocument:.
@interface BufferEditProposal : NSObject
// A validated plan from EditorContextBuffer, prepared on a worker thread.
- (instancetype)initWithEditPlan:(NSDictionary*)plan summary:(NSString*)summary error:(NSString**)error;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) NSString* documentId;
@property (nonatomic, readonly) NSString* status;
@property (nonatomic, readonly) NSString* summary;
@property (nonatomic, readonly) NSString* diff;
- (BOOL)matchesDocument:(OakDocument*)document;
- (NSString*)prepareReplacements:(NSArray<NSString*>*)replacements summary:(NSString*)summary;
- (BOOL)applyToDocument:(OakDocument*)document;
- (void)discard;
- (void)markStale;
- (EditReviewWindowController*)reviewControllerWithCompletion:(void (^)(NSModalResponse))completion;
@end
