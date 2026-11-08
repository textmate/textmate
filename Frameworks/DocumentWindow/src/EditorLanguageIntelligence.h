#import "EditorContextService.h"
@class OakDocument;

// Per-window result cache. Query targets are restricted to this window's loaded
// documents; result locations can refer to other files indexed by its server.
@interface EditorLanguageIntelligence : NSObject
- (EditorContextSnapshot*)snapshotForMethod:(NSString*)method arguments:(NSDictionary*)arguments
                                documents:(NSArray<OakDocument*>* (^)(void))documents
                                  current:(OakDocument*)current caret:(NSUInteger)caret windowId:(NSString*)windowId;
@end

// Pure, bounded normalization of untrusted LSP responses for native tests.
NSDictionary* EditorLanguageNormalize(NSString* method, id raw, NSDictionary* document, double deadline);
