#import <Foundation/Foundation.h>

@class OakDocument;

typedef void (^EditorContextAction)(double deadline, BOOL (^canCommit)(void), void (^completion)(NSDictionary*));

// Capture on the main thread; await on a worker thread. Query snapshots render
// there; asynchronous editor actions return to the main thread to query or commit.
@interface EditorContextSnapshot : NSObject
+ (instancetype)response:(NSDictionary*)response;
+ (instancetype)renderer:(NSDictionary* (^)(double deadline))renderer;
+ (instancetype)action:(EditorContextAction)action;
- (NSDictionary*)renderBefore:(double)deadline;
@end

typedef EditorContextSnapshot* (^EditorContextProvider)(NSString* method, NSDictionary* arguments);
@interface EditorContextService : NSObject
+ (instancetype)sharedService;
- (NSDictionary<NSString*, NSString*>*)environmentForSession:(NSUUID*)session provider:(EditorContextProvider)provider;
- (void)revokeSession:(NSUUID*)session;
// The single definition of a document revision: buffer revision plus a content
// generation, because undo can rewind the buffer revision to an earlier value.
- (NSString*)revisionForDocument:(OakDocument*)document;
@end
