#import <Foundation/Foundation.h>
@class OakDocument;

// Capture shared buffer storage on the main thread, materialize on a worker.
@interface ProjectChangesBuffer : NSObject
+ (instancetype)captureDocument:(OakDocument*)document;
- (NSData*)data;
@end

// Worker-thread, read-only repository queries. Paths are passed as Git arguments,
// never shell commands. The baseline is the immutable commit in the snapshot.
NSDictionary* ProjectChangesSnapshot(NSString* directory, NSDictionary<NSString*, ProjectChangesBuffer*>* buffers);
NSDictionary* ProjectChangesDiff(NSDictionary* snapshot, NSDictionary* file, NSDictionary<NSString*, ProjectChangesBuffer*>* buffers);
