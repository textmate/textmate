#import <Foundation/Foundation.h>

// Main-thread, bounded journal. Contains event kinds/IDs, never document text.
@interface EditorContextChanges : NSObject
@property (nonatomic, readonly) NSString* cursor;
- (void)record:(NSString*)type documentId:(NSString*)documentId;
- (NSDictionary*)changesAfter:(NSString*)cursor;
@end
