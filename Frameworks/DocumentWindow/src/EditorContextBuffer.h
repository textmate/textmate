#import <Foundation/Foundation.h>
#import <selection/types.h>
@class OakDocument;

// Immutable, shared-storage snapshot with a lazily built UTF-16 index.
@interface EditorContextBuffer : NSObject
+ (void)invalidateDocument:(OakDocument*)document;
+ (instancetype)bufferForDocument:(OakDocument*)document revision:(NSString*)revision;
- (NSDictionary*)editPlanForArguments:(NSDictionary*)arguments document:(NSDictionary*)document deadline:(double)deadline;
- (NSDictionary*)navigationForArguments:(NSDictionary*)arguments deadline:(double)deadline;
- (NSDictionary*)responseForMethod:(NSString*)method arguments:(NSDictionary*)arguments base:(NSDictionary*)base
                         document:(NSDictionary*)document ranges:(ng::ranges_t const&)ranges caret:(NSUInteger)caret deadline:(double)deadline;
@end
