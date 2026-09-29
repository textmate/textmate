typedef void (^NewComparisonHandler)(NSString* leftPath, NSString* rightPath);

@interface NewComparisonWindowController : NSWindowController
- (instancetype)initWithCompletionHandler:(NewComparisonHandler)completionHandler;
@end
