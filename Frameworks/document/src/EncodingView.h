@interface EncodingWindowController : NSWindowController
- (instancetype)initWithData:(NSData*)data;
- (void)beginSheetModalForWindow:(NSWindow*)aWindow completionHandler:(void(^)(NSModalResponse))callback;
@property (nonatomic) NSString* encoding;
@property (nonatomic, readonly) NSString* encodingNoBOM; // Same as encoding except there will never be a //BOM modifier
@property (nonatomic) NSString* displayName;
@property (nonatomic) BOOL acceptableEncoding;
@property (nonatomic) BOOL trainClassifier;
@end
