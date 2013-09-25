@interface EncodingWindowController : NSWindowController
- (id)initWithFirst:(char const*)firstPointer last:(char const*)lastPointer;
@property (nonatomic) NSString* encoding;
@property (nonatomic) NSString* displayName;
@property (nonatomic) BOOL acceptableEncoding;
@property (nonatomic) BOOL trainClassifier;
@end
