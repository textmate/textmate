@interface GenieTask : NSObject
@property (nonatomic) NSString* standardInputString;
@property (nonatomic) NSDictionary* environment;
@property (nonatomic) NSUInteger timeOut;
@property (nonatomic, readonly, getter = isRunning) BOOL running;
- (instancetype)initWithCommand:(NSArray*)command directory:(NSString*)directory;
- (void)launch:(void(^)(int, NSData*, NSData*))callback;
- (void)terminate;
@end
