static NSString* const kOakCommitWindowClientPortName       = @"clientPortName";
static NSString* const kOakCommitWindowArguments            = @"arguments";
static NSString* const kOakCommitWindowEnvironment          = @"environment";
static NSString* const kOakCommitWindowStandardOutput       = @"stdout";
static NSString* const kOakCommitWindowStandardError        = @"stderr";
static NSString* const kOakCommitWindowReturnCode           = @"returnCode";
static NSString* const kOakCommitWindowContinue             = @"continue";

@protocol OakCommitWindowClientProtocol <NSObject>
- (void)connectFromServerWithOptions:(NSDictionary*)someOptions;
@end

@protocol OakCommitWindowServerProtocol <NSObject>
- (void)connectFromClientWithOptions:(NSDictionary*)someOptions;
@end

@interface OakCommitWindowServer : NSObject <OakCommitWindowServerProtocol>
@property (class, readonly) OakCommitWindowServer* sharedInstance;
@end
