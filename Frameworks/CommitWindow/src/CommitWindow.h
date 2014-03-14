#import <oak/misc.h>

static NSString* const kOakCommitWindowServerConnectionName = @"com.macromates.textmate.commit-window-server";
static NSString* const kOakCommitWindowClientPortName       = @"clientPortName";
static NSString* const kOakCommitWindowArguments            = @"arguments";
static NSString* const kOakCommitWindowEnvironment          = @"environment";
static NSString* const kOakCommitWindowStandardOutput       = @"stdout";
static NSString* const kOakCommitWindowStandardError        = @"stderr";
static NSString* const kOakCommitWindowReturnCode           = @"returnCode";

@protocol OakCommitWindowClientProtocol <NSObject>
- (void)connectFromServerWithOptions:(NSDictionary*)someOptions;
@end

@protocol OakCommitWindowServerProtocol <NSObject>
- (void)connectFromClientWithOptions:(NSDictionary*)someOptions;
@end

PUBLIC @interface OakCommitWindowServer : NSObject <OakCommitWindowServerProtocol>
+ (instancetype)sharedInstance;
@end
