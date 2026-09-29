#include <CommitWindow/src/CommitWindow.h>
#include <oak/oak.h>

static double const AppVersion = 1.1;

@interface OakCommitWindowClient : NSObject <OakCommitWindowClientProtocol>
@property (nonatomic) NSString*     portName;
@property (nonatomic) NSConnection* connection;
@property (nonatomic) NSInteger     returnCode;
@end

@implementation OakCommitWindowClient
- (id)init
{
	if(self = [super init])
	{
		_portName   = [NSString stringWithFormat:@"com.macromates.commit-window-client.%d", getpid()];
		_connection = [NSConnection new];

		[_connection setRootObject:self];
		if([_connection registerName:_portName] == NO)
		{
			fprintf(stderr, "%s: failed vending object as ‘%s’\n", getprogname(), [_portName UTF8String]);
			return nil;
		}
	}
	return self;
}

- (void)connectFromServerWithOptions:(NSDictionary*)someOptions
{
	if(NSString* err = someOptions[kOakCommitWindowStandardError])
		fprintf(stderr, "%s", [err UTF8String]);

	if(NSString* out = someOptions[kOakCommitWindowStandardOutput])
	{
		fprintf(stdout, "%s", [out UTF8String]);

		if([someOptions[kOakCommitWindowContinue] boolValue])
			fprintf(stdout, "TM_SCM_COMMIT_CONTINUE=1\n");
	}

	_returnCode = [someOptions[kOakCommitWindowReturnCode] intValue];

	// Tear down the connection in next run loop iteration.
	// This should make us run long enough to allow the sender to get a reply.
	[self performSelector:@selector(setConnection:) withObject:nil afterDelay:0];

	// Prior to 10.9 the above does not cause runMode:beforeDate: to return,
	// so for the benefit of 10.7 and 10.8 users, we explicitly call exit()
	[self performSelector:@selector(terminate:) withObject:nil afterDelay:0];
}

- (void)terminate:(id)sender
{
	exit(self.returnCode);
}
@end

int main (int argc, char* argv[])
{
	if(argc == 2 && (strcmp(argv[1], "-v") == 0 || strcmp(argv[1], "--version") == 0))
	{
		fprintf(stderr, "%1$s %2$.1f (" __DATE__ ")\n", getprogname(), AppVersion);
		return EX_OK;
	}

	@autoreleasepool {
		if(OakCommitWindowClient* client = [[OakCommitWindowClient alloc] init])
		{
			NSMutableArray* arg = [NSMutableArray array];
			for(size_t i = 0; i < argc; ++i)
				[arg addObject:@(argv[i])];

			NSDictionary* plist = @{
				kOakCommitWindowClientPortName: client.portName,
				kOakCommitWindowArguments:      arg,
				kOakCommitWindowEnvironment:    [[NSProcessInfo processInfo] environment],
			};

			NSString* serviceName = [NSString stringWithFormat:@"%@.CommitWindow.%@", NSProcessInfo.processInfo.environment[@"TM_APP_IDENTIFIER"], NSProcessInfo.processInfo.environment[@"TM_PID"]];
			if(id proxy = [NSConnection rootProxyForConnectionWithRegisteredName:serviceName host:nil])
			{
				[proxy setProtocolForProxy:@protocol(OakCommitWindowServerProtocol)];
				[proxy connectFromClientWithOptions:plist];

				while(client.connection)
					[[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];

				return client.returnCode;
			}
			else
			{
				fprintf(stderr, "%s: failed connecting to ‘%s’\n", getprogname(), serviceName.UTF8String);
			}
		}
	}
	return EX_OK;
}
