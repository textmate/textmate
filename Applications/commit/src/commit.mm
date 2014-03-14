#include <CommitWindow/CommitWindow.h>
#include <oak/oak.h>

static double const AppVersion  = 1.0;
static size_t const AppRevision = APP_REVISION;

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
		fprintf(stdout, "%s", [out UTF8String]);

	_returnCode = [someOptions[kOakCommitWindowReturnCode] intValue];

	// Tear down the connection in next event loop iteration.
	// This should allow the sender to get a reply.
	[self performSelector:@selector(setConnection:) withObject:nil afterDelay:0];
}
@end

int main (int argc, char* argv[])
{
	if(argc == 2 && (strcmp(argv[1], "-v") == 0 || strcmp(argv[1], "--version") == 0))
	{
		fprintf(stderr, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
		return 0;
	}

	@autoreleasepool {
		if(OakCommitWindowClient* client = [[OakCommitWindowClient alloc] init])
		{
			NSMutableArray* arg = [NSMutableArray array];
			for(size_t i = 0; i < argc; ++i)
				[arg addObject:[NSString stringWithUTF8String:argv[i]]];

			NSDictionary* plist = @{
				kOakCommitWindowClientPortName : client.portName,
				kOakCommitWindowArguments      : arg,
				kOakCommitWindowEnvironment    : [[NSProcessInfo processInfo] environment],
			};

			if(id proxy = [NSConnection rootProxyForConnectionWithRegisteredName:kOakCommitWindowServerConnectionName host:nil])
			{
				[proxy setProtocolForProxy:@protocol(OakCommitWindowServerProtocol)];
				[proxy connectFromClientWithOptions:plist];

				while(client.connection)
					[[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]];

				return client.returnCode;
			}
			else
			{
				fprintf(stderr, "%s: failed connecting to ‘%s’\n", getprogname(), [kOakCommitWindowServerConnectionName UTF8String]);
			}
		}
	}
	return 0;
}
