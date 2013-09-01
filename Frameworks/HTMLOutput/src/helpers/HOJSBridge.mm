#import "HOJSBridge.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <OakFoundation/NSArray Additions.h>
#import <document/collection.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <cf/run_loop.h>

@interface HOJSShellCommand : NSObject
- (id)initShellCommand:(NSString*)aCommand withEnvironment:(const std::map<std::string, std::string>&)someEnvironment andExitHandler:(id)aHandler;
@end

/*
	This class exposes a ‘TextMate’ object to the JavaScript interpreter.
	The object will have the following methods available:

		system()                 See HOJSShellCommand.mm for information.
		log(msg)                 Adds a message to the system console (using NSLog).
		open(path, options)      Opens a file on disk as a document in the current application.
		                         options may be either a selection range string or a (line) number.

	in addition, these properties are exposed:

		busy       (boolean)     The busy spinner in the output window will be displayed when this is true.
		progress   (double, 0-1) Controls the value displayed in the determinate progress indicator.
*/

OAK_DEBUG_VAR(HTMLOutput_JSBridge);

@implementation HOJSBridge
{
	std::map<std::string, std::string> environment;

	// unused dummy keys to get them exposed to javascript
	BOOL isBusy;
	float progress;
}

- (std::map<std::string, std::string> const&)environment;
{
	return environment;
}

- (void)setEnvironment:(const std::map<std::string, std::string>&)variables;
{
	environment = variables;
}

+ (BOOL)isSelectorExcludedFromWebScript:(SEL)aSelector
{
	return aSelector != @selector(system:handler:) && aSelector != @selector(log:) && aSelector != @selector(openFile:withOptions:);
}

+ (NSString*)webScriptNameForSelector:(SEL)aSelector
{
	if(aSelector == @selector(system:handler:))
		return @"system";
	else if(aSelector == @selector(log:))
		return @"log";
	else if(aSelector == @selector(openFile:withOptions:))
		return @"open";
	return NSStringFromSelector(aSelector);
}

+ (BOOL)isKeyExcludedFromWebScript:(char const*)name
{
	return strcmp(name, "isBusy") != 0 && strcmp(name, "progress") != 0;
}

+ (NSString*)webScriptNameForKey:(char const*)name
{
	return @(name);
}

- (void)setIsBusy:(BOOL)flag
{
	[_delegate setIsBusy:flag];
}

- (void)setProgress:(id)newProgress;
{
	[(id <HOJSBridgeDelegate>)_delegate setProgress:[newProgress floatValue]];
}

- (double)progress
{
	return [_delegate progress];
}

- (void)log:(NSString*)aMessage
{
	NSLog(@"JavaScript Log: %@", aMessage);
}

- (void)openFile:(NSString*)path withOptions:(id)options
{
	text::range_t range = text::range_t::undefined;
	if([options isKindOfClass:[NSNumber class]])
		range = text::pos_t([options intValue]-1, 0);
	else if([options isKindOfClass:[NSString class]])
		range = to_s((NSString*)options);
	document::show(document::create(to_s(path)), document::kCollectionAny, range);
}

- (id)system:(NSString*)aCommand handler:(id)aHandler
{
	return [[HOJSShellCommand alloc] initShellCommand:aCommand withEnvironment:[self environment] andExitHandler:[aHandler isKindOfClass:[WebUndefined class]] ? nil : aHandler];
}
@end

/*
	<http://developer.apple.com/documentation/AppleApplications/Conceptual/Dashboard_ProgTopics/Articles/CommandLine.html>

	# Synchronous Operation

	Example: obj = TextMate.system("/usr/bin/id -un", null);

	Result is an object with following properties:

		outputString:  The output of the command, as placed on stdout.
		errorString:   The output of the command, as placed on stderr.
		status:        The exit status of the command.

	# Asynchronous Operation

	Example: obj = TextMate.system("/usr/bin/id -un", handler);

	Handler is called when the command is finished and given an object with the following properties:

		outputString:  The last output of the command, as placed on stdout.
		errorString:   The last output of the command, as placed on stderr.
		status:        The exit status of the command.

	Result is an object with following properties/methods:

		outputString:  The current string written to stdout (standard output) by the command.
		errorString:   The current string written to stderr (standard error output) by the command.
		status:        The command’s exit status, as defined by the command.
		onreadoutput:  A function called whenever the command writes to stdout. The handler must accept a single argument; when called, the argument contains the current string placed on stdout.
		onreaderror:   A function called whenever the command writes to stderr. The handler must accept a single argument; when called, the argument contains the current string placed on stderr.
		cancel():      Cancels the execution of the command.
		write(string): Writes a string to stdin (standard input).
		close():       Closes stdin (EOF).

*/

OAK_DEBUG_VAR(HTMLOutput_JSShellCommand);

@interface HOJSShellCommand ()
{
	OBJC_WATCH_LEAKS(HOJSShellCommand);

	io::process_t process;
	std::string output, error;

	// unused dummy keys to get them exposed to javascript
	NSString* outputString;
	NSString* errorString;
}
@property (nonatomic) id exitHandler;
@property (nonatomic) id onreadoutput;
@property (nonatomic) id onreaderror;
@property (nonatomic) int status;
@end

@implementation HOJSShellCommand
// We need @synthesize to avoid the instance variables from being prefixed with an underscore, as they are mapped to JavaScript
@synthesize onreadoutput, onreaderror, status;

- (id)initShellCommand:(NSString*)aCommand withEnvironment:(const std::map<std::string, std::string>&)someEnvironment andExitHandler:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("run ‘%s’ with exit handler %s\n", to_s(aCommand).c_str(), BSTR(aHandler)););
	if(self = [super init])
	{
		self.exitHandler = aHandler;
		if(process = io::spawn(std::vector<std::string>{ "/bin/sh", "-c", to_s(aCommand) }, someEnvironment))
		{
			auto runLoop = std::make_shared<cf::run_loop_t>(kCFRunLoopDefaultMode, 15);
			auto weakRunLoop = std::weak_ptr<cf::run_loop_t>(runLoop);
			auto group = dispatch_group_create();
			auto queue = aHandler ? dispatch_get_main_queue() : dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

			dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				std::vector<char> buf(1024);
				while(ssize_t len = read(process.out, buf.data(), buf.size()))
				{
					if(len < 0)
						break;

					dispatch_sync(queue, ^{
						if(self.onreadoutput)
							output.erase(output.begin(), utf8::find_safe_end(output.begin(), output.end()));
						output.insert(output.end(), buf.begin(), buf.begin() + len);
						if(self.onreadoutput)
							[self.onreadoutput callWebScriptMethod:@"call" withArguments:@[ self.onreadoutput, [self outputString] ]];
					});
				}
			});

			dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				std::vector<char> buf(1024);
				while(ssize_t len = read(process.err, buf.data(), buf.size()))
				{
					if(len < 0)
						break;

					dispatch_sync(queue, ^{
						if(self.onreaderror)
							error.erase(error.begin(), utf8::find_safe_end(error.begin(), error.end()));
						error.insert(error.end(), buf.begin(), buf.begin() + len);
						if(self.onreaderror)
							[self.onreaderror callWebScriptMethod:@"call" withArguments:@[ self.onreaderror, [self errorString] ]];
					});
				}
			});

			dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				int result = 0;
				if(waitpid(process.pid, &result, 0) != process.pid)
					perror("waitpid");
				process.pid = -1;
				dispatch_sync(queue, ^{
					self.status = WIFEXITED(result) ? WEXITSTATUS(result) : -1;
				});
			});

			dispatch_group_notify(group, dispatch_get_main_queue(), ^{
				close(process.out);
				close(process.err);
				if(self.exitHandler)
					[self.exitHandler callWebScriptMethod:@"call" withArguments:@[ self.exitHandler, self ]];
				else if(auto runLoop = weakRunLoop.lock())
					runLoop->stop();
			});

			if(!self.exitHandler)
			{
				[self closeInput];

				while(runLoop->start() == false) // timeout
				{
					NSInteger choice = NSRunAlertPanel(@"JavaScript Warning", @"The command ‘%@’ has been running for 15 seconds. Would you like to stop it?\n\nTo avoid this warning, the bundle command should use the asynchronous version of TextMate.system().", @"Stop Command", @"Cancel", nil, aCommand);
					if(choice == NSAlertDefaultReturn) // "Stop Command"
					{
						runLoop.reset();
						[self cancelCommand];
						break;
					}
				}
			}

			dispatch_release(group);
		}
	}
	return self;
}

- (void)cancelCommand
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%p\n", &process););

	self.onreadoutput = nil;
	self.onreaderror  = nil;
	self.exitHandler  = nil;

	[self closeInput];

	if(process)
		kill(process.pid, SIGINT);
}

- (void)writeToInput:(NSString*)someData
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%zu bytes\n", strlen([someData UTF8String])););
	if(process.in != -1)
	{
		char const* bytes = [someData UTF8String];
		write(process.in, bytes, strlen(bytes));
	}
}

- (void)closeInput
{
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););
	if(process.in != -1)
	{
		close(process.in);
		process.in = -1;
	}
}

- (void)dealloc
{
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););
	[self cancelCommand];
}

// =========================
// = JavaScript Properties =
// =========================

+ (BOOL)isKeyExcludedFromWebScript:(char const*)name
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", name););
	static auto const PublicProperties = new std::set<std::string>{ "outputString", "errorString", "onreadoutput", "onreaderror" };
	return PublicProperties->find(name) == PublicProperties->end();
}

+ (NSString*)webScriptNameForKey:(char const*)name
{
	return @(name);
}

+ (BOOL)isSelectorExcludedFromWebScript:(SEL)aSelector
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", sel_getName(aSelector)););
	static auto const PublicMethods = new std::set<SEL>{ @selector(cancelCommand), @selector(writeToInput:), @selector(closeInput) };
	return PublicMethods->find(aSelector) == PublicMethods->end();
}

+ (NSString*)webScriptNameForSelector:(SEL)aSelector
{
	if(aSelector == @selector(cancelCommand))
		return @"cancel";
	else if(aSelector == @selector(writeToInput:))
		return @"write";
	else if(aSelector == @selector(closeInput))
		return @"close";

	ASSERT(false);
	return @"undefined";
}

- (NSString*)outputString     { return output.empty() ? @"" : [NSString stringWithUTF8String:output.data() length:utf8::find_safe_end(output.begin(), output.end()) - output.begin()]; }
- (NSString*)errorString      { return error.empty()  ? @"" : [NSString stringWithUTF8String:error.data()  length:utf8::find_safe_end(error.begin(),  error.end())  - error.begin()];  }

- (void)setOnreadoutput:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", [[aHandler description] UTF8String]););
	if(onreadoutput = aHandler)
		[onreadoutput callWebScriptMethod:@"call" withArguments:@[ onreadoutput, [self outputString] ]];
}

- (void)setOnreaderror:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", [[aHandler description] UTF8String]););
	if(onreaderror = aHandler)
		[onreaderror callWebScriptMethod:@"call" withArguments:@[ onreaderror, [self errorString] ]];
}

- (void)finalizeForWebScript
{
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););
	[self cancelCommand];
}
@end
