#import "HOJSBridge.h"
#import <OakFoundation/NSString Additions.h>
#import <oak/debug.h>
#import <OakSystem/process.h>
#import <OakSystem/reader.h>
#import <cf/run_loop.h>
#import <OakFoundation/NSArray Additions.h>
#import <document/collection.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <command/runner.h>

@interface HOJSShellCommand : NSObject
{
	OBJC_WATCH_LEAKS(HOJSShellCommand);

	std::string command;
	std::map<std::string, std::string> environment;

	std::vector<char> outputData, errorData;
	int status;

	id outputHandler, errorHandler, exitHandler;

	oak::process_t* process;
	bool didCloseInput;
	io::reader_t* outputReader;
	io::reader_t* errorReader;

	cf::run_loop_t runLoop;
	size_t completeCounter;

	// unused dummy keys to get them exposed to javascript
	NSString* outputString;
	NSString* errorString;
	id onreadoutput, onreaderror;
}
+ (HOJSShellCommand*)runShellCommand:(NSString*)aCommand withEnvironment:(const std::map<std::string, std::string>&)someEnvironment andExitHandler:(id)aHandler;
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
	[delegate setIsBusy:flag];
}

- (void)setProgress:(id)newProgress;
{
	[delegate setProgress:[newProgress floatValue]];
}

- (double)progress
{
	return [delegate progress];
}

- (void)setDelegate:(id)aDelegate
{
	delegate = aDelegate;
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
	return [HOJSShellCommand runShellCommand:aCommand withEnvironment:[self environment] andExitHandler:[aHandler isKindOfClass:[WebUndefined class]] ? nil : aHandler];
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
- (void)closeInput;

@property (nonatomic, retain) id outputHandler;
@property (nonatomic, retain) id errorHandler;
@property (nonatomic, retain) id exitHandler;
@end

@implementation HOJSShellCommand
@synthesize outputHandler, errorHandler, exitHandler;

- (id)initWithCommand:(NSString*)aCommand andEnvironment:(const std::map<std::string, std::string>&)someEnvironment
{
	if(self = [super init])
	{
		command     = [aCommand UTF8String];
		environment = someEnvironment;

		command::fix_shebang(&command);
	}
	return self;
}

- (void)increaseCompleteCounter
{
	if(++completeCounter == 3)
	{
		if(exitHandler)
				[exitHandler callWebScriptMethod:@"call" withArguments:@[ exitHandler, self ]];
		else	runLoop.stop();
	}
}

- (void)outputDataReceived:(char const*)bytes length:(size_t)len
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%zu bytes\n", len););
	if(exitHandler)
		outputData.erase(outputData.begin(), utf8::find_safe_end(outputData.begin(), outputData.end()));
	outputData.insert(outputData.end(), bytes, bytes + len);

	if(len == 0)
		[self increaseCompleteCounter];
	else if(outputHandler)
		[outputHandler callWebScriptMethod:@"call" withArguments:@[ outputHandler, [self valueForKey:@"outputString"] ]];
}

- (void)errorDataReceived:(char const*)bytes length:(size_t)len
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%zu bytes\n", len););
	if(exitHandler)
		errorData.erase(errorData.begin(), utf8::find_safe_end(errorData.begin(), errorData.end()));
	errorData.insert(errorData.end(), bytes, bytes + len);

	if(len == 0)
		[self increaseCompleteCounter];
	else if(errorHandler)
		[errorHandler callWebScriptMethod:@"call" withArguments:@[ errorHandler, [self valueForKey:@"errorString"] ]];
}

- (void)processDidExit:(int)rc
{
	D(DBF_HTMLOutput_JSShellCommand, bug("rc: %d\n", rc););
	status = rc;
	[self increaseCompleteCounter];
}

- (void)cancelCommand
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%p\n", process););

	self.outputHandler = nil;
	self.errorHandler  = nil;
	self.exitHandler   = nil;

	if(process)
	{
		[self closeInput];

		delete outputReader;
		outputReader = NULL;
		delete errorReader;
		errorReader = NULL;

		oak::kill_process_group_in_background(process->process_id);
		delete process;
		process = NULL;
	}
}

- (void)writeToInput:(NSString*)someData
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%zu bytes\n", strlen([someData UTF8String])););
	ASSERT(process);
	char const* bytes = [someData UTF8String];
	write(process->input_fd, bytes, strlen(bytes));
}

- (void)closeInput
{
	if(didCloseInput)
		return;
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););
	ASSERT(process);
	close(process->input_fd);
	didCloseInput = true;
}

- (void)launchAndWait:(BOOL)shouldWait
{
	struct process_t : oak::process_t
	{
		WATCH_LEAKS(process_t);
		HOJSShellCommand* self;
		process_t (HOJSShellCommand* self) : self(self) { }

		void did_exit (int rc)
		{
			oak::process_t::did_exit(rc);
			[self processDidExit:rc];
		}
	};

	struct reader_t : io::reader_t
	{
		WATCH_LEAKS(reader_t);
		bool regularOutput;
		HOJSShellCommand* self;

		reader_t (int fd, bool regularOutput, HOJSShellCommand* self) : io::reader_t(fd), regularOutput(regularOutput), self(self) { }
		void receive_data (char const* bytes, size_t len)
		{
			if(regularOutput)
					[self outputDataReceived:bytes length:len];
			else	[self errorDataReceived:bytes length:len];
		}
	};

	process = new process_t(self);
	process->command = command;
	process->environment = environment;
	process->launch();

	outputReader = new reader_t(process->output_fd, true, self);
	errorReader  = new reader_t(process->error_fd, false, self);

	while(shouldWait)
	{
		[self closeInput];
		runLoop.set_timeout(15);
		if(runLoop.start())
			break;

		fprintf(stderr, "*** Shell command still running after 30 seconds: %s\n", command.c_str());
		fprintf(stderr, "*** Completion counter %zu, running %s, %s %d\n", completeCounter, process->is_running ? "YES" : "NO", process->is_running ? "pid" : "rc", process->is_running ? process->process_id : status);
		fprintf(stderr, "*** stdout (%d): %.*s\n", process->output_fd, (int)outputData.size(), &outputData[0]);
		fprintf(stderr, "*** stderr (%d): %.*s\n", process->error_fd, (int)errorData.size(), &errorData[0]);

		int choice = NSRunAlertPanel(@"JavaScript Warning", @"The command ‘%@’ has been running for 15 seconds. Would you like to stop it?", @"Stop Command", @"Cancel", nil, [NSString stringWithCxxString:command]);
		if(choice == NSAlertDefaultReturn) // "Stop Command"
		{
			[self cancelCommand];
			break;
		}
	}
}

+ (HOJSShellCommand*)runShellCommand:(NSString*)aCommand withEnvironment:(const std::map<std::string, std::string>&)someEnvironment andExitHandler:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s (handler: %s)\n", [aCommand UTF8String], [[aHandler description] UTF8String]););
	HOJSShellCommand* res = [[[self alloc] initWithCommand:aCommand andEnvironment:someEnvironment] autorelease];
	res.exitHandler = aHandler;
	[res launchAndWait:aHandler == nil];
	return res;
}

- (void)dealloc
{
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););

	[self cancelCommand];
	[super dealloc];
}

// =========================
// = JavaScript Properties =
// =========================

+ (BOOL)isKeyExcludedFromWebScript:(char const*)name
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", name););
	static std::set<std::string> const PublicProperties = { "outputString", "errorString", "onreadoutput", "onreaderror" };
	return PublicProperties.find(name) == PublicProperties.end();
}

+ (NSString*)webScriptNameForKey:(char const*)name
{
	return @(name);
}

+ (BOOL)isSelectorExcludedFromWebScript:(SEL)aSelector
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", (char const*)aSelector););
	static std::set<SEL> const PublicMethods = { @selector(cancelCommand), @selector(writeToInput:), @selector(closeInput) };
	return PublicMethods.find(aSelector) == PublicMethods.end();
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

- (NSString*)outputString     { return outputData.empty() ? @"" : [NSString stringWithUTF8String:&outputData[0] length:utf8::find_safe_end(outputData.begin(), outputData.end()) - outputData.begin()]; }
- (NSString*)errorString      { return errorData.empty()  ? @"" : [NSString stringWithUTF8String:&errorData[0]  length:utf8::find_safe_end(errorData.begin(),  errorData.end())  - errorData.begin()];  }
- (void)setOutputString       { ASSERT(false); }
- (void)setErrorString        { ASSERT(false); }

- (id)onreadOutput            { return self.outputHandler; }
- (id)onreadError             { return self.errorHandler; }

- (void)setOnreadoutput:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", [[aHandler description] UTF8String]););
	self.outputHandler = aHandler;
	[outputHandler callWebScriptMethod:@"call" withArguments:@[ outputHandler, [self outputString] ]];
}

- (void)setOnreaderror:(id)aHandler
{
	D(DBF_HTMLOutput_JSShellCommand, bug("%s\n", [[aHandler description] UTF8String]););
	self.errorHandler = aHandler;
	[errorHandler callWebScriptMethod:@"call" withArguments:@[ errorHandler, [self errorString] ]];
}

- (void)finalizeForWebScript
{
	D(DBF_HTMLOutput_JSShellCommand, bug("\n"););
	[self cancelCommand];
}
@end
