#import "OakSystemCommandURLSchemeHandler.h"
#import <io/exec.h>
#import <oak/oak.h>

// This scheme handler implements a synchronous command execution bridge for WKWebView.
// JavaScript uses synchronous XMLHttpRequest to tm-system:// which blocks the JS thread
// until this handler returns. The command is executed, output collected, and returned as JSON.
// This preserves the synchronous TextMate.system(cmd, null) behavior the Git bundle depends on.

@implementation OakSystemCommandURLSchemeHandler

- (void)webView:(WKWebView*)webView startURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	// The command is passed as the request body (POST) or URL query parameter (GET)
	NSString* command = nil;

	if(NSData* body = urlSchemeTask.request.HTTPBody)
	{
		command = [[NSString alloc] initWithData:body encoding:NSUTF8StringEncoding];
	}
	else
	{
		NSURLComponents* components = [NSURLComponents componentsWithURL:urlSchemeTask.request.URL resolvingAgainstBaseURL:NO];
		for(NSURLQueryItem* item in components.queryItems)
		{
			if([item.name isEqualToString:@"cmd"])
			{
				command = item.value;
				break;
			}
		}
	}

	if(!command || command.length == 0)
	{
		NSDictionary* errorResult = @{
			@"outputString": @"",
			@"errorString": @"No command provided",
			@"status": @(-1)
		};
		[self respondToTask:urlSchemeTask withJSON:errorResult];
		return;
	}

	// Execute synchronously in the current (background) context
	// io::spawn + waitpid to collect output
	std::map<std::string, std::string> env = _environment;
	io::process_t process = io::spawn(std::vector<std::string>{ "/bin/sh", "-c", [command UTF8String] }, env);

	if(!process)
	{
		NSDictionary* errorResult = @{
			@"outputString": @"",
			@"errorString": @"Failed to spawn process",
			@"status": @(-1)
		};
		[self respondToTask:urlSchemeTask withJSON:errorResult];
		return;
	}

	// Close stdin immediately for synchronous commands
	if(process.in != -1)
	{
		close(process.in);
		process.in = -1;
	}

	// Read stdout and stderr
	std::string stdoutStr, stderrStr;
	io::exhaust_fd(process.out, &stdoutStr);
	io::exhaust_fd(process.err, &stderrStr);

	int status = 0;
	if(waitpid(process.pid, &status, 0) != process.pid)
		perror("OakSystemCommandURLSchemeHandler: waitpid");

	int exitCode = WIFEXITED(status) ? WEXITSTATUS(status) : -1;

	NSString* outStr = [NSString stringWithUTF8String:stdoutStr.c_str()] ?: @"";
	NSString* errStr = [NSString stringWithUTF8String:stderrStr.c_str()] ?: @"";

	NSDictionary* result = @{
		@"outputString": outStr,
		@"errorString": errStr,
		@"status": @(exitCode)
	};

	[self respondToTask:urlSchemeTask withJSON:result];
}

- (void)respondToTask:(id<WKURLSchemeTask>)urlSchemeTask withJSON:(NSDictionary*)dict
{
	NSData* jsonData = [NSJSONSerialization dataWithJSONObject:dict options:0 error:nil];
	if(!jsonData)
		jsonData = [@"{\"outputString\":\"\",\"errorString\":\"JSON serialization failed\",\"status\":-1}" dataUsingEncoding:NSUTF8StringEncoding];

	NSDictionary* headers = @{
		@"Content-Type": @"application/json",
		@"Content-Length": [NSString stringWithFormat:@"%lu", (unsigned long)jsonData.length],
		@"Access-Control-Allow-Origin": @"*"
	};

	NSHTTPURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:urlSchemeTask.request.URL
		statusCode:200 HTTPVersion:@"HTTP/1.1" headerFields:headers];
	[urlSchemeTask didReceiveResponse:response];
	[urlSchemeTask didReceiveData:jsonData];
	[urlSchemeTask didFinish];
}

- (void)webView:(WKWebView*)webView stopURLSchemeTask:(id<WKURLSchemeTask>)urlSchemeTask
{
	// Synchronous execution -- nothing to cancel
}
@end
