#import "HTMLOutput.h"
#import <ns/ns.h>
#import <OakSystem/process.h>
#import <command/runner.h>
#import <oak/debug.h>

NSString* const kCommandRunnerURLScheme = @"x-txmt-command";

@interface CommandRunnerURLProtocol : NSURLProtocol
@end

@implementation CommandRunnerURLProtocol
+ (void)load
{
	[self registerClass:self];
	[WebView registerURLSchemeAsLocal:kCommandRunnerURLScheme];
}

+ (BOOL)canInitWithRequest:(NSURLRequest*)request                            { return [[[request URL] scheme] isEqualToString:kCommandRunnerURLScheme]; }
+ (NSURLRequest*)canonicalRequestForRequest:(NSURLRequest*)request           { return request; }
+ (BOOL)requestIsCacheEquivalent:(NSURLRequest*)a toRequest:(NSURLRequest*)b { return NO; }

// =============================================
// = These methods might be called in a thread =
// =============================================

- (void)startLoading
{
	NSFileHandle* fileHandle = [NSURLProtocol propertyForKey:@"fileHandle" inRequest:self.request];
	if(!fileHandle)
	{
		NSURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:[[self request] URL] statusCode:404 HTTPVersion:@"HTTP/1.1" headerFields:nil];
		[[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];
		[[self client] URLProtocolDidFinishLoading:self];
		NSLog(@"No command output for ‘%@’", [[self request] URL]);
		return;
	}

	NSURLResponse* response = [[NSURLResponse alloc] initWithURL:[[self request] URL] MIMEType:@"text/html" expectedContentLength:-1 textEncodingName:@"utf-8"];
	[[self client] URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];

	// WebView seems to stall until it has received at least 1024 bytes
	static std::string const dummy("<!--" + std::string(1017, ' ') + "-->");
	[[self client] URLProtocol:self didLoadData:[NSData dataWithBytes:dummy.data() length:dummy.size()]];

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		char buf[8192];
		int len;
		while((len = read(fileHandle.fileDescriptor, buf, sizeof(buf))) > 0)
		{
			NSData* data = [NSData dataWithBytesNoCopy:buf length:len freeWhenDone:NO];
			[[self client] URLProtocol:self didLoadData:data];
		}

		if(len == -1)
			perror("read");

		[fileHandle closeFile];
		[[self client] URLProtocolDidFinishLoading:self];
	});
}

- (void)stopLoading
{
	[[NSURLProtocol propertyForKey:@"fileHandle" inRequest:self.request] closeFile];
	if(pid_t pid = [[NSURLProtocol propertyForKey:@"processIdentifier" inRequest:self.request] intValue])
		oak::kill_process_group_in_background(pid);
}
@end

// ==============
// = Public API =
// ==============

NSURLRequest* URLRequestForCommandRunner (command::runner_ptr aRunner)
{
	struct filehandle_callback_t : command::callback_t
	{
		filehandle_callback_t (command::runner_ptr runner, NSFileHandle* fileHandle) : _runner(runner), _fileHandle(fileHandle) { _runner->add_callback(this); }
		~filehandle_callback_t ()                                                                                               { _runner->remove_callback(this); }

		void output (command::runner_ptr runner, char const* data, size_t len)
		{
			ssize_t bytesWritten = write(_fileHandle.fileDescriptor, data, len);
			if(bytesWritten == -1)
			{
				perror("write");
				delete this;
			}
		}

		void done (command::runner_ptr runner)
		{
			[_fileHandle closeFile];
			delete this;
		}

	private:
		command::runner_ptr _runner;
		NSFileHandle* _fileHandle;
	};

	NSPipe* pipe = [NSPipe pipe];
	new filehandle_callback_t(aRunner, pipe.fileHandleForWriting);

	static NSInteger LastKey = 0;

	NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@://job/%ld", kCommandRunnerURLScheme, ++LastKey]] cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:6000];
	[NSURLProtocol setProperty:pipe.fileHandleForReading forKey:@"fileHandle" inRequest:request];
	[NSURLProtocol setProperty:@(aRunner->process_id()) forKey:@"processIdentifier" inRequest:request];
	[NSURLProtocol setProperty:to_ns(aRunner->name()) forKey:@"processName" inRequest:request];
	return request;
}
