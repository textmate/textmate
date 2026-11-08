#import "HOWKScriptMessageHandler.h"
#import "HOJSBridge.h" // for HOJSBridgeDelegate protocol
#import "add_to_buffer.h"
#import <OakFoundation/NSString Additions.h>
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <text/utf8.h>
#import <ns/ns.h>
#import <io/exec.h>

@interface HOWKShellCommand : NSObject
- (instancetype)initWithCommand:(NSString*)command
                    environment:(std::map<std::string, std::string> const&)env
                      commandId:(NSInteger)commandId
                        webView:(WKWebView*)webView;
- (void)cancel;
- (void)writeToInput:(NSString*)data;
- (void)closeInput;
@end

@implementation HOWKScriptMessageHandler
{
	std::map<std::string, std::string> _environment;
	NSMutableDictionary<NSNumber*, HOWKShellCommand*>* _shellCommands;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_shellCommands = [NSMutableDictionary new];
	}
	return self;
}

- (void)setEnvironment:(const std::map<std::string, std::string>&)variables { _environment = variables; }
- (std::map<std::string, std::string> const&)environment { return _environment; }

- (void)userContentController:(WKUserContentController*)ucc didReceiveScriptMessage:(WKScriptMessage*)message
{
	if(![message.name isEqualToString:@"textmate"])
		return;

	NSString* command = message.body[@"command"];
	NSDictionary* payload = message.body[@"payload"];

	if([command isEqualToString:@"log"])
	{
		if([payload[@"level"] isEqualToString:@"error"])
		{
			static os_log_t log = os_log_create("com.macromates.JavaScript", "error");
			os_log_error(log, "%{public}@:%{public}@: %{public}@", payload[@"filename"], payload[@"lineno"], payload[@"message"]);
		}
		else
		{
			NSLog(@"JavaScript Log: %@", payload[@"message"]);
		}
	}
	else if([command isEqualToString:@"open"])
	{
		NSString* path = payload[@"path"];
		id options = payload[@"options"];
		text::range_t range = text::range_t::undefined;
		if([options isKindOfClass:[NSString class]])
		{
			NSInteger num = [(NSString*)options integerValue];
			if(num > 0)
				range = text::pos_t((int)num - 1, 0);
			else
				range = to_s((NSString*)options);
		}
		if(OakDocument* doc = [OakDocumentController.sharedInstance documentWithPath:path])
			[OakDocumentController.sharedInstance showDocument:doc andSelect:range inProject:nil bringToFront:YES];
	}
	else if([command isEqualToString:@"setBusy"])
	{
		[_delegate setBusy:[payload[@"value"] boolValue]];
	}
	else if([command isEqualToString:@"setProgress"])
	{
		[_delegate setProgress:[payload[@"value"] doubleValue]];
	}
	else if([command isEqualToString:@"system"])
	{
		NSInteger cmdId = [payload[@"id"] integerValue];
		NSString* cmdString = payload[@"command"];

		HOWKShellCommand* shellCmd = [[HOWKShellCommand alloc]
			initWithCommand:cmdString environment:_environment
			commandId:cmdId webView:_webView];
		_shellCommands[@(cmdId)] = shellCmd;
	}
	else if([command isEqualToString:@"shellCancel"])
	{
		NSNumber* cmdId = payload[@"id"];
		[_shellCommands[cmdId] cancel];
		[_shellCommands removeObjectForKey:cmdId];
	}
	else if([command isEqualToString:@"shellWrite"])
	{
		NSNumber* cmdId = payload[@"id"];
		[_shellCommands[cmdId] writeToInput:payload[@"data"]];
	}
	else if([command isEqualToString:@"shellClose"])
	{
		NSNumber* cmdId = payload[@"id"];
		[_shellCommands[cmdId] closeInput];
	}
}

- (void)cleanup
{
	for(HOWKShellCommand* cmd in _shellCommands.allValues)
		[cmd cancel];
	[_shellCommands removeAllObjects];
}

- (void)dealloc
{
	[self cleanup];
}
@end

// ==============================
// = HOWKShellCommand (async)   =
// ==============================

@interface HOWKShellCommand ()
{
	io::process_t process;
	std::string output, error;
}
@property (nonatomic) NSInteger commandId;
@property (nonatomic, weak) WKWebView* webView;
@property (nonatomic) int status;
@end

@implementation HOWKShellCommand
- (instancetype)initWithCommand:(NSString*)command environment:(std::map<std::string, std::string> const&)env
	commandId:(NSInteger)commandId webView:(WKWebView*)webView
{
	if(self = [super init])
	{
		_commandId = commandId;
		_webView = webView;

		if(process = io::spawn(std::vector<std::string>{ "/bin/sh", "-c", to_s(command) }, env))
		{
			auto group = dispatch_group_create();
			auto queue = dispatch_get_main_queue();

			[self exhaustFileDescriptor:process.out inQueue:queue group:group buffer:output isError:NO];
			[self exhaustFileDescriptor:process.err inQueue:queue group:group buffer:error isError:YES];

			dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
				int result = 0;
				if(waitpid(process.pid, &result, 0) != process.pid)
					perror("HOWKShellCommand: waitpid");
				process.pid = -1;
				dispatch_sync(queue, ^{
					self.status = WIFEXITED(result) ? WEXITSTATUS(result) : -1;
				});
			});

			dispatch_group_notify(group, dispatch_get_main_queue(), ^{
				close(process.out);
				close(process.err);
				NSString* js = [NSString stringWithFormat:@"_tmShellExit(%ld, %d)", (long)_commandId, self.status];
				[_webView evaluateJavaScript:js completionHandler:nil];
			});
		}
	}
	return self;
}

- (void)exhaustFileDescriptor:(int)fd inQueue:(dispatch_queue_t)queue group:(dispatch_group_t)group buffer:(std::string&)buf isError:(BOOL)isError
{
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		char tmp[1024];
		while(ssize_t len = read(fd, &tmp[0], sizeof(tmp)))
		{
			if(len < 0)
				break;

			char const* bytes = &tmp[0];
			dispatch_sync(queue, ^{
				auto range = add_bytes_to_utf8_buffer(buf, bytes, bytes + len, true);
				if(range.first != range.second)
				{
					NSString* str = [NSString stringWithUTF8String:std::string(range.first, range.second).c_str()];
					// Use JSON serialization for safe JS string escaping
					NSData* jsonData = [NSJSONSerialization dataWithJSONObject:@[str] options:0 error:nil];
					if(jsonData)
					{
						NSString* jsonArray = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
						// jsonArray is like ["the string"], extract just the quoted string
						NSString* jsonStr = [jsonArray substringWithRange:NSMakeRange(1, jsonArray.length - 2)];
						NSString* func = isError ? @"_tmShellError" : @"_tmShellOutput";
						NSString* js = [NSString stringWithFormat:@"%@(%ld, %@)", func, (long)self->_commandId, jsonStr];
						[self->_webView evaluateJavaScript:js completionHandler:nil];
					}
				}
			});
		}
	});
}

- (void)cancel
{
	if(process && process.pid != -1)
		kill(process.pid, SIGINT);
	[self closeInput];
}

- (void)writeToInput:(NSString*)data
{
	if(process.in != -1)
	{
		char const* bytes = [data UTF8String];
		write(process.in, bytes, strlen(bytes));
	}
}

- (void)closeInput
{
	if(process.in != -1)
	{
		close(process.in);
		process.in = -1;
	}
}

- (void)dealloc
{
	[self cancel];
}
@end
