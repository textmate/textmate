#import "LSPClient.h"
#import "LSPManager.h"
#import "LSPFileWatcher.h"
#import "LSPFileWatchRegistration.h"
#import <io/FSEventsManager.h>
#import <io/environment.h>
#import <nlohmann/json.hpp>
#import <oak/debug.h>
#import <ns/ns.h>
#import <settings/settings.h>
#import <signal.h>

NSString* const LSPLogNotification = @"LSPLogNotification";
NSString* const LSPShowMessageNotification = @"LSPShowMessageNotification";
NSString* const LSPProgressNotification = @"LSPProgressNotification";
NSString* const LSPShowMessageRequestNotification = @"LSPShowMessageRequestNotification";

using json = nlohmann::json;

// JSON-RPC id can be string, integer, or null
static id jsonIdToObjC (json const& j)
{
	if(j.is_string())
		return @(j.get<std::string>().c_str());
	if(j.is_number_integer())
		return @(j.get<int64_t>());
	return nil;
}

static json objCIdToJson (id obj)
{
	if([obj isKindOfClass:NSNumber.class])
		return [obj longLongValue];
	if([obj isKindOfClass:NSString.class])
		return [obj UTF8String];
	return nullptr;
}

static NSString* fileFromURI (std::string const& uri)
{
	auto slash = uri.rfind('/');
	return slash != std::string::npos ? @(uri.substr(slash + 1).c_str()) : @(uri.c_str());
}

static NSString* fileFromParams (json const& params)
{
	if(params.contains("textDocument") && params["textDocument"].contains("uri"))
		return fileFromURI(params["textDocument"]["uri"].get<std::string>());
	return nil;
}

// Expand brace groups: "**/*.{php,inc}" → {"**/*.php", "**/*.inc"}
static NSArray<NSString*>* expandBraces (NSString* pattern)
{
	NSRange open = [pattern rangeOfString:@"{"];
	if(open.location == NSNotFound)
		return @[pattern];

	NSRange close = [pattern rangeOfString:@"}" options:0 range:NSMakeRange(open.location, pattern.length - open.location)];
	if(close.location == NSNotFound)
		return @[pattern];

	NSString* prefix = [pattern substringToIndex:open.location];
	NSString* suffix = [pattern substringFromIndex:close.location + 1];
	NSString* inner  = [pattern substringWithRange:NSMakeRange(open.location + 1, close.location - open.location - 1)];

	NSMutableArray<NSString*>* result = [NSMutableArray new];
	for(NSString* alt in [inner componentsSeparatedByString:@","])
	{
		NSString* expanded = [NSString stringWithFormat:@"%@%@%@", prefix, [alt stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceCharacterSet], suffix];
		[result addObjectsFromArray:expandBraces(expanded)];
	}
	return result;
}

// Extract extension from a simple glob like "**/*.php" or "*.php"
static NSString* extensionFromGlob (NSString* glob)
{
	// Match patterns like "**/*.ext" or "*.ext"
	if([glob hasPrefix:@"**/*."] || [glob hasPrefix:@"*."])
	{
		NSRange lastDot = [glob rangeOfString:@"." options:NSBackwardsSearch];
		if(lastDot.location != NSNotFound)
		{
			NSString* ext = [glob substringFromIndex:lastDot.location];
			// Only accept simple extensions (no wildcards in the extension part)
			if([ext rangeOfString:@"*"].location == NSNotFound && [ext rangeOfString:@"?"].location == NSNotFound)
				return ext.lowercaseString;
		}
	}
	return nil;
}

static void extractExtensionsFromGlob (NSString* pattern, NSMutableSet<NSString*>* extensions, NSMutableSet<NSString*>* exactNames, BOOL* watchAll)
{
	NSArray<NSString*>* expanded = expandBraces(pattern);
	for(NSString* glob in expanded)
	{
		// Catch-all patterns like **/* or **/*.* match every file
		if([glob isEqualToString:@"**/*"] || [glob isEqualToString:@"**/*.*"] || [glob isEqualToString:@"*"])
		{
			if(watchAll) *watchAll = YES;
			continue;
		}

		NSString* ext = extensionFromGlob(glob);
		if(ext)
		{
			[extensions addObject:ext];
		}
		else
		{
			// Unrecognized pattern — if it has no path separators or wildcards, treat as exact filename
			if([glob rangeOfString:@"*"].location == NSNotFound && [glob rangeOfString:@"?"].location == NSNotFound)
			{
				NSString* name = glob.lastPathComponent;
				[exactNames addObject:name];
				NSLog(@"[LSP] File watch: unrecognized glob '%@', using exact filename match for '%@'", glob, name);
			}
			else
			{
				NSLog(@"[LSP] File watch: unsupported glob pattern '%@', skipping", glob);
			}
		}
	}
}

@interface LSPClient ()
{
	NSTask* _task;
	NSSet<NSString*>* _languageQueries;
	NSPipe* _stdinPipe;
	NSPipe* _stdoutPipe;
	NSPipe* _stderrPipe;
	dispatch_queue_t _readQueue;
	int _nextRequestId;
	NSString* _serverName;
	BOOL _initialized;
	int _initializeRequestId;
	BOOL _indexing;
	BOOL _documentFormattingProvider;
	BOOL _documentRangeFormattingProvider;
	BOOL _completionResolveProvider;
	BOOL _renameProvider;
	BOOL _codeActionProvider;
	BOOL _codeActionResolveProvider;
	BOOL _documentDiagnosticProvider;
	BOOL _diagnosticInterFileDependencies;
	NSString* _diagnosticIdentifier;
	NSMutableSet<NSString*>* _diagnosticOpenURIs;
	NSMutableDictionary<NSString*, NSNumber*>* _diagnosticGenerations;
	NSMutableDictionary<NSString*, NSNumber*>* _diagnosticInFlightIds;
	NSMutableSet<NSString*>* _diagnosticPendingURIs;
	BOOL _diagnosticRefreshAll;
	NSTimer* _diagnosticDebounceTimer;
	NSUInteger _nextDiagnosticGeneration;
	NSArray<NSString*>* _executeCommands;
	NSString* _workingDirectory;
	NSString* _initOptionsJSON;
	NSMutableDictionary<NSNumber*, void(^)(id)>* _responseCallbacks;
	NSMutableDictionary<NSNumber*, NSString*>* _requestMethods;

	// File watching
	NSMutableDictionary<NSString*, LSPFileWatchRegistration*>* _fileWatchRegistrations;
	LSPFileWatcher* _fileWatcher;
	id _fsEventsObserver;
	dispatch_queue_t _scanQueue;
	NSTimer* _debounceTimer;
	NSMutableDictionary<NSString*, NSDictionary*>* _pendingChanges; // URI → latest change
	NSMutableSet* _indexingProgressTokens; // tokens for indexing-related $/progress
	NSString* _logPrefix;
}
- (void)openDocument:(OakDocument*)document languageId:(NSString*)languageId retryCount:(int)retryCount;
- (void)scheduleDiagnosticsForURI:(NSString*)uri;
- (void)scheduleAllDiagnostics;
- (void)fireDiagnosticPull;
- (void)resetDiagnosticState;
- (void)armDiagnosticDebounceTimer;
@end

@implementation LSPClient

- (BOOL)running
{
	return _task.isRunning;
}

- (BOOL)indexing
{
	return _indexing;
}

- (void)setIndexing:(BOOL)flag
{
	if(_indexing == flag)
		return;
	_indexing = flag;
	[self postLog:[NSString stringWithFormat:@"indexing state → %@", flag ? @"YES" : @"NO"] source:@"event"];
	[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:_delegate];
}

- (instancetype)initWithCommand:(NSString*)command arguments:(NSArray<NSString*>*)arguments workingDirectory:(NSString*)workingDirectory initOptions:(NSString*)initOptionsJSON
{
	if(self = [super init])
	{
		_serverName = command.lastPathComponent;
		_logPrefix = [NSString stringWithFormat:@"LSP:%@", _serverName];
		_workingDirectory = workingDirectory;
		_initOptionsJSON = initOptionsJSON;
		_readQueue = dispatch_queue_create("com.macromates.lsp.read", DISPATCH_QUEUE_SERIAL);
		_nextRequestId = 1;
		_initialized = NO;
		_responseCallbacks      = [NSMutableDictionary new];
		_requestMethods         = [NSMutableDictionary new];
		_indexingProgressTokens = [NSMutableSet new];
		_diagnosticOpenURIs     = [NSMutableSet new];
		_diagnosticGenerations  = [NSMutableDictionary new];
		_diagnosticInFlightIds  = [NSMutableDictionary new];
		_diagnosticPendingURIs  = [NSMutableSet new];

		_stdinPipe  = [NSPipe pipe];
		_stdoutPipe = [NSPipe pipe];
		_stderrPipe = [NSPipe pipe];

		// Build environment first — we need PATH for executable resolution
		NSMutableDictionary* env = [NSProcessInfo.processInfo.environment mutableCopy];
		auto const& tmEnv = oak::basic_environment();
		auto it = tmEnv.find("PATH");
		if(it != tmEnv.end())
			env[@"PATH"] = to_ns(it->second);

		// Resolve bare command names via PATH
		NSString* resolvedCommand = command;
		if(![command hasPrefix:@"/"])
		{
			NSString* path = env[@"PATH"] ?: @"/usr/bin:/bin:/usr/sbin:/sbin";
			for(NSString* dir in [path componentsSeparatedByString:@":"])
			{
				NSString* candidate = [dir stringByAppendingPathComponent:command];
				if([[NSFileManager defaultManager] isExecutableFileAtPath:candidate])
				{
					resolvedCommand = candidate;
					break;
				}
			}
		}

		_task = [[NSTask alloc] init];
		_task.executableURL      = [NSURL fileURLWithPath:resolvedCommand];
		_task.arguments          = arguments ?: @[];
		_task.standardInput      = _stdinPipe;
		_task.standardOutput     = _stdoutPipe;
		_task.standardError      = _stderrPipe;
		_task.currentDirectoryURL = [NSURL fileURLWithPath:workingDirectory];
		_task.environment = env;

		__weak LSPClient* weakSelf = self;
		_task.terminationHandler = ^(NSTask* task){
			dispatch_async(dispatch_get_main_queue(), ^{
				LSPClient* strongSelf = weakSelf;
				if(!strongSelf)
					return;
				[strongSelf postLog:[NSString stringWithFormat:@"Server terminated with status %d", task.terminationStatus] source:task.terminationStatus == 0 ? @"event" : @"error"];
				if(task.terminationStatus != 0)
				{
					[[NSNotificationCenter defaultCenter] postNotificationName:LSPShowMessageNotification object:strongSelf userInfo:@{
						@"type": @1,
						@"message": [NSString stringWithFormat:@"LSP server crashed (exit %d)", task.terminationStatus]
					}];
				}
				strongSelf->_initialized = NO;
				strongSelf->_indexing = NO;
				[strongSelf->_indexingProgressTokens removeAllObjects];
				[strongSelf teardownFileWatcher];
				[strongSelf->_fileWatchRegistrations removeAllObjects];
				[strongSelf resetDiagnosticState];
				[strongSelf cancelPendingCallbacks];
				if([strongSelf->_delegate respondsToSelector:@selector(lspClientDidTerminate:)])
					[strongSelf->_delegate lspClientDidTerminate:strongSelf];
			});
		};

		NSError* error = nil;
		if(![_task launchAndReturnError:&error])
		{
			[self postLog:[NSString stringWithFormat:@"Failed to launch server: %@", error.localizedDescription] source:@"error"];
			return nil;
		}

		_logPrefix = [NSString stringWithFormat:@"LSP:%@/%d", _serverName, _task.processIdentifier];
		[self postLog:[NSString stringWithFormat:@"Server launched: %@ %@", command, [arguments componentsJoinedByString:@" "]] source:@"event"];

		[self startReadLoop];
		[self startStderrLoop];
		[self sendInitialize];
	}
	return self;
}

// MARK: - JSON-RPC framing

- (void)cancelPendingCallbacks
{
	NSDictionary<NSNumber*, void(^)(id)>* callbacks = [_responseCallbacks copy];
	[_responseCallbacks removeAllObjects];
	[_requestMethods removeAllObjects];
	for(NSNumber* key in callbacks)
	{
		void(^callback)(id) = callbacks[key];
		if(callback)
			callback(nil);
	}
}

- (void)sendMessage:(json const&)message
{
	if(!_task.isRunning)
	{
		[self postLog:@"Message dropped — server not running" source:@"error"];
		return;
	}

	std::string body = message.dump();
	NSString* header = [NSString stringWithFormat:@"Content-Length: %lu\r\n\r\n", (unsigned long)body.size()];

	NSMutableData* data = [NSMutableData dataWithBytes:header.UTF8String length:strlen(header.UTF8String)];
	[data appendBytes:body.c_str() length:body.size()];

	@try {
		[_stdinPipe.fileHandleForWriting writeData:data];
	} @catch(NSException* e) {
		[self postLog:[NSString stringWithFormat:@"Write failed: %@", e.reason] source:@"error"];
	}
}

- (void)sendRequest:(NSString*)method params:(json)params
{
	int reqId = _nextRequestId++;
	_requestMethods[@(reqId)] = method;
	json msg = {
		{"jsonrpc", "2.0"},
		{"id",      reqId},
		{"method",  method.UTF8String},
		{"params",  params}
	};
	if(params.is_null())
		msg.erase("params");
	[self postLog:[NSString stringWithFormat:@"%@ (id=%d)", method, reqId] source:@"request"];
	[self sendMessage:msg];
}

- (void)sendNotification:(NSString*)method params:(json)params
{
	NSMutableString* logMsg = [NSMutableString stringWithString:method];
	NSString* file = fileFromParams(params);
	if(file)
		[logMsg appendFormat:@"  %@", file];

	json msg = {
		{"jsonrpc", "2.0"},
		{"method",  method.UTF8String},
		{"params",  params}
	};
	if(params.is_null())
		msg.erase("params");
	[self postLog:logMsg source:@"notify"];
	[self sendMessage:msg];
}

// MARK: - Read loop

- (void)startReadLoop
{
	NSFileHandle* handle = _stdoutPipe.fileHandleForReading;
	NSString* logPrefix = _logPrefix;
	__weak LSPClient* weakSelf = self;

	// Helper: post to log panel if client is alive, else fall back to NSLog
	void (^logOrFallback)(NSString*, NSString*) = ^(NSString* message, NSString* source){
		dispatch_async(dispatch_get_main_queue(), ^{
			LSPClient* strongSelf = weakSelf;
			if(strongSelf)
				[strongSelf postLog:message source:source];
			else
				NSLog(@"[%@] %@", logPrefix, message);
		});
	};

	dispatch_async(_readQueue, ^{
		NSMutableData* buffer = [NSMutableData data];

		while(true)
		{
			NSInteger contentLength = -1;
			while(true)
			{
				NSRange headerEnd = [buffer rangeOfData:[@"\r\n\r\n" dataUsingEncoding:NSUTF8StringEncoding] options:0 range:NSMakeRange(0, buffer.length)];
				if(headerEnd.location != NSNotFound)
				{
					NSString* headers = [[NSString alloc] initWithData:[buffer subdataWithRange:NSMakeRange(0, headerEnd.location)] encoding:NSUTF8StringEncoding];
					for(NSString* line in [headers componentsSeparatedByString:@"\r\n"])
					{
						if([line hasPrefix:@"Content-Length: "])
							contentLength = [line substringFromIndex:16].integerValue;
					}

					[buffer replaceBytesInRange:NSMakeRange(0, headerEnd.location + headerEnd.length) withBytes:NULL length:0];
					break;
				}

				NSData* chunk = [handle availableData];
				if(chunk.length == 0)
				{
					logOrFallback(@"Server stdout closed", @"error");
					return;
				}
				[buffer appendData:chunk];
			}

			if(contentLength < 0)
			{
				logOrFallback(@"Missing Content-Length header", @"error");
				continue;
			}

			while((NSInteger)buffer.length < contentLength)
			{
				NSData* chunk = [handle availableData];
				if(chunk.length == 0)
				{
					logOrFallback(@"Server stdout closed mid-message", @"error");
					return;
				}
				[buffer appendData:chunk];
			}

			NSData* bodyData = [buffer subdataWithRange:NSMakeRange(0, contentLength)];
			[buffer replaceBytesInRange:NSMakeRange(0, contentLength) withBytes:NULL length:0];

			try {
				json msg = json::parse((const char*)bodyData.bytes, (const char*)bodyData.bytes + bodyData.length);
				dispatch_async(dispatch_get_main_queue(), ^{
					LSPClient* strongSelf = weakSelf;
					if(!strongSelf)
						return;
					try {
						[strongSelf handleMessage:msg];
					} catch(std::exception const& e) {
						[strongSelf postLog:[NSString stringWithFormat:@"handleMessage exception: %s", e.what()] source:@"error"];
					}
				});
			} catch(std::exception const& e) {
				logOrFallback([NSString stringWithFormat:@"JSON parse error: %s", e.what()], @"error");
			}
		}
	});
}

- (void)startStderrLoop
{
	NSString* logPrefix = _logPrefix;
	__weak LSPClient* weakSelf = self;
	_stderrPipe.fileHandleForReading.readabilityHandler = ^(NSFileHandle* handle){
		NSData* data = handle.availableData;
		if(data.length > 0)
		{
			NSString* message = [NSString stringWithFormat:@"[stderr] %@", [[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet]];
			dispatch_async(dispatch_get_main_queue(), ^{
				LSPClient* strongSelf = weakSelf;
				if(strongSelf)
					[strongSelf postLog:message source:@"error"];
				else
					NSLog(@"[%@] %@", logPrefix, message);
			});
		}
	};
}

// MARK: - Message dispatch

- (NSString*)logPrefix
{
	return _logPrefix;
}

- (void)postLog:(NSString*)message source:(NSString*)source
{
	NSLog(@"[%@] %@", self.logPrefix, message);
	[[NSNotificationCenter defaultCenter] postNotificationName:LSPLogNotification object:self userInfo:@{
		@"message": message,
		@"source":  source,
		@"server":  _serverName ?: @"?"
	}];
}

- (void)handleShowMessage:(json const&)params
{
	if(!params.contains("type") || !params.contains("message"))
		return;

	try {
		int type = params["type"].get<int>();
		std::string message = params["message"].get<std::string>();

		[[NSNotificationCenter defaultCenter] postNotificationName:LSPShowMessageNotification object:self userInfo:@{
			@"type": @(type),
			@"message": [NSString stringWithUTF8String:message.c_str()]
		}];
	} catch(std::exception const& e) {
		[self postLog:[NSString stringWithFormat:@"Failed to parse showMessage: %s", e.what()] source:@"error"];
	}
}

- (void)handleLogMessage:(json const&)params
{
	if(!params.contains("type") || !params.contains("message"))
		return;

	try {
		int type = params["type"].get<int>();
		std::string message = params["message"].get<std::string>();

		NSString* nsMessage = [NSString stringWithUTF8String:message.c_str()];
		NSLog(@"[%@] [Server] %@", self.logPrefix, nsMessage);
		[[NSNotificationCenter defaultCenter] postNotificationName:LSPLogNotification object:self userInfo:@{@"message": nsMessage, @"type": @(type), @"source": @"server", @"server": _serverName ?: @"?"}];
	} catch(std::exception const& e) {
		[self postLog:[NSString stringWithFormat:@"Failed to parse logMessage: %s", e.what()] source:@"error"];
	}
}

- (void)handleProgress:(json const&)params
{
	try {
		id token = nil;
		if(params.contains("token"))
		{
			if(params["token"].is_string())
				token = [NSString stringWithUTF8String:params["token"].get<std::string>().c_str()];
			else if(params["token"].is_number())
				token = @(params["token"].get<int>());
		}

		if(!params.contains("value") || !params["value"].contains("kind"))
			return;

		auto const& value = params["value"];
		std::string kind = value["kind"].get<std::string>();

		NSMutableDictionary* info = [NSMutableDictionary dictionary];
		if(token) info[@"token"] = token;
		info[@"kind"] = [NSString stringWithUTF8String:kind.c_str()];

		if(value.contains("title"))
			info[@"title"] = [NSString stringWithUTF8String:value["title"].get<std::string>().c_str()];
		if(value.contains("message"))
			info[@"message"] = [NSString stringWithUTF8String:value["message"].get<std::string>().c_str()];
		if(value.contains("percentage"))
			info[@"percentage"] = @(value["percentage"].get<int>());

		[[NSNotificationCenter defaultCenter] postNotificationName:LSPProgressNotification object:self userInfo:info];
	} catch(std::exception const& e) {
		[self postLog:[NSString stringWithFormat:@"Failed to parse progress: %s", e.what()] source:@"error"];
	}
}

- (void)handleMessage:(json const&)msg
{
	if(msg.contains("method"))
	{
		std::string method = msg["method"].get<std::string>();

		if(msg.contains("id"))
		{
			json requestId = msg["id"];
			id objcRequestId = jsonIdToObjC(requestId);
			[self postLog:[NSString stringWithFormat:@"%s (id=%s)", method.c_str(), requestId.dump().c_str()] source:@"event"];

			if(method == "workspace/applyEdit")
			{
				NSDictionary* edit = nil;
				if(msg["params"].contains("edit"))
					edit = [self convertJSON:msg["params"]["edit"]];

				if(edit && [_delegate respondsToSelector:@selector(lspClient:didReceiveApplyEditRequest:requestId:)])
				{
					[_delegate lspClient:self didReceiveApplyEditRequest:edit requestId:objcRequestId];
				}
				else
				{
					json response = {
						{"jsonrpc", "2.0"},
						{"id",      requestId},
						{"result",  {{"applied", false}, {"failureReason", "Not supported"}}}
					};
					[self sendMessage:response];
				}
			}
			else if(method == "client/registerCapability")
			{
				[self handleRegisterCapability:msg["params"]];
				json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
				[self sendMessage:response];
			}
			else if(method == "client/unregisterCapability")
			{
				[self handleUnregisterCapability:msg["params"]];
				json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
				[self sendMessage:response];
			}
			else if(method == "window/workDoneProgress/create")
			{
				json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
				[self sendMessage:response];
			}
			else if(method == "workspace/diagnostic/refresh")
			{
				json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
				[self sendMessage:response];
				[self scheduleAllDiagnostics];
			}
			else if(method == "window/showMessageRequest")
			{
				std::string message = msg["params"].contains("message") ? msg["params"]["message"].get<std::string>() : "";
				int type = msg["params"].contains("type") ? msg["params"]["type"].get<int>() : 3;

				NSMutableArray<NSDictionary*>* actions = [NSMutableArray new];
				NSMutableArray<NSString*>* actionTitles = [NSMutableArray new];
				if(msg["params"].contains("actions"))
				{
					for(auto const& action : msg["params"]["actions"])
					{
						[actions addObject:[self convertJSON:action]];
						[actionTitles addObject:to_ns(action["title"].get<std::string>())];
					}
				}

				if(actions.count == 0)
				{
					json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", nullptr}};
					[self sendMessage:response];
					return;
				}

				[[NSNotificationCenter defaultCenter] postNotificationName:LSPShowMessageRequestNotification
					object:self
					userInfo:@{
						@"type": @(type),
						@"message": to_ns(message),
						@"actions": actions,
						@"actionTitles": actionTitles,
						@"requestId": objcRequestId ?: [NSNull null]
					}];
			}
			else if(method == "window/showDocument")
			{
				NSString* uri = to_ns(msg["params"]["uri"].get<std::string>());
				bool external = msg["params"].value("external", false);
				bool takeFocus = msg["params"].value("takeFocus", true);

				bool success = false;
				NSURL* url = [NSURL URLWithString:uri];

				if(!url)
				{
					json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", {{"success", false}}}};
					[self sendMessage:response];
					return;
				}

				if(external || !url.isFileURL)
				{
					success = [[NSWorkspace sharedWorkspace] openURL:url];
				}
				else
				{
					if([_delegate respondsToSelector:@selector(lspClient:didRequestShowDocument:takeFocus:)])
					{
						[_delegate lspClient:self didRequestShowDocument:url.path takeFocus:takeFocus];
						success = true;
					}
				}

				json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", {{"success", success}}}};
				[self sendMessage:response];
			}
			else if(method == "workspace/configuration")
			{
				BOOL handled = NO;

				// Let delegate handle first (e.g. CopilotManager returns specific config)
				if([_delegate respondsToSelector:@selector(lspClient:handleServerRequest:params:)])
				{
					NSDictionary* params = msg.contains("params") ? [self convertJSON:msg["params"]] : @{};
					id delegateResult = [_delegate lspClient:self handleServerRequest:to_ns(method) params:params];
					if(delegateResult)
					{
						json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", [self convertToJSON:delegateResult]}};
						[self sendMessage:response];
						handled = YES;
					}
				}

				if(!handled)
				{
					// Return initializationOptions as configuration settings
					// LSP servers like Intelephense request settings via workspace/configuration
					// and expect the same keys they accept in initializationOptions
					json initOpts = _initOptionsJSON.length ? json::parse(_initOptionsJSON.UTF8String, nullptr, false) : json::object();
					json result = json::array();
					if(msg.contains("params") && msg["params"].contains("items"))
					{
						for(size_t i = 0; i < msg["params"]["items"].size(); ++i)
							result.push_back(initOpts);
					}
					json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", result}};
					[self sendMessage:response];
				}
			}
			else
			{
				if([_delegate respondsToSelector:@selector(lspClient:handleServerRequest:params:)])
				{
					NSDictionary* params = msg.contains("params") ? [self convertJSON:msg["params"]] : @{};
					id result = [_delegate lspClient:self handleServerRequest:to_ns(method) params:params];
					json response = {
						{"jsonrpc", "2.0"},
						{"id",      requestId},
						{"result",  result ? [self convertToJSON:result] : json::object()}
					};
					[self sendMessage:response];
				}
				else
				{
					json response = {
						{"jsonrpc", "2.0"},
						{"id",      requestId},
						{"result",  json::object()}
					};
					[self sendMessage:response];
				}
			}
		}
		else if(method == "textDocument/publishDiagnostics")
		{
			if(!msg.contains("params") || !msg["params"].contains("uri") || !msg["params"].contains("diagnostics"))
			{
				[self postLog:@"publishDiagnostics missing params/uri/diagnostics" source:@"error"];
				return;
			}
			auto const& diags = msg["params"]["diagnostics"];
			NSString* file = fileFromURI(msg["params"]["uri"].get<std::string>());
			[self postLog:[NSString stringWithFormat:@"publishDiagnostics  %@  %lu items", file, (unsigned long)diags.size()] source:@"event"];
			[self handleDiagnostics:msg["params"]];
		}
		else if(method == "window/showMessage")
		{
			std::string text = msg["params"].contains("message") ? msg["params"]["message"].get<std::string>() : "";
			int type = msg["params"].contains("type") ? msg["params"]["type"].get<int>() : 3;
			static char const* const typeNames[] = { "?", "Error", "Warning", "Info", "Log" };
			[self postLog:[NSString stringWithFormat:@"showMessage [%s] %s", typeNames[(type > 0 && type < 5) ? type : 0], text.c_str()] source:@"event"];
			[self handleShowMessage:msg["params"]];
		}
		else if(method == "window/logMessage")
		{
			[self handleLogMessage:msg["params"]];
		}
		else if(method == "$/progress")
		{
			auto const& val = msg["params"]["value"];
			std::string kind = val.contains("kind") ? val["kind"].get<std::string>() : "?";
			std::string title = val.contains("title") ? val["title"].get<std::string>() : "";
			std::string pmsg = val.contains("message") ? val["message"].get<std::string>() : "";
			NSMutableString* logMsg = [NSMutableString stringWithFormat:@"progress/%s", kind.c_str()];
			if(!title.empty())
				[logMsg appendFormat:@"  %s", title.c_str()];
			if(!pmsg.empty())
				[logMsg appendFormat:@": %s", pmsg.c_str()];
			if(val.contains("percentage"))
				[logMsg appendFormat:@" (%d%%)", val["percentage"].get<int>()];
			[self postLog:logMsg source:@"event"];
			[self handleProgress:msg["params"]];

			// Track indexing-related progress by token
			id token = nil;
			if(msg["params"].contains("token"))
			{
				auto const& t = msg["params"]["token"];
				if(t.is_string())
					token = @(t.get<std::string>().c_str());
				else if(t.is_number())
					token = @(t.get<int>());
			}

			if(kind == "begin" && token)
			{
				NSString* lowerTitle = [NSString stringWithUTF8String:title.c_str()].lowercaseString;
				if([lowerTitle isEqualToString:@"indexing"] || [lowerTitle hasPrefix:@"indexing "]
				|| [lowerTitle isEqualToString:@"loading packages"] || [lowerTitle isEqualToString:@"loading workspace"]
				|| [lowerTitle hasPrefix:@"initializ"])
				{
					[_indexingProgressTokens addObject:token];
					[self setIndexing:YES];
				}
			}
			else if(kind == "end" && token)
			{
				if([_indexingProgressTokens containsObject:token])
				{
					[_indexingProgressTokens removeObject:token];
					if(_indexingProgressTokens.count == 0)
						[self setIndexing:NO];
				}
			}
		}
		else if(method == "indexingStarted")
		{
			[self postLog:@"indexingStarted" source:@"event"];
			[_indexingProgressTokens removeAllObjects];
			[self setIndexing:YES];
		}
		else if(method == "indexingEnded")
		{
			[self postLog:@"indexingEnded" source:@"event"];
			[_indexingProgressTokens removeAllObjects];
			[self setIndexing:NO];
		}
		else
		{
			[self postLog:[NSString stringWithFormat:@"%s", method.c_str()] source:@"event"];
			if([_delegate respondsToSelector:@selector(lspClient:didReceiveNotification:params:)])
			{
				NSDictionary* params = msg.contains("params") ? [self convertJSON:msg["params"]] : @{};
				[_delegate lspClient:self didReceiveNotification:to_ns(method) params:params];
			}
		}
	}
	else if(msg.contains("id"))
	{
		if(!msg["id"].is_number_integer())
		{
			[self postLog:[NSString stringWithFormat:@"Ignoring response with non-integer id: %s", msg["id"].dump().c_str()] source:@"error"];
			return;
		}
		int reqId = msg["id"].get<int>();
		NSString* method = _requestMethods[@(reqId)];
		[_requestMethods removeObjectForKey:@(reqId)];

		if(msg.contains("error"))
		{
			auto const& err = msg["error"];
			int errCode = err.value("code", 0);
			[self postLog:[NSString stringWithFormat:@"%@ (id=%d) error %d: %s",
				method ?: @"?", reqId, errCode, err.value("message", std::string("unknown")).c_str()] source:@"error"];

			NSNumber* key = @(reqId);
			void(^callback)(id) = _responseCallbacks[key];
			if(callback)
			{
				[_responseCallbacks removeObjectForKey:key];
				callback(nil);
			}

			// RequestCancelled (-32800), ContentModified (-32801), ServerCancelled (-32802) are expected, don't toast
			if(errCode != -32800 && errCode != -32801 && errCode != -32802)
			{
				std::string errMsg = err.value("message", std::string("unknown error"));
				[[NSNotificationCenter defaultCenter] postNotificationName:LSPShowMessageNotification object:self userInfo:@{
					@"type": @1,
					@"message": @(errMsg.c_str())
				}];
			}
		}
		else if(!_initialized && reqId == _initializeRequestId && msg.contains("result"))
		{
			_initialized = YES;

			NSMutableString* logMsg = [NSMutableString stringWithFormat:@"%@ (id=%d) initialized", method ?: @"initialize", reqId];
			[self sendNotification:@"initialized" params:json::object()];
			json settings = _initOptionsJSON.length ? json::parse(_initOptionsJSON.UTF8String, nullptr, false) : json::object();
			[self sendNotification:@"workspace/didChangeConfiguration" params:{{"settings", settings}}];

			if([_delegate respondsToSelector:@selector(lspClientDidInitialize:)])
				[_delegate lspClientDidInitialize:self];

			if(msg["result"].contains("capabilities"))
			{
				auto const& caps = msg["result"]["capabilities"];
				NSMutableSet* queries = [NSMutableSet set];
				NSDictionary<NSString*, NSString*>* providers = @{ @"textDocument/definition": @"definitionProvider", @"textDocument/references": @"referencesProvider",
					@"textDocument/documentSymbol": @"documentSymbolProvider", @"workspace/symbol": @"workspaceSymbolProvider", @"textDocument/hover": @"hoverProvider" };
				BOOL utf16 = !caps.contains("positionEncoding") || caps["positionEncoding"] == "utf-16";
				for(NSString* method in providers) {
					auto found = caps.find(providers[method].UTF8String);
					if(utf16 && found != caps.end() && (found->is_object() || (found->is_boolean() && found->get<bool>()))) [queries addObject:method];
				}
				_languageQueries = queries;
				if(caps.contains("diagnosticProvider") && caps["diagnosticProvider"].is_object())
				{
					auto const& diagnostic = caps["diagnosticProvider"];
					_documentDiagnosticProvider = YES;
					_diagnosticInterFileDependencies = diagnostic.value("interFileDependencies", false);
					if(diagnostic.contains("identifier") && diagnostic["identifier"].is_string())
						_diagnosticIdentifier = to_ns(diagnostic["identifier"].get<std::string>());
				}
				_documentFormattingProvider = caps.contains("documentFormattingProvider") && !caps["documentFormattingProvider"].is_null() && (caps["documentFormattingProvider"].is_boolean() ? caps["documentFormattingProvider"].get<bool>() : true);
				_documentRangeFormattingProvider = caps.contains("documentRangeFormattingProvider") && !caps["documentRangeFormattingProvider"].is_null() && (caps["documentRangeFormattingProvider"].is_boolean() ? caps["documentRangeFormattingProvider"].get<bool>() : true);
				if(caps.contains("completionProvider") && caps["completionProvider"].contains("resolveProvider"))
					_completionResolveProvider = caps["completionProvider"]["resolveProvider"].get<bool>();
				_renameProvider = caps.contains("renameProvider") && !caps["renameProvider"].is_null() && (caps["renameProvider"].is_boolean() ? caps["renameProvider"].get<bool>() : true);
				_codeActionProvider = caps.contains("codeActionProvider") && !caps["codeActionProvider"].is_null() && (caps["codeActionProvider"].is_boolean() ? caps["codeActionProvider"].get<bool>() : true);
				if(caps.contains("codeActionProvider") && !caps["codeActionProvider"].is_boolean() && caps["codeActionProvider"].is_object() && caps["codeActionProvider"].contains("resolveProvider"))
					_codeActionResolveProvider = caps["codeActionProvider"]["resolveProvider"].get<bool>();
				if(caps.contains("executeCommandProvider") && caps["executeCommandProvider"].is_object() && caps["executeCommandProvider"].contains("commands") && caps["executeCommandProvider"]["commands"].is_array())
				{
					NSMutableArray* cmds = [NSMutableArray new];
					for(auto const& cmd : caps["executeCommandProvider"]["commands"])
					{
						if(cmd.is_string())
							[cmds addObject:@(cmd.get<std::string>().c_str())];
					}
					_executeCommands = [cmds copy];
				}
				[logMsg appendFormat:@"  formatting=%d rangeFormatting=%d completionResolve=%d rename=%d codeAction=%d execCmds=%lu pullDiagnostics=%d", _documentFormattingProvider, _documentRangeFormattingProvider, _completionResolveProvider, _renameProvider, _codeActionProvider, (unsigned long)_executeCommands.count, _documentDiagnosticProvider];
			}
			[self postLog:logMsg source:@"response"];
		}
		else if(msg.contains("result"))
		{
			NSMutableString* logMsg = [NSMutableString stringWithFormat:@"%@ (id=%d)", method ?: @"?", reqId];

			// Summarize result based on method
			auto const& result = msg["result"];
			if([method isEqualToString:@"textDocument/completion"])
			{
				size_t count = result.is_array() ? result.size() : (result.contains("items") ? result["items"].size() : 0);
				[logMsg appendFormat:@"  %lu items", (unsigned long)count];
			}
			else if([method isEqualToString:@"completionItem/resolve"])
			{
				if(result.contains("label"))
					[logMsg appendFormat:@"  \"%s\"", result["label"].get<std::string>().c_str()];
				if(result.contains("detail"))
					[logMsg appendFormat:@"  %s", result["detail"].get<std::string>().c_str()];
			}
			else if([method isEqualToString:@"textDocument/definition"])
			{
				size_t count = result.is_array() ? result.size() : (result.is_object() ? 1 : 0);
				[logMsg appendFormat:@"  %lu location%s", (unsigned long)count, count == 1 ? "" : "s"];
			}
			else if([method isEqualToString:@"textDocument/hover"])
			{
				[logMsg appendFormat:@"  %s", result.is_null() ? "empty" : "content"];
			}
			else if([method isEqualToString:@"textDocument/references"])
			{
				size_t count = result.is_array() ? result.size() : 0;
				[logMsg appendFormat:@"  %lu ref%s", (unsigned long)count, count == 1 ? "" : "s"];
			}
			else if([method isEqualToString:@"textDocument/formatting"] || [method isEqualToString:@"textDocument/rangeFormatting"])
			{
				size_t count = result.is_array() ? result.size() : 0;
				[logMsg appendFormat:@"  %lu edit%s", (unsigned long)count, count == 1 ? "" : "s"];
			}
			else if([method isEqualToString:@"textDocument/prepareRename"])
			{
				[logMsg appendFormat:@"  %s", result.is_null() ? "not renameable" : "renameable"];
			}
			else if([method isEqualToString:@"textDocument/rename"])
			{
				size_t count = 0;
				if(result.contains("changes"))
				{
					for(auto& [uri, edits] : result["changes"].items())
						count += edits.size();
				}
				else if(result.contains("documentChanges"))
					count = result["documentChanges"].size();
				[logMsg appendFormat:@"  %lu edit%s", (unsigned long)count, count == 1 ? "" : "s"];
			}
			else if([method isEqualToString:@"textDocument/codeAction"])
			{
				size_t count = result.is_array() ? result.size() : 0;
				[logMsg appendFormat:@"  %lu action%s", (unsigned long)count, count == 1 ? "" : "s"];
			}
			else if([method isEqualToString:@"codeAction/resolve"])
			{
				if(result.contains("title"))
					[logMsg appendFormat:@"  \"%s\"", result["title"].get<std::string>().c_str()];
			}
			else if([method isEqualToString:@"workspace/executeCommand"])
			{
				[logMsg appendString:@"  done"];
			}

			[self postLog:logMsg source:@"response"];

			NSNumber* key = @(reqId);
			void(^callback)(id) = _responseCallbacks[key];
			if(callback)
			{
				[_responseCallbacks removeObjectForKey:key];
				id resultObj = [self convertJSON:result];
				callback(resultObj);
			}
		}
	}
}

- (void)handleDiagnostics:(json const&)params
{
	std::string uriStr = params["uri"].get<std::string>();
	auto const& diagnostics = params["diagnostics"];

	[self postLog:[NSString stringWithFormat:@"Diagnostics for %s: %lu items", uriStr.c_str(), (unsigned long)diagnostics.size()] source:@"event"];

	NSMutableArray<NSDictionary*>* results = [NSMutableArray arrayWithCapacity:diagnostics.size()];

	for(auto const& diag : diagnostics)
	{
		if(!diag.contains("range") || !diag.contains("message"))
			continue;

		auto const& range = diag["range"];
		int line     = range.value("/start/line"_json_pointer, 0);
		int col      = range.value("/start/character"_json_pointer, 0);
		int endLine  = range.value("/end/line"_json_pointer, 0);
		int endCol   = range.value("/end/character"_json_pointer, 0);
		int severity = diag.value("severity", 1);
		std::string message = diag["message"].get<std::string>();

		NSMutableDictionary* entry = [NSMutableDictionary dictionaryWithDictionary:@{
			@"line":         @(line),
			@"character":    @(col),
			@"endLine":      @(endLine),
			@"endCharacter": @(endCol),
			@"severity":     @(severity),
			@"message":      to_ns(message)
		}];

		// Preserve code, source, data for codeAction context.diagnostics
		if(diag.contains("code"))
		{
			if(diag["code"].is_string())
				entry[@"code"] = to_ns(diag["code"].get<std::string>());
			else if(diag["code"].is_number())
				entry[@"code"] = @(diag["code"].get<int>());
		}

		if(diag.contains("source"))
			entry[@"source"] = to_ns(diag["source"].get<std::string>());

		if(diag.contains("data"))
			entry[@"data"] = [self convertJSON:diag["data"]];

		[results addObject:entry];
	}

	[_delegate lspClient:self didReceiveDiagnostics:results forDocumentURI:to_ns(uriStr)];
}

// MARK: - LSP lifecycle

- (void)scheduleAllDiagnostics
{
	NSAssert(NSThread.isMainThread, @"scheduleAllDiagnostics must run on the main thread");
	if(!_initialized || !_documentDiagnosticProvider || !self.running)
		return;

	_diagnosticRefreshAll = YES;

	[self armDiagnosticDebounceTimer];
}

- (void)scheduleDiagnosticsForURI:(NSString*)uri
{
	NSAssert(NSThread.isMainThread, @"scheduleDiagnosticsForURI: must run on the main thread");
	if(!_initialized || !_documentDiagnosticProvider || !self.running)
		return;

	[_diagnosticPendingURIs addObject:uri];
	if(_diagnosticInterFileDependencies)
		_diagnosticRefreshAll = YES;

	[self armDiagnosticDebounceTimer];
}

- (void)armDiagnosticDebounceTimer
{
	if(_diagnosticDebounceTimer && _diagnosticDebounceTimer.isValid)
	{
		[_diagnosticDebounceTimer setFireDate:[NSDate dateWithTimeIntervalSinceNow:0.25]];
		return;
	}

	__weak LSPClient* weakSelf = self;
	_diagnosticDebounceTimer = [NSTimer timerWithTimeInterval:0.25 repeats:NO block:^(NSTimer* timer) {
		LSPClient* client = weakSelf;
		if(!client)
			return;
		[client fireDiagnosticPull];
	}];
	[NSRunLoop.mainRunLoop addTimer:_diagnosticDebounceTimer forMode:NSRunLoopCommonModes];
}

// Timer callback: coalesces all scheduling between debounce fires into one pull set.
- (void)fireDiagnosticPull
{
	NSAssert(NSThread.isMainThread, @"fireDiagnosticPull must run on the main thread");
	if(!_initialized || !_documentDiagnosticProvider || !self.running)
		return;

	NSMutableSet<NSString*>* pullURIs = [_diagnosticPendingURIs mutableCopy];
	if(_diagnosticRefreshAll)
		[pullURIs unionSet:_diagnosticOpenURIs];
	[_diagnosticPendingURIs removeAllObjects];
	_diagnosticRefreshAll = NO;

	__weak LSPClient* weakSelf = self;
	for(NSString* uri in pullURIs)
	{
		if(![_diagnosticOpenURIs containsObject:uri])
			continue;

		NSNumber* inFlight = _diagnosticInFlightIds[uri];
		if(inFlight)
		{
			[self cancelRequest:inFlight.intValue];
			[_diagnosticInFlightIds removeObjectForKey:uri];
		}

		// Each pull bumps the generation; a superseded response is dropped.
		NSNumber* generation = @(++_nextDiagnosticGeneration);
		_diagnosticGenerations[uri] = generation;

		json params = {{"textDocument", {{"uri", uri.UTF8String}}}};
		if(_diagnosticIdentifier)
			params["identifier"] = _diagnosticIdentifier.UTF8String;

		__block int reqId = -1;
		reqId = [self sendRequest:@"textDocument/diagnostic" params:params callback:^(id result){
			LSPClient* client = weakSelf;
			if(!client)
				return;

			// Only clear our own entry; a stale callback must not clear a newer request.
			if([client->_diagnosticInFlightIds[uri] isEqual:@(reqId)])
				[client->_diagnosticInFlightIds removeObjectForKey:uri];
			if(!client.initialized || ![client->_diagnosticGenerations[uri] isEqual:generation])
				return;
			if(![result isKindOfClass:NSDictionary.class])
				return;

			NSString* kind = result[@"kind"];
			if([kind isEqual:@"full"] && [result[@"items"] isKindOfClass:NSArray.class])
			{
				json diagnostics = {{"uri", uri.UTF8String}, {"diagnostics", [client convertToJSON:result[@"items"]]}};
				[client handleDiagnostics:diagnostics];
			}
			else if([kind isEqual:@"unchanged"])
			{
				// Intentional no-op: an unchanged report means the current marks are still valid.
			}
		}];
		_diagnosticInFlightIds[uri] = @(reqId);
	}
}

- (void)resetDiagnosticState
{
	NSAssert(NSThread.isMainThread, @"resetDiagnosticState must run on the main thread");
	[_diagnosticDebounceTimer invalidate];
	_diagnosticDebounceTimer = nil;
	[_diagnosticOpenURIs removeAllObjects];
	[_diagnosticPendingURIs removeAllObjects];
	[_diagnosticGenerations removeAllObjects];
	[_diagnosticInFlightIds removeAllObjects];
	_diagnosticRefreshAll = NO;
}

- (void)sendInitialize
{
	std::string rootUriStr = [NSURL fileURLWithPath:_workingDirectory].absoluteString.UTF8String;
	json params = {
		{"processId",    (int)NSProcessInfo.processInfo.processIdentifier},
		{"rootUri",      rootUriStr},
		{"workspaceFolders", json::array({{{"uri", rootUriStr}, {"name", _workingDirectory.lastPathComponent.UTF8String}}})},
		{"initializationOptions", _initOptionsJSON.length ? json::parse(_initOptionsJSON.UTF8String, nullptr, false) : json::object()},
		{"capabilities", {
			{"general", {{"positionEncodings", {"utf-16"}}}},
			{"textDocument", {
				{"documentSymbol", {{"dynamicRegistration", false}, {"hierarchicalDocumentSymbolSupport", true}}},
				{"references", {{"dynamicRegistration", false}}},
				{"publishDiagnostics", json::object()},
				{"diagnostic", {
					{"dynamicRegistration", false},
					{"relatedDocumentSupport", false}
				}},
				{"synchronization", {
					{"didSave", true},
					{"dynamicRegistration", false}
				}},
				{"completion", {
					{"dynamicRegistration", false},
					{"completionItem", {
						{"snippetSupport", true},
						{"resolveSupport", {
							{"properties", {"documentation", "detail", "additionalTextEdits"}}
						}}
					}}
				}},
				{"definition", {
					{"dynamicRegistration", false}
				}},
				{"hover", {
					{"dynamicRegistration", false},
					{"contentFormat", {"markdown", "plaintext"}}
				}},
				{"rename", {
					{"dynamicRegistration", false},
					{"prepareSupport", true}
				}},
				{"codeAction", {
					{"dynamicRegistration", false},
					{"codeActionLiteralSupport", {
						{"codeActionKind", {
							{"valueSet", {"quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"}}
						}}
					}},
					{"resolveSupport", {
						{"properties", {"edit", "command"}}
					}},
					{"dataSupport", true},
					{"isPreferredSupport", true}
				}}
			}},
			{"window", {
				{"workDoneProgress", true}
			}},
			{"workspace", {
				{"symbol", {{"dynamicRegistration", false}}},
				{"diagnostics", {{"refreshSupport", true}}},
				{"didChangeWatchedFiles", {
					{"dynamicRegistration", true},
					{"relativePatternSupport", true}
				}},
				{"workspaceFolders", true}
			}}
		}}
	};
	_initializeRequestId = _nextRequestId;
	[self sendRequest:@"initialize" params:params];
}

- (void)openDocument:(OakDocument*)document languageId:(NSString*)languageId retryCount:(int)retryCount
{
	NSAssert(NSThread.isMainThread, @"openDocument:languageId:retryCount: must run on the main thread");
	if(!_initialized)
	{
		if(retryCount >= 5)
		{
			[self postLog:[NSString stringWithFormat:@"Server failed to initialize after %d retries, giving up on didOpen", retryCount] source:@"error"];
			return;
		}
		[self postLog:[NSString stringWithFormat:@"Not yet initialized, deferring didOpen (attempt %d)", retryCount + 1] source:@"event"];
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(1 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
			[self openDocument:document languageId:languageId retryCount:retryCount + 1];
		});
		return;
	}

	NSString* path = document.path;
	if(!path)
		return;

	NSString* content = document.content;
	if(!content)
		return;

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	std::string uri = fileURL.absoluteString.UTF8String;

	json params = {
		{"textDocument", {
			{"uri",        uri},
			{"languageId", languageId.UTF8String},
			{"version",    1},
			{"text",       content.UTF8String}
		}}
	};
	[self sendNotification:@"textDocument/didOpen" params:params];
	[_diagnosticOpenURIs addObject:fileURL.absoluteString];
	[self scheduleDiagnosticsForURI:fileURL.absoluteString];
	if(_diagnosticInterFileDependencies)
		[self scheduleAllDiagnostics];
}

- (void)openDocument:(OakDocument*)document languageId:(NSString*)languageId
{
	[self openDocument:document languageId:languageId retryCount:0];
}

- (void)documentDidChange:(OakDocument*)document version:(int)version
{
	NSAssert(NSThread.isMainThread, @"documentDidChange:version: must run on the main thread");
	NSString* path = document.path;
	if(!path)
		return;

	NSString* content = document.content;
	if(!content)
		return;

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	std::string uri = fileURL.absoluteString.UTF8String;

	json params = {
		{"textDocument", {
			{"uri",     uri},
			{"version", version}
		}},
		{"contentChanges", {
			{{"text", content.UTF8String}}
		}}
	};
	[self sendNotification:@"textDocument/didChange" params:params];
	[self scheduleDiagnosticsForURI:fileURL.absoluteString];
	if(_diagnosticInterFileDependencies)
		[self scheduleAllDiagnostics];
}

- (void)documentDidSave:(OakDocument*)document
{
	NSAssert(NSThread.isMainThread, @"documentDidSave: must run on the main thread");
	NSString* path = document.path;
	if(!path)
		return;

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	std::string uri = fileURL.absoluteString.UTF8String;

	json params = {
		{"textDocument", {
			{"uri", uri}
		}}
	};
	[self sendNotification:@"textDocument/didSave" params:params];
	[self scheduleDiagnosticsForURI:fileURL.absoluteString];
	if(_diagnosticInterFileDependencies)
		[self scheduleAllDiagnostics];
}

- (void)closeDocument:(OakDocument*)document
{
	NSAssert(NSThread.isMainThread, @"closeDocument: must run on the main thread");
	NSString* path = document.path;
	if(!path)
		return;

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	std::string uri = fileURL.absoluteString.UTF8String;

	json params = {
		{"textDocument", {
			{"uri", uri}
		}}
	};
	[self sendNotification:@"textDocument/didClose" params:params];

	NSString* uriString = fileURL.absoluteString;
	[_diagnosticOpenURIs removeObject:uriString];
	[_diagnosticPendingURIs removeObject:uriString];

	NSNumber* inFlight = _diagnosticInFlightIds[uriString];
	if(inFlight)
	{
		[self cancelRequest:inFlight.intValue];
		[_diagnosticInFlightIds removeObjectForKey:uriString];
	}

	[_diagnosticGenerations removeObjectForKey:uriString];
	if(_diagnosticInterFileDependencies)
		[self scheduleAllDiagnostics];
}

- (int)sendRequest:(NSString*)method params:(json)params callback:(void(^)(id))callback
{
	int reqId = _nextRequestId++;
	_requestMethods[@(reqId)] = method;

	NSMutableString* logMsg = [NSMutableString stringWithFormat:@"%@ (id=%d)", method, reqId];
	NSString* file = fileFromParams(params);
	if(file)
		[logMsg appendFormat:@"  %@", file];
	if(params.contains("position"))
		[logMsg appendFormat:@":%d:%d", params["position"]["line"].get<int>(), params["position"]["character"].get<int>()];
	if(params.contains("label"))
		[logMsg appendFormat:@"  \"%s\"", params["label"].get<std::string>().c_str()];

	json msg = {
		{"jsonrpc", "2.0"},
		{"id",      reqId},
		{"method",  method.UTF8String},
		{"params",  params}
	};
	if(params.is_null())
		msg.erase("params");
	[self postLog:logMsg source:@"request"];
	if(callback)
		_responseCallbacks[@(reqId)] = [callback copy];
	[self sendMessage:msg];
	return reqId;
}

- (void)cancelRequest:(int)requestId
{
	[_responseCallbacks removeObjectForKey:@(requestId)];
	[_requestMethods removeObjectForKey:@(requestId)];

	json params = {{"id", requestId}};
	[self sendNotification:@"$/cancelRequest" params:params];
}

- (void)respondToApplyEdit:(id)requestId applied:(BOOL)applied failureReason:(NSString*)reason
{
	json result = {{"applied", (bool)applied}};
	if(!applied && reason)
		result["failureReason"] = reason.UTF8String;

	json response = {
		{"jsonrpc", "2.0"},
		{"id",      objCIdToJson(requestId)},
		{"result",  result}
	};
	[self sendMessage:response];
}

- (void)respondToShowMessageRequest:(id)requestId action:(NSDictionary*)action
{
	json result = action ? [self convertToJSON:action] : json(nullptr);
	json response = {{"jsonrpc", "2.0"}, {"id", objCIdToJson(requestId)}, {"result", result}};
	[self sendMessage:response];
}

- (BOOL)supportsLanguageQuery:(NSString*)method { return [_languageQueries containsObject:method]; }

- (int)sendCustomRequest:(NSString*)method params:(NSDictionary*)params completion:(void(^)(id))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return -1;
	}
	json jsonParams = params ? [self convertToJSON:params] : json::object();
	return [self sendRequest:method params:jsonParams callback:callback];
}

- (void)sendCustomNotification:(NSString*)method params:(NSDictionary*)params
{
	if(!_initialized)
		return;
	json jsonParams = params ? [self convertToJSON:params] : json::object();
	[self sendNotification:method params:jsonParams];
}

- (id)convertJSON:(json const&)value
{
	if(value.is_null())
		return [NSNull null];
	if(value.is_boolean())
		return @(value.get<bool>());
	if(value.is_number_integer())
		return @(value.get<int64_t>());
	if(value.is_number_float())
		return @(value.get<double>());
	if(value.is_string())
		return to_ns(value.get<std::string>());
	if(value.is_array())
	{
		NSMutableArray* arr = [NSMutableArray arrayWithCapacity:value.size()];
		for(auto const& item : value)
			[arr addObject:[self convertJSON:item]];
		return arr;
	}
	if(value.is_object())
	{
		NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithCapacity:value.size()];
		for(auto it = value.begin(); it != value.end(); ++it)
			dict[to_ns(it.key())] = [self convertJSON:it.value()];
		return dict;
	}
	return [NSNull null];
}

- (json)convertToJSON:(id)obj
{
	if([obj isKindOfClass:[NSDictionary class]])
	{
		json result = json::object();
		for(NSString* key in obj)
			result[key.UTF8String] = [self convertToJSON:obj[key]];
		return result;
	}
	else if([obj isKindOfClass:[NSArray class]])
	{
		json result = json::array();
		for(id item in obj)
			result.push_back([self convertToJSON:item]);
		return result;
	}
	else if([obj isKindOfClass:[NSString class]])
		return json([obj UTF8String]);
	else if([obj isKindOfClass:[NSNumber class]])
	{
		// @YES/@NO are CFBoolean singletons — objCType is "c" not "B" on arm64
		if(obj == (id)kCFBooleanTrue || obj == (id)kCFBooleanFalse)
			return json([obj boolValue]);
		else if(strcmp([obj objCType], @encode(double)) == 0 || strcmp([obj objCType], @encode(float)) == 0)
			return json([obj doubleValue]);
		else
			return json([obj longLongValue]);
	}
	else if([obj isKindOfClass:[NSNull class]])
		return json(nullptr);
	return json(nullptr);
}

- (void)requestCompletionForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(@[]);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}},
		{"context", {{"triggerKind", 1}}}
	};

	[self sendRequest:@"textDocument/completion" params:params callback:^(id result) {
		NSMutableArray<NSDictionary*>* suggestions = [NSMutableArray new];

		NSArray* items = nil;
		if([result isKindOfClass:[NSArray class]])
		{
			items = result;
		}
		else if([result isKindOfClass:[NSDictionary class]])
		{
			items = result[@"items"];
		}

	for(NSDictionary* item in items)
		{
			NSString* label = item[@"label"];
			if(label.length == 0)
				continue;

			NSString* insertText = item[@"insertText"];
			NSNumber* insertTextFormat = item[@"insertTextFormat"];

			// Servers use textEdit instead of insertText when snippetSupport is on
			if(!insertText && item[@"textEdit"])
			{
				NSDictionary* textEdit = item[@"textEdit"];
				insertText = textEdit[@"newText"];
			}
			if(!insertText)
				insertText = label;

			NSMutableDictionary* suggestion = [@{
				@"label":      label,
				@"filterText": label,
				@"insert":     insertText,
			} mutableCopy];

			if(item[@"kind"])
				suggestion[@"kind"] = item[@"kind"];
			if(item[@"detail"])
				suggestion[@"detail"] = item[@"detail"];
			if(insertTextFormat)
				suggestion[@"insertTextFormat"] = insertTextFormat;

			suggestion[@"_originalItem"] = item;

			[suggestions addObject:suggestion];
		}

		if(callback)
			callback(suggestions);
	}];
}

- (void)resolveCompletionItem:(NSDictionary*)item completion:(void(^)(NSDictionary*))callback
{
	if(!_initialized || !_completionResolveProvider)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSData* jsonData = [NSJSONSerialization dataWithJSONObject:item options:0 error:nil];
	NSString* jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
	json params = json::parse(jsonString.UTF8String, nullptr, false);

	[self sendRequest:@"completionItem/resolve" params:params callback:^(id result) {
		if([result isKindOfClass:[NSDictionary class]])
		{
			if(callback)
				callback(result);
		}
		else
		{
			if(callback)
				callback(nil);
		}
	}];
}

- (void)requestDefinitionForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(@[]);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}}
	};

	[self sendRequest:@"textDocument/definition" params:params callback:^(id result) {
		NSMutableArray<NSDictionary*>* locations = [NSMutableArray new];

		// Response can be Location, Location[], or null
		NSArray* items = nil;
		if([result isKindOfClass:[NSArray class]])
			items = result;
		else if([result isKindOfClass:[NSDictionary class]])
			items = @[result];

		for(NSDictionary* item in items)
		{
			NSString* locationUri = item[@"uri"];
			NSDictionary* range = item[@"range"];
			if(!locationUri || !range)
				continue;

			NSDictionary* start = range[@"start"];
			[locations addObject:@{
				@"uri":       locationUri,
				@"line":      start[@"line"] ?: @0,
				@"character": start[@"character"] ?: @0
			}];
		}

		if(callback)
			callback(locations);
	}];
}

- (int)requestHoverForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return 0;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}}
	};

	return [self sendRequest:@"textDocument/hover" params:params callback:^(id result) {
		if(![result isKindOfClass:[NSDictionary class]])
		{
			if(callback)
				callback(nil);
			return;
		}

		NSDictionary* dict = (NSDictionary*)result;
		id contents = dict[@"contents"];
		if(!contents)
		{
			if(callback)
				callback(nil);
			return;
		}

		// contents can be: MarkedString, MarkedString[], or MarkupContent
		// MarkedString = string | {language, value}
		// MarkupContent = {kind, value}
		NSString* value = nil;
		NSString* language = nil;
		NSString* kind = nil;

		if([contents isKindOfClass:[NSString class]])
		{
			value = contents;
		}
		else if([contents isKindOfClass:[NSDictionary class]])
		{
			NSDictionary* contentsDict = contents;
			if(contentsDict[@"kind"])
			{
				// MarkupContent: {kind: "markdown"|"plaintext", value: "..."}
				kind = contentsDict[@"kind"];
				value = contentsDict[@"value"];
			}
			else if(contentsDict[@"language"])
			{
				// MarkedString object: {language: "php", value: "..."}
				language = contentsDict[@"language"];
				value = contentsDict[@"value"];
			}
			else if(contentsDict[@"value"])
			{
				value = contentsDict[@"value"];
			}
		}
		else if([contents isKindOfClass:[NSArray class]])
		{
			// MarkedString[] — wrap {language,value} entries in code fences and deduplicate
			NSMutableString* combined = [NSMutableString new];
			NSMutableSet* seen = [NSMutableSet new];
			kind = @"markdown";

			for(id item in (NSArray*)contents)
			{
				NSString* fragment = nil;
				if([item isKindOfClass:[NSString class]])
				{
					fragment = item;
				}
				else if([item isKindOfClass:[NSDictionary class]])
				{
					NSDictionary* d = item;
					NSString* v = d[@"value"];
					NSString* lang = d[@"language"];
					if(v.length > 0 && lang.length > 0)
						fragment = [NSString stringWithFormat:@"```%@\n%@\n```", lang, v];
					else if(v.length > 0)
						fragment = v;
					if(!language && lang)
						language = lang;
				}

				if(fragment.length > 0 && ![seen containsObject:fragment])
				{
					[seen addObject:fragment];
					if(combined.length > 0) [combined appendString:@"\n\n"];
					[combined appendString:fragment];
				}
			}
			value = combined;
		}

		if(!value.length)
		{
			if(callback)
				callback(nil);
			return;
		}

		NSMutableDictionary* hover = [NSMutableDictionary new];
		hover[@"value"] = value;
		if(kind)
			hover[@"kind"] = kind;
		if(language)
			hover[@"language"] = language;

		if(callback)
			callback(hover);
	}];
}

- (void)requestReferencesForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(@[]);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}},
		{"context", {{"includeDeclaration", true}}}
	};

	[self sendRequest:@"textDocument/references" params:params callback:^(id result) {
		NSMutableArray<NSDictionary*>* locations = [NSMutableArray new];

		NSArray* items = nil;
		if([result isKindOfClass:[NSArray class]])
			items = result;
		else if([result isKindOfClass:[NSDictionary class]])
			items = @[result];

		for(NSDictionary* item in items)
		{
			NSString* locationUri = item[@"uri"];
			NSDictionary* range = item[@"range"];
			if(!locationUri || !range)
				continue;

			NSDictionary* start = range[@"start"];
			[locations addObject:@{
				@"uri":       locationUri,
				@"line":      start[@"line"] ?: @0,
				@"character": start[@"character"] ?: @0
			}];
		}

		if(callback)
			callback(locations);
	}];
}

- (void)prepareRenameForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary* _Nullable))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}}
	};

	[self sendRequest:@"textDocument/prepareRename" params:params callback:^(id result) {
		if(!result || [result isKindOfClass:[NSNull class]])
		{
			if(callback)
				callback(nil);
			return;
		}

		if(callback)
			callback([result isKindOfClass:[NSDictionary class]] ? result : nil);
	}];
}

- (void)requestRenameForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character newName:(NSString*)newName completion:(void(^)(NSDictionary* _Nullable))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"position", {{"line", (int)line}, {"character", (int)character}}},
		{"newName", newName.UTF8String}
	};

	[self sendRequest:@"textDocument/rename" params:params callback:^(id result) {
		if(!result || [result isKindOfClass:[NSNull class]])
		{
			if(callback)
				callback(nil);
			return;
		}

		if(callback)
			callback([result isKindOfClass:[NSDictionary class]] ? result : nil);
	}];
}

- (void)requestCodeActionsForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter diagnostics:(NSArray<NSDictionary*>*)diagnostics completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json diagsJson = json::array();
	for(NSDictionary* diag in diagnostics)
	{
		json d = {
			{"range", {
				{"start", {{"line", [diag[@"line"] intValue]}, {"character", [diag[@"character"] intValue]}}},
				{"end", {{"line", [diag[@"endLine"] intValue]}, {"character", [diag[@"endCharacter"] intValue]}}}
			}},
			{"severity", [diag[@"severity"] intValue]},
			{"message", [diag[@"message"] UTF8String]}
		};

		if(diag[@"code"])
		{
			if([diag[@"code"] isKindOfClass:[NSString class]])
				d["code"] = [diag[@"code"] UTF8String];
			else
				d["code"] = [diag[@"code"] intValue];
		}
		if(diag[@"source"])
			d["source"] = [diag[@"source"] UTF8String];
		if(diag[@"data"])
			d["data"] = [self convertToJSON:diag[@"data"]];

		diagsJson.push_back(d);
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"range", {
			{"start", {{"line", (int)line}, {"character", (int)character}}},
			{"end", {{"line", (int)endLine}, {"character", (int)endCharacter}}}
		}},
		{"context", {
			{"diagnostics", diagsJson},
			{"triggerKind", 1}
		}}
	};

	[self sendRequest:@"textDocument/codeAction" params:params callback:^(id result) {
		if(callback)
			callback([result isKindOfClass:[NSArray class]] ? result : nil);
	}];
}

- (void)resolveCodeAction:(NSDictionary*)codeAction completion:(void(^)(NSDictionary*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = [self convertToJSON:codeAction];
	[self sendRequest:@"codeAction/resolve" params:params callback:^(id result) {
		if(callback)
			callback([result isKindOfClass:[NSDictionary class]] ? result : nil);
	}];
}

- (void)executeCommand:(NSString*)command arguments:(NSArray*)arguments completion:(void(^)(id))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = {
		{"command", command.UTF8String}
	};
	if(arguments)
		params["arguments"] = [self convertToJSON:arguments];

	[self sendRequest:@"workspace/executeCommand" params:params callback:^(id result) {
		if(callback)
			callback(result);
	}];
}

- (void)requestFormattingForURI:(NSString*)uri tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"options", {
			{"tabSize", (int)tabSize},
			{"insertSpaces", (bool)insertSpaces}
		}}
	};

	[self sendRequest:@"textDocument/formatting" params:params callback:^(id result) {
		if([result isKindOfClass:[NSArray class]])
		{
			if(callback)
				callback(result);
		}
		else
		{
			if(callback)
				callback(nil);
		}
	}];
}

- (void)requestRangeFormattingForURI:(NSString*)uri startLine:(NSUInteger)startLine startCharacter:(NSUInteger)startCharacter endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	if(!_initialized)
	{
		if(callback)
			callback(nil);
		return;
	}

	json params = {
		{"textDocument", {{"uri", uri.UTF8String}}},
		{"range", {
			{"start", {{"line", (int)startLine}, {"character", (int)startCharacter}}},
			{"end", {{"line", (int)endLine}, {"character", (int)endCharacter}}}
		}},
		{"options", {
			{"tabSize", (int)tabSize},
			{"insertSpaces", (bool)insertSpaces}
		}}
	};

	[self sendRequest:@"textDocument/rangeFormatting" params:params callback:^(id result) {
		if([result isKindOfClass:[NSArray class]])
		{
			if(callback)
				callback(result);
		}
		else
		{
			if(callback)
				callback(nil);
		}
	}];
}

// MARK: - File watching

- (NSArray<NSString*>*)fileWatchExcludes
{
	std::string filePath = to_s(_workingDirectory);
	settings_t settings = settings_for_path(filePath, "", filePath);
	std::string excludeSetting = settings.get("lspFileWatchExclude", "");

	if(excludeSetting.empty())
		return @[];

	NSString* excludeStr = to_ns(excludeSetting);
	NSMutableArray<NSString*>* result = [NSMutableArray new];
	for(NSString* item in [excludeStr componentsSeparatedByString:@","])
	{
		NSString* trimmed = [item stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceCharacterSet];
		// Strip trailing slash
		if([trimmed hasSuffix:@"/"])
			trimmed = [trimmed substringToIndex:trimmed.length - 1];
		if(trimmed.length)
			[result addObject:trimmed];
	}
	return result;
}

- (void)handleRegisterCapability:(json const&)params
{
	if(!params.contains("registrations"))
		return;

	for(auto const& reg : params["registrations"])
	{
		if(!reg.contains("method") || !reg["method"].is_string() || !reg.contains("id"))
			continue;

		std::string method = reg["method"].get<std::string>();
		if(method != "workspace/didChangeWatchedFiles")
		{
			if([_delegate respondsToSelector:@selector(lspClient:handleServerRequest:params:)])
			{
				NSDictionary* regDict = [self convertJSON:reg];
				[_delegate lspClient:self handleServerRequest:@"client/registerCapability" params:@{@"registrations": @[regDict]}];
			}
			continue;
		}

		std::string regId = reg["id"].is_string() ? reg["id"].get<std::string>() : std::to_string(reg["id"].get<int>());
		[self postLog:[NSString stringWithFormat:@"Registering file watcher: %s", regId.c_str()] source:@"event"];

		if(!_fileWatchRegistrations)
			_fileWatchRegistrations = [NSMutableDictionary new];

		if(reg.contains("registerOptions") && reg["registerOptions"].contains("watchers"))
		{
			int watcherIndex = 0;
			for(auto const& watcher : reg["registerOptions"]["watchers"])
			{
				LSPFileWatchRegistration* registration = [LSPFileWatchRegistration new];
				// Unique ID per watcher within a registration
				registration.registrationId = [NSString stringWithFormat:@"%s:%d", regId.c_str(), watcherIndex++];
				registration.watchKind = watcher.contains("kind") ? watcher["kind"].get<int>() : 7;

				NSMutableSet<NSString*>* extensions = [NSMutableSet new];
				NSMutableSet<NSString*>* exactNames = [NSMutableSet new];
				NSString* basePath = nil;
				BOOL watchAll = NO;

				if(watcher.contains("globPattern"))
				{
					auto const& glob = watcher["globPattern"];
					if(glob.is_string())
					{
						extractExtensionsFromGlob(to_ns(glob.get<std::string>()), extensions, exactNames, &watchAll);
					}
					else if(glob.is_object())
					{
						// RelativePattern: {baseUri, pattern}
						if(glob.contains("pattern"))
							extractExtensionsFromGlob(to_ns(glob["pattern"].get<std::string>()), extensions, exactNames, &watchAll);

						if(glob.contains("baseUri"))
						{
							std::string baseUri;
							if(glob["baseUri"].is_string())
								baseUri = glob["baseUri"].get<std::string>();
							else if(glob["baseUri"].is_object() && glob["baseUri"].contains("uri"))
								baseUri = glob["baseUri"]["uri"].get<std::string>();

							if(!baseUri.empty())
							{
								NSURL* url = [NSURL URLWithString:to_ns(baseUri)];
								if(url.isFileURL)
								{
									NSString* path = url.path;
									if([path hasPrefix:_workingDirectory])
									{
										basePath = path;
									}
									else
									{
										[self postLog:[NSString stringWithFormat:@"File watch: baseUri '%s' is outside working directory, skipping", baseUri.c_str()] source:@"event"];
										continue;
									}
								}
							}
						}
					}
					else
					{
						[self postLog:@"File watch: unrecognized globPattern format, skipping" source:@"event"];
						continue;
					}
				}

				registration.extensions = extensions;
				registration.exactNames = exactNames;
				registration.basePath = basePath;
				registration.watchAll = watchAll;

				_fileWatchRegistrations[registration.registrationId] = registration;

				[self postLog:[NSString stringWithFormat:@"File watch registered: id=%@ extensions=%@ exactNames=%@ kind=%d basePath=%@ watchAll=%d",
					registration.registrationId, registration.extensions, registration.exactNames,
					registration.watchKind, registration.basePath ?: _workingDirectory, registration.watchAll] source:@"event"];
			}
		}

		[self setupFileWatcherIfNeeded];
	}
}

- (void)handleUnregisterCapability:(json const&)params
{
	// LSP spec uses "unregisterations" (with the typo)
	NSString* key = params.contains("unregisterations") ? @"unregisterations" : @"unregistrations";
	std::string keyStr = key.UTF8String;

	if(!params.contains(keyStr))
		return;

	for(auto const& unreg : params[keyStr])
	{
		if(!unreg.contains("method") || !unreg["method"].is_string() || !unreg.contains("id"))
			continue;

		std::string method = unreg["method"].get<std::string>();
		if(method != "workspace/didChangeWatchedFiles")
		{
			if([_delegate respondsToSelector:@selector(lspClient:handleServerRequest:params:)])
			{
				NSDictionary* unregDict = [self convertJSON:unreg];
				[_delegate lspClient:self handleServerRequest:@"client/unregisterCapability" params:@{key: @[unregDict]}];
			}
			continue;
		}

		std::string regId = unreg["id"].is_string() ? unreg["id"].get<std::string>() : std::to_string(unreg["id"].get<int>());
		NSString* regIdPrefix = [NSString stringWithFormat:@"%s:", regId.c_str()];
		[self postLog:[NSString stringWithFormat:@"Unregistering file watcher: %s", regId.c_str()] source:@"event"];

		// Remove all per-watcher entries for this registration (keyed as "regId:0", "regId:1", etc.)
		NSArray<NSString*>* regKeys = _fileWatchRegistrations.allKeys;
		for(NSString* regKey in regKeys)
		{
			if([regKey hasPrefix:regIdPrefix])
				[_fileWatchRegistrations removeObjectForKey:regKey];
		}
	}

	if(_fileWatchRegistrations.count == 0)
		[self teardownFileWatcher];
}

- (void)setupFileWatcherIfNeeded
{
	// Tear down existing watcher to rebuild with merged extensions from all registrations
	if(_fileWatcher)
		[self teardownFileWatcher];

	NSArray<NSString*>* excludes = [self fileWatchExcludes];
	_fileWatcher = [[LSPFileWatcher alloc] initWithRootDirectory:_workingDirectory excludes:excludes];

	// Merge extensions, exact names, and watchAll from all registrations
	for(LSPFileWatchRegistration* reg in _fileWatchRegistrations.allValues)
	{
		[_fileWatcher addExtensions:reg.extensions];
		[_fileWatcher addExactNames:reg.exactNames];
		if(reg.watchAll)
			_fileWatcher.watchAll = YES;
	}

	if(!_scanQueue)
		_scanQueue = dispatch_queue_create("com.macromates.lsp.filescan", DISPATCH_QUEUE_SERIAL);

	__weak LSPClient* weakSelf = self;
	LSPFileWatcher* capturedWatcher = _fileWatcher;
	[_fileWatcher performInitialScanOnQueue:_scanQueue completion:^{
		LSPClient* strongSelf = weakSelf;
		if(!strongSelf)
			return;

		// If watcher was torn down or replaced during the scan, discard results
		if(strongSelf->_fileWatcher != capturedWatcher)
			return;

		[strongSelf startFSEventsObserver];
	}];
}

- (void)startFSEventsObserver
{
	if(_fsEventsObserver)
		return;

	NSURL* rootURL = [NSURL fileURLWithPath:_workingDirectory];
	__weak LSPClient* weakSelf = self;

	_fsEventsObserver = [FSEventsManager.sharedInstance addObserverToDirectoryAtURL:rootURL observeSubdirectories:YES usingBlock:^(NSURL* changedURL) {
		dispatch_async(dispatch_get_main_queue(), ^{
			LSPClient* strongSelf = weakSelf;
			if(!strongSelf || !strongSelf->_fileWatcher)
				return;

			[strongSelf handleFSEventAtURL:changedURL];
		});
	}];
}

- (void)handleFSEventAtURL:(NSURL*)url
{
	NSString* dirPath = url.path;
	__weak LSPClient* weakSelf = self;
	LSPFileWatcher* capturedWatcher = _fileWatcher;

	[_fileWatcher asyncDiffForChangedDirectory:dirPath onQueue:_scanQueue completion:^(NSArray<NSDictionary*>* changes) {
		LSPClient* strongSelf = weakSelf;
		if(!strongSelf || strongSelf->_fileWatcher != capturedWatcher)
			return;

		[strongSelf processFileChanges:changes];
	}];
}

- (void)processFileChanges:(NSArray<NSDictionary*>*)changes
{
	if(changes.count == 0)
		return;

	// Get open document paths to filter Changed events
	NSSet<NSString*>* openPaths = nil;
	if([_delegate respondsToSelector:@selector(lspClientOpenDocumentPaths:)])
		openPaths = [_delegate lspClientOpenDocumentPaths:self];

	NSMutableArray<NSDictionary*>* filtered = [NSMutableArray new];
	for(NSDictionary* change in changes)
	{
		int changeType = [change[@"type"] intValue];
		NSString* uri = change[@"uri"];

		// Filter by watchKind across all registrations (union semantics)
		BOOL matchesAnyRegistration = NO;
		for(LSPFileWatchRegistration* reg in _fileWatchRegistrations.allValues)
		{
			// Convert FileChangeType enum (1=Created,2=Changed,3=Deleted) to WatchKind bit (1,2,4)
			int watchKindBit;
			switch(changeType)
			{
				case 1: watchKindBit = 1; break;
				case 2: watchKindBit = 2; break;
				case 3: watchKindBit = 4; break;
				default: continue;
			}
			if(!(reg.watchKind & watchKindBit))
				continue;

			// Check if file falls under this registration's basePath
			if(reg.basePath)
			{
				NSURL* fileURL = [NSURL URLWithString:uri];
				NSString* filePath = fileURL.path;
				if(filePath && ![filePath hasPrefix:reg.basePath])
					continue;
			}

			matchesAnyRegistration = YES;
			break;
		}

		if(!matchesAnyRegistration)
			continue;

		// Skip Changed events for open documents
		if(changeType == 2 && openPaths)
		{
			NSURL* fileURL = [NSURL URLWithString:uri];
			NSString* filePath = fileURL.path;
			if(filePath && [openPaths containsObject:filePath])
				continue;
		}

		[filtered addObject:change];
	}

	if(filtered.count == 0)
		return;

	if(!_pendingChanges)
		_pendingChanges = [NSMutableDictionary new];
	for(NSDictionary* change in filtered)
	{
		NSString* uri = change[@"uri"];
		NSDictionary* existing = _pendingChanges[uri];
		if(!existing)
		{
			_pendingChanges[uri] = change;
			continue;
		}

		int oldType = [existing[@"type"] intValue];
		int newType = [change[@"type"] intValue];

		if(oldType == 1 && newType == 2)
			continue; // Created + Changed → keep Created (file is still new to server)
		else if(oldType == 1 && newType == 3)
			[_pendingChanges removeObjectForKey:uri]; // Created + Deleted → net no-op
		else
			_pendingChanges[uri] = change;
	}

	[_debounceTimer invalidate];
	__weak LSPClient* weakSelf = self;
	_debounceTimer = [NSTimer scheduledTimerWithTimeInterval:0.2 repeats:NO block:^(NSTimer* timer) {
		[weakSelf flushPendingFileChanges];
	}];
}

- (void)flushPendingFileChanges
{
	if(!_pendingChanges.count)
		return;

	NSArray<NSDictionary*>* allChanges = _pendingChanges.allValues;
	[_pendingChanges removeAllObjects];

	static NSUInteger const kBatchSize = 500;

	for(NSUInteger offset = 0; offset < allChanges.count; offset += kBatchSize)
	{
		NSUInteger length = MIN(kBatchSize, allChanges.count - offset);
		NSArray<NSDictionary*>* batch = [allChanges subarrayWithRange:NSMakeRange(offset, length)];

		json changesArray = json::array();
		for(NSDictionary* change in batch)
		{
			changesArray.push_back({
				{"uri",  [change[@"uri"] UTF8String]},
				{"type", [change[@"type"] intValue]}
			});
		}

		json params = {{"changes", changesArray}};
		[self sendNotification:@"workspace/didChangeWatchedFiles" params:params];
	}
	[self scheduleAllDiagnostics];
}

- (void)teardownFileWatcher
{
	[_debounceTimer invalidate];
	_debounceTimer = nil;
	[_pendingChanges removeAllObjects];
	_pendingChanges = nil;

	if(_fsEventsObserver)
	{
		[FSEventsManager.sharedInstance removeObserver:_fsEventsObserver];
		_fsEventsObserver = nil;
	}

	[_fileWatcher clearSnapshot];
	_fileWatcher = nil;
}

- (void)dealloc
{
	// Safety net: clean up resources if -shutdown was never called or the
	// server outlived its owner.  The readability handler retains a block
	// that captures a weak self, but NSFileHandle keeps dispatching until
	// the handler is explicitly nilled — which also prevents the pipe's
	// file descriptor from being closed.
	_stderrPipe.fileHandleForReading.readabilityHandler = nil;

	if(_task.isRunning)
		[_task terminate];

	[_debounceTimer invalidate];
	if(_fsEventsObserver)
		[FSEventsManager.sharedInstance removeObserver:_fsEventsObserver];

	// Diagnostic state is main-thread-confined: invalidate the debounce timer on
	// its owning thread, then drop the collections without asserting.
	NSTimer* diagnosticTimer = _diagnosticDebounceTimer;
	if(diagnosticTimer)
	{
		if([NSThread isMainThread])
			[diagnosticTimer invalidate];
		else
			dispatch_async(dispatch_get_main_queue(), ^{ [diagnosticTimer invalidate]; });
	}
	_diagnosticDebounceTimer = nil;
	[_diagnosticOpenURIs removeAllObjects];
	[_diagnosticPendingURIs removeAllObjects];
	[_diagnosticGenerations removeAllObjects];
	[_diagnosticInFlightIds removeAllObjects];
	_diagnosticRefreshAll = NO;

	[_responseCallbacks removeAllObjects];
}

- (void)shutdown
{
	[self resetDiagnosticState];
	if(!_task.isRunning)
		return;

	[self teardownFileWatcher];
	[_fileWatchRegistrations removeAllObjects];

	[self postLog:@"Shutting down server" source:@"event"];
	[self sendRequest:@"shutdown" params:nullptr];

	// Give server 2s to respond, then send exit
	__weak LSPClient* weakSelf = self;
	dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(2 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
		LSPClient* strongSelf = weakSelf;
		if(!strongSelf)
			return;
		[strongSelf sendNotification:@"exit" params:nullptr];
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(1 * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
			LSPClient* innerSelf = weakSelf;
			if(innerSelf && innerSelf->_task.isRunning)
				[innerSelf->_task terminate];
		});
	});
}

@end

NSString* LSPLanguageIdForExtension (NSString* ext)
{
	if(!ext.length)
		return @"plaintext";

	static NSDictionary* map = @{
		@"php"  : @"php",
		@"c"    : @"c",
		@"h"    : @"c",
		@"cc"   : @"cpp",
		@"cpp"  : @"cpp",
		@"cxx"  : @"cpp",
		@"hpp"  : @"cpp",
		@"m"    : @"objective-c",
		@"mm"   : @"objective-cpp",
		@"js"   : @"javascript",
		@"jsx"  : @"javascriptreact",
		@"ts"   : @"typescript",
		@"tsx"  : @"typescriptreact",
		@"py"   : @"python",
		@"go"   : @"go",
		@"rs"   : @"rust",
		@"rb"   : @"ruby",
		@"java" : @"java",
		@"json" : @"json",
		@"css"  : @"css",
		@"html" : @"html",
		@"htm"  : @"html",
		@"sh"   : @"shellscript",
		@"bash" : @"shellscript",
		@"zsh"  : @"shellscript",
		@"yaml" : @"yaml",
		@"yml"  : @"yaml",
		@"xml"  : @"xml",
		@"sql"  : @"sql",
		@"lua"  : @"lua",
		@"swift": @"swift",
		@"md"   : @"markdown",
		@"vue"  : @"vue",
		@"svelte": @"svelte",
		@"scss" : @"scss",
		@"less" : @"less",
		@"r"    : @"r",
		@"pl"   : @"perl",
		@"kt"   : @"kotlin",
		@"dart" : @"dart",
		@"ex"   : @"elixir",
		@"exs"  : @"elixir",
		@"erl"  : @"erlang",
		@"hs"   : @"haskell",
		@"toml" : @"toml",
		@"ini"  : @"ini",
		@"tf"   : @"terraform",
		@"dockerfile" : @"dockerfile",
	};

	NSString* langId = map[ext.lowercaseString];
	return langId ?: ext.lowercaseString;
}
