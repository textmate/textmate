#import "CopilotManager.h"
#import "LSPClient.h"
#import <settings/settings.h>
#import <io/environment.h>
#import <ns/ns.h>



NSNotificationName const CopilotStatusDidChangeNotification = @"CopilotStatusDidChangeNotification";
NSNotificationName const CopilotLogNotification             = @"CopilotLogNotification";

// copilot-language-server speaks LSP JSON-RPC with textDocument/inlineCompletion

@interface CopilotManager () <LSPClientDelegate>
{
	LSPClient* _client;
	NSMutableDictionary<NSString*, NSNumber*>* _documentVersions;
	NSMutableSet<NSString*>* _openURIs;
	NSMutableDictionary<NSString*, NSTimer*>* _changeTimers;
	NSHashTable<OakDocument*>* _pendingDocuments; // docs opened before server initialized
	NSInteger _restartCount;
	CopilotStatus _status;
	NSString* _username;
	BOOL _checkingAuth;
}
@end

@implementation CopilotManager

@synthesize status   = _status;
@synthesize username = _username;

static settings_t settingsForCopilotDocument(OakDocument* document)
{
	std::string filePath  = document.path ? to_s(document.path) : NULL_STR;
	std::string fileType  = document.path ? to_s(document.fileType ?: @"") : "";
	std::string directory = document.path ? to_s(document.directory ?: [document.path stringByDeletingLastPathComponent]) : NULL_STR;
	return settings_for_path(filePath, fileType, directory);
}

// MARK: - Singleton

+ (instancetype)sharedManager
{
	static CopilotManager* instance;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		instance = [[CopilotManager alloc] init];
	});
	return instance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_documentVersions = [NSMutableDictionary new];
		_openURIs         = [NSMutableSet new];
		_pendingDocuments = [NSHashTable weakObjectsHashTable];
		_changeTimers     = [NSMutableDictionary new];
		_restartCount     = 0;
		_status           = CopilotStatusDisabled;
	}
	return self;
}

// MARK: - Binary Detection

- (NSString*)detectServerPath
{
	NSFileManager* fm = NSFileManager.defaultManager;

	// Search PATH from oak::basic_environment() (includes user's shell PATH)
	auto const& env = oak::basic_environment();
	auto it = env.find("PATH");
	NSString* pathStr = it != env.end() ? to_ns(it->second) : @"/usr/bin:/bin:/usr/sbin:/sbin";
	for(NSString* dir in [pathStr componentsSeparatedByString:@":"])
	{
		NSString* candidate = [dir stringByAppendingPathComponent:@"copilot-language-server"];
		if([fm isExecutableFileAtPath:candidate])
		{
			[self log:[NSString stringWithFormat:@"Found via PATH: %@", candidate]];
			return candidate;
		}
	}

	// Fallback: well-known npm global locations (arm64 + x86_64)
	NSArray<NSString*>* suffixes = @[
		@"node_modules/@github/copilot-language-server-darwin-arm64/copilot-language-server",
		@"node_modules/@github/copilot-language-server-darwin-x64/copilot-language-server",
	];
	NSArray<NSString*>* npmRoots = @[
		@"/opt/homebrew/lib/node_modules/@github/copilot-language-server",
		[NSHomeDirectory() stringByAppendingPathComponent:@".config/yarn/global/node_modules/@github/copilot-language-server"],
		@"/usr/local/lib/node_modules/@github/copilot-language-server",
	];

	for(NSString* root in npmRoots)
	{
		for(NSString* suffix in suffixes)
		{
			NSString* candidate = [root stringByAppendingPathComponent:suffix];
			if([fm isExecutableFileAtPath:candidate])
			{
				[self log:[NSString stringWithFormat:@"Found at %@", candidate]];
				return candidate;
			}
		}
	}

	[self log:@"copilot-language-server not found — install via: npm install -g @github/copilot-language-server"];
	return nil;
}

- (NSString*)serverPathForDocument:(OakDocument*)document
{
	settings_t settings = settingsForCopilotDocument(document);

	bool enabled = settings.get("copilotEnabled", false);
	if(!enabled)
	{
		[self log:@"Copilot disabled via copilotEnabled setting"];
		return nil;
	}

	std::string customCmd = settings.get("copilotCommand", "");
	if(!customCmd.empty())
	{
		[self log:[NSString stringWithFormat:@"Using copilotCommand from settings: %s", customCmd.c_str()]];
		return to_ns(customCmd);
	}

	return [self detectServerPath];
}

// MARK: - Client Lifecycle

- (void)ensureClientForDocument:(OakDocument*)document
{
	if(document.disableLanguageServices)
		return;
	if(_client.running)
		return;

	BOOL enabled = settingsForCopilotDocument(document).get("copilotEnabled", false);
	NSString* serverPath = [self serverPathForDocument:document];
	if(!serverPath)
	{
		[self log:@"Cannot start Copilot"];
		_status = enabled ? CopilotStatusError : CopilotStatusDisabled;
		[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
		return;
	}

	[self log:[NSString stringWithFormat:@"Starting Copilot server: %@", serverPath]];
	_status = CopilotStatusConnecting;
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];

	// Native copilot-language-server requires editorInfo in initializationOptions
	NSString* initOpts = @"{\"editorInfo\":{\"name\":\"TextMate\",\"version\":\"2.1\"},\"editorPluginInfo\":{\"name\":\"textmate-copilot\",\"version\":\"1.0\"}}";

	_client = [[LSPClient alloc] initWithCommand:serverPath
	                                   arguments:@[@"--stdio"]
	                            workingDirectory:NSHomeDirectory()
	                                 initOptions:initOpts];
	_client.delegate = self;

	if(!_client.running)
	{
		[self log:@"Copilot agent failed to launch"];
		_client = nil;
		_status  = CopilotStatusError;
		[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
	}
}

- (void)shutdown
{
	[self log:@"Shutting down Copilot agent"];

	for(NSTimer* timer in _changeTimers.allValues)
		[timer invalidate];

	[_client shutdown];
	_client = nil;

	[_openURIs removeAllObjects];
	[_documentVersions removeAllObjects];
	[_changeTimers removeAllObjects];

	_status   = CopilotStatusDisabled;
	_username = nil;

	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
}

// MARK: - LSPClientDelegate

- (void)lspClientDidInitialize:(LSPClient*)client
{
	[self log:@"Copilot server initialized"];
	[self checkAuthStatus];
	[self flushPendingDocuments];
}

- (void)lspClientDidTerminate:(LSPClient*)client
{
	[self log:@"Copilot agent terminated"];

	_client = nil;
	_checkingAuth = NO;
	[_openURIs removeAllObjects];
	[_documentVersions removeAllObjects];

	for(NSTimer* timer in _changeTimers.allValues)
		[timer invalidate];
	[_changeTimers removeAllObjects];

	_status = CopilotStatusError;
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];

	if(_restartCount <= 3)
	{
		_restartCount++;
		[self log:[NSString stringWithFormat:@"Scheduling restart attempt %ld in 5s", (long)_restartCount]];
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC), dispatch_get_main_queue(), ^{
			// Re-launch without a document — binary detection uses the cached path
			[self ensureClientForDocument:nil];
		});
	}
	else
	{
		[self log:@"Copilot agent crashed too many times — giving up"];
	}
}

- (void)lspClient:(LSPClient*)client didReceiveDiagnostics:(NSArray<NSDictionary*>*)diagnostics forDocumentURI:(NSString*)uri
{
	// Copilot does not send diagnostics; this method is required by the protocol
}

- (id)lspClient:(LSPClient*)client handleServerRequest:(NSString*)method params:(NSDictionary*)params
{
	if([method isEqualToString:@"workspace/configuration"])
	{
		NSArray* items = params[@"items"] ?: @[];
		NSMutableArray* configs = [NSMutableArray arrayWithCapacity:items.count];
		for(NSDictionary* item in items)
		{
			NSString* section = item[@"section"] ?: @"";
			if([section isEqualToString:@"github.copilot"])
			{
				[configs addObject:@{
					@"enable": @{@"*": @YES},
					@"editor.enableAutoCompletions": @YES,
					@"advanced": @{},
				}];
			}
			else
			{
				[configs addObject:@{}];
			}
		}
		return configs;
	}

	[self log:[NSString stringWithFormat:@"Unhandled server request: %@", method]];
	return nil;
}

- (void)lspClient:(LSPClient*)client didReceiveNotification:(NSString*)method params:(NSDictionary*)params
{
	if([method isEqualToString:@"statusNotification"])
	{
		NSString* status = params[@"status"] ?: params[@"kind"];
		[self log:[NSString stringWithFormat:@"Status notification: %@", status]];

		if([status isEqualToString:@"Normal"])
		{
			if(_status != CopilotStatusReady)
				[self checkAuthStatus];
		}
		else if([status isEqualToString:@"Error"])
		{
			_status = CopilotStatusError;
			[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
		}
	}
}

// MARK: - Authentication

- (void)checkAuthStatus
{
	if(_checkingAuth)
		return;
	_checkingAuth = YES;

	[self log:@"Checking Copilot auth status"];

	// copilot-language-server uses direct JSON-RPC methods, not workspace/executeCommand
	[_client sendCustomRequest:@"checkStatus" params:@{} completion:^(id result) {
		self->_checkingAuth = NO;
		if(![result isKindOfClass:[NSDictionary class]])
		{
			[self log:@"checkStatus returned unexpected result"];
			self->_status = CopilotStatusError;
			[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
			return;
		}

		NSString* statusStr = result[@"status"];
		NSString* user      = result[@"user"];

		BOOL authenticated = [statusStr isEqualToString:@"OK"] || [statusStr isEqualToString:@"AlreadySignedIn"];
		if(authenticated)
		{
			self->_username = user;
			self->_status   = CopilotStatusReady;
			self->_restartCount = 0;
			[self log:[NSString stringWithFormat:@"Authenticated as %@", user ?: @"(unknown)"]];
		}
		else
		{
			self->_status = CopilotStatusAuthRequired;
			[self log:[NSString stringWithFormat:@"Auth required — status: %@", statusStr]];
		}

		[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
	}];
}

- (void)signIn
{
	[self log:@"Starting Copilot sign-in flow"];

	[_client sendCustomRequest:@"signInInitiate" params:@{} completion:^(id result) {
		if(![result isKindOfClass:[NSDictionary class]])
		{
			[self log:@"signIn returned unexpected result"];
			return;
		}

		NSString* statusStr = result[@"status"];
		if([statusStr isEqualToString:@"AlreadySignedIn"])
		{
			[self log:@"Already signed in — refreshing status"];
			[self checkAuthStatus];
			return;
		}

		NSString* userCode        = result[@"userCode"];
		NSString* verificationUri = result[@"verificationUri"];

		if(userCode && verificationUri)
			[self showAuthPanelWithCode:userCode uri:verificationUri];
		else
			[self log:[NSString stringWithFormat:@"Unexpected signIn response: %@", result]];
	}];
}

- (void)showAuthPanelWithCode:(NSString*)code uri:(NSString*)uriString
{
	[self log:[NSString stringWithFormat:@"Showing auth panel — code: %@  uri: %@", code, uriString]];

	[[NSPasteboard generalPasteboard] clearContents];
	[[NSPasteboard generalPasteboard] setString:code forType:NSPasteboardTypeString];

	NSAlert* alert = [NSAlert new];
	alert.messageText = @"GitHub Copilot";
	alert.informativeText = [NSString stringWithFormat:
		@"Enter this code at github.com/login/device:\n\n%@\n\nThe code has been copied to your clipboard.", code];
	[alert addButtonWithTitle:@"Open Browser"];
	[alert addButtonWithTitle:@"Cancel"];

	NSWindow* window = NSApp.keyWindow ?: NSApp.mainWindow;
	if(window)
	{
		[alert beginSheetModalForWindow:window completionHandler:^(NSModalResponse response) {
			if(response == NSAlertFirstButtonReturn)
			{
				[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:uriString]];
				[self pollAuthStatusWithRetries:20 interval:3.0];
			}
		}];
	}
	else
	{
		if([alert runModal] == NSAlertFirstButtonReturn)
		{
			[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:uriString]];
			[self pollAuthStatusWithRetries:20 interval:3.0];
		}
	}
}

- (void)pollAuthStatusWithRetries:(NSInteger)remaining interval:(NSTimeInterval)interval
{
	if(remaining <= 0)
	{
		[self log:@"Auth polling timed out"];
		return;
	}

	dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(interval * NSEC_PER_SEC)), dispatch_get_main_queue(), ^{
		[self log:[NSString stringWithFormat:@"Polling auth status (%ld retries left)", (long)remaining]];
		[_client sendCustomRequest:@"checkStatus" params:@{} completion:^(id result) {
			if(![result isKindOfClass:[NSDictionary class]])
			{
				[self pollAuthStatusWithRetries:remaining - 1 interval:interval];
				return;
			}

			NSString* status = result[@"status"];
			if([status isEqualToString:@"OK"] || [status isEqualToString:@"AlreadySignedIn"])
			{
				self->_username = result[@"user"];
				self->_status = CopilotStatusReady;
				self->_restartCount = 0;
				[self log:[NSString stringWithFormat:@"Authenticated as %@", self->_username ?: @"(unknown)"]];
				[NSNotificationCenter.defaultCenter postNotificationName:CopilotStatusDidChangeNotification object:self];
			}
			else
			{
				[self pollAuthStatusWithRetries:remaining - 1 interval:interval];
			}
		}];
	});
}

// MARK: - Static Helpers

static NSString* uriForDocument(OakDocument* doc)
{
	if(!doc.path)
		return nil;
	return [NSURL fileURLWithPath:doc.path].absoluteString;
}

static NSString* languageIdForDocument(OakDocument* doc)
{
	return LSPLanguageIdForExtension(doc.path.pathExtension);
}

// MARK: - Telemetry

- (void)sendAcceptanceTelemetry:(NSDictionary*)completionItem
{
	if(!_client.initialized || !completionItem)
		return;

	// Native server: completion item has "command" with command name + arguments
	NSDictionary* command = completionItem[@"command"];
	if(command)
	{
		NSString* cmd = command[@"command"];
		NSArray* args = command[@"arguments"];
		if(cmd)
			[_client executeCommand:cmd arguments:args completion:nil];
	}
}

- (void)sendDidShowCompletion:(NSDictionary*)completionItem
{
	if(!_client.initialized || !completionItem)
		return;

	[_client sendCustomNotification:@"textDocument/didShowCompletion" params:@{@"item": completionItem}];
}

- (void)flushPendingDocuments
{
	if(_pendingDocuments.count == 0)
		return;

	[self log:[NSString stringWithFormat:@"Flushing %lu pending documents", (unsigned long)_pendingDocuments.count]];
	for(OakDocument* doc in [_pendingDocuments allObjects])
	{
		NSString* uri = uriForDocument(doc);
		if(uri)
			[self sendDidOpenForDocument:doc uri:uri];
	}
	[_pendingDocuments removeAllObjects];
}

// MARK: - Document Sync

- (void)documentDidOpen:(OakDocument*)document
{
	if(document.disableLanguageServices)
		return;
	[self ensureClientForDocument:document];
	if(!_client)
		return;

	NSString* uri = uriForDocument(document);
	if(!uri)
		return;

	if(!_client.initialized)
	{
		[_pendingDocuments addObject:document];
		[self log:[NSString stringWithFormat:@"Queued pending document: %@", uri.lastPathComponent]];
		return;
	}

	[self sendDidOpenForDocument:document uri:uri];
}

- (void)sendDidOpenForDocument:(OakDocument*)document uri:(NSString*)uri
{
	if([_openURIs containsObject:uri])
		return;

	[_openURIs addObject:uri];
	_documentVersions[uri] = @1;

	NSDictionary* params = @{
		@"textDocument": @{
			@"uri":        uri,
			@"languageId": languageIdForDocument(document),
			@"version":    @1,
			@"text":       document.content ?: @"",
		}
	};
	[_client sendCustomNotification:@"textDocument/didOpen" params:params];
	[self log:[NSString stringWithFormat:@"didOpen %@ (%@)", uri.lastPathComponent, languageIdForDocument(document)]];
}

- (void)documentDidChange:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || ![_openURIs containsObject:uri])
		return;

	[_changeTimers[uri] invalidate];

	NSTimer* timer = [NSTimer scheduledTimerWithTimeInterval:0.3
	                                                 repeats:NO
	                                                   block:^(NSTimer* t) {
		[self flushChangeForDocument:document uri:uri];
	}];
	_changeTimers[uri] = timer;
}

- (void)flushChangeForDocument:(OakDocument*)document uri:(NSString*)uri
{
	NSInteger version = [_documentVersions[uri] integerValue] + 1;
	_documentVersions[uri] = @(version);

	NSDictionary* params = @{
		@"textDocument": @{
			@"uri":     uri,
			@"version": @(version),
		},
		@"contentChanges": @[ @{ @"text": document.content ?: @"" } ],
	};
	[_client sendCustomNotification:@"textDocument/didChange" params:params];
}

- (void)documentDidSave:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri || ![_openURIs containsObject:uri])
		return;

	// Flush any pending debounced change before the save notification
	NSTimer* pending = _changeTimers[uri];
	if(pending.valid)
	{
		[pending invalidate];
		[self flushChangeForDocument:document uri:uri];
	}
	[_changeTimers removeObjectForKey:uri];

	NSDictionary* params = @{
		@"textDocument": @{ @"uri": uri }
	};
	[_client sendCustomNotification:@"textDocument/didSave" params:params];
}

- (void)documentWillClose:(OakDocument*)document
{
	NSString* uri = uriForDocument(document);
	if(!uri)
		return;

	[_changeTimers[uri] invalidate];
	[_changeTimers removeObjectForKey:uri];
	[_openURIs removeObject:uri];
	[_documentVersions removeObjectForKey:uri];

	if(!_client.initialized)
		return;

	NSDictionary* params = @{
		@"textDocument": @{ @"uri": uri }
	};
	[_client sendCustomNotification:@"textDocument/didClose" params:params];
}

- (void)documentDidFocus:(OakDocument*)document
{
	if(document.disableLanguageServices)
		return;
	[self ensureClientForDocument:document];
	if(!_client)
		return;

	if(!_client.initialized)
		return;

	NSString* uri = uriForDocument(document);
	if(!uri)
		return;

	NSDictionary* params = @{
		@"textDocument": @{ @"uri": uri }
	};
	[_client sendCustomNotification:@"textDocument/didFocus" params:params];
}

// MARK: - Completion

- (int)requestCompletionForDocument:(OakDocument*)document
                               line:(NSUInteger)line
                          character:(NSUInteger)character
                         completion:(void(^)(NSArray<NSDictionary*>* items))callback
{
	NSString* uri = uriForDocument(document);
	if(!uri || !_client.initialized)
	{
		[self log:[NSString stringWithFormat:@"Cannot request completion: uri=%@ initialized=%d", uri, _client.initialized]];
		if(callback)
			callback(nil);
		return 0;
	}

	if(![_openURIs containsObject:uri])
	{
		[self log:[NSString stringWithFormat:@"Document not open on server, opening now: %@", uri.lastPathComponent]];
		[self sendDidOpenForDocument:document uri:uri];
	}

	// Flush any pending debounced change so the server sees the latest content
	NSTimer* pending = _changeTimers[uri];
	if(pending.valid)
	{
		[pending invalidate];
		[self flushChangeForDocument:document uri:uri];
		[_changeTimers removeObjectForKey:uri];
	}

	[self log:[NSString stringWithFormat:@"Requesting inlineCompletion at %lu:%lu %@", (unsigned long)line, (unsigned long)character, uri.lastPathComponent]];

	NSDictionary* params = @{
		@"textDocument": @{@"uri": uri},
		@"position":     @{@"line": @(line), @"character": @(character)},
		@"context":      @{@"triggerKind": @1},
	};

	int requestId = [_client sendCustomRequest:@"textDocument/inlineCompletion"
	                                    params:params
	                                completion:^(id result) {
		NSArray* items = nil;
		if([result isKindOfClass:[NSDictionary class]])
			items = result[@"items"];
		else if([result isKindOfClass:[NSArray class]])
			items = result;

		[self log:[NSString stringWithFormat:@"Completion result: %lu items", (unsigned long)items.count]];

		if(callback)
			callback(items);
	}];
	return requestId;
}

- (void)cancelCompletionRequest:(int)requestId
{
	if(requestId && _client.initialized)
		[_client cancelRequest:requestId];
}

// MARK: - Logging

- (void)log:(NSString*)message
{
	NSLog(@"[Copilot] %@", message);
	[NSNotificationCenter.defaultCenter postNotificationName:CopilotLogNotification
	                                                  object:self
	                                                userInfo:@{@"message": message}];
}

@end
