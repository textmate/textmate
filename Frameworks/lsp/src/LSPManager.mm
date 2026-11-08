#import "LSPManager.h"
#import "LSPClient.h"
#import <settings/settings.h>
#import <text/types.h>
#import <io/path.h>
#import <ns/ns.h>
#import <OakFoundation/NSString Additions.h>

NSString* const LSPDiagnosticsDidChangeNotification = @"LSPDiagnosticsDidChange";
NSString* const LSPServerStatusDidChangeNotification = @"LSPServerStatusDidChange";

static NSDictionary<NSString*, NSString*>* scopeToLanguageId ()
{
	static NSDictionary* map = @{
		@"source.php"           : @"php",
		@"source.c"             : @"c",
		@"source.c++"          : @"cpp",
		@"source.objc"          : @"objective-c",
		@"source.objc++"       : @"objective-cpp",
		@"source.js"            : @"javascript",
		@"source.ts"            : @"typescript",
		@"source.python"        : @"python",
		@"source.go"            : @"go",
		@"source.rust"          : @"rust",
		@"source.ruby"          : @"ruby",
		@"source.java"          : @"java",
		@"source.json"          : @"json",
		@"source.css"           : @"css",
		@"source.html"          : @"html",
		@"source.shell"         : @"shellscript",
		@"source.yaml"          : @"yaml",
		@"source.swift"         : @"swift",
		@"text.html.markdown"   : @"markdown",
	};
	return map;
}

static NSString* languageIdForScope (NSString* fileType)
{
	if(!fileType)
		return @"plaintext";

	NSDictionary* map = scopeToLanguageId();

	// Try progressively shorter scope prefixes for best match
	NSString* scope = fileType;
	while(scope.length > 0)
	{
		NSString* langId = map[scope];
		if(langId)
			return langId;

		NSRange lastDot = [scope rangeOfString:@"." options:NSBackwardsSearch];
		if(lastDot.location == NSNotFound)
			break;
		scope = [scope substringToIndex:lastDot.location];
	}

	// Fallback: strip "source." prefix and use remainder
	if([fileType hasPrefix:@"source."])
		return [fileType substringFromIndex:7];

	return @"plaintext";
}

// Uses shared LSPLanguageIdForExtension() from LSPClient.mm

static std::vector<std::string> const& workspaceMarkers ()
{
	static std::vector<std::string> const markers = {
		".git", "composer.json", "package.json", "tsconfig.json",
		"CMakeLists.txt", "compile_commands.json", "go.mod",
		"Cargo.toml", "pyproject.toml", "setup.py", ".clangd"
	};
	return markers;
}

static std::string detectWorkspaceRoot (std::string const& filePath)
{
	std::string dir = path::parent(filePath);
	std::string previousDir;

	while(dir != previousDir && dir != "/")
	{
		for(auto const& marker : workspaceMarkers())
		{
			if(path::exists(path::join(dir, marker)))
				return dir;
		}
		previousDir = dir;
		dir = path::parent(dir);
	}

	// No marker found — use file's directory
	return path::parent(filePath);
}

@interface LSPManager () <LSPClientDelegate>
{
	NSMutableDictionary<NSString*, LSPClient*>*              _clients;
	NSMutableDictionary<NSUUID*, LSPClient*>*                _documentClients;
	NSMutableDictionary<NSUUID*, NSNumber*>*                 _documentVersions;
	NSMutableSet<NSUUID*>*                                   _openDocuments;
	NSMutableDictionary<NSUUID*, NSTimer*>*                  _changeTimers;
	NSMutableDictionary<NSString*, NSArray<NSDictionary*>*>* _diagnosticsByURI;
	NSMutableDictionary<NSString*, NSString*>*               _diagnosticReportIdentifiers;
	NSMutableSet<NSString*>* _clearCacheRoots;
}
@end

@implementation LSPManager
+ (instancetype)sharedManager
{
	static LSPManager* instance;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		instance = [LSPManager new];
	});
	return instance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_clients            = [NSMutableDictionary new];
		_documentClients    = [NSMutableDictionary new];
		_documentVersions   = [NSMutableDictionary new];
		_openDocuments      = [NSMutableSet new];
		_changeTimers       = [NSMutableDictionary new];
		_diagnosticsByURI   = [NSMutableDictionary new];
		_diagnosticReportIdentifiers = [NSMutableDictionary new];
		_clearCacheRoots    = [NSMutableSet new];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(applicationWillTerminate:) name:NSApplicationWillTerminateNotification object:nil];
	}
	return self;
}

- (void)applicationWillTerminate:(NSNotification*)notification
{
	[self shutdownAll];
}

- (NSString*)rootForClient:(LSPClient*)client
{
	for(NSString* root in _clients)
	{
		if(_clients[root] == client)
			return root;
	}
	return nil;
}

- (LSPClient*)clientForDocument:(OakDocument*)document
{
	if(document.disableLanguageServices || !document.path)
		return nil;

	std::string filePath  = to_s(document.path);
	std::string fileType  = to_s(document.fileType);
	std::string directory = to_s(document.directory ?: [document.path stringByDeletingLastPathComponent]);

	settings_t settings = settings_for_path(filePath, fileType, directory);

	std::string lspCommand = settings.get("lspCommand", "");
	if(lspCommand.empty())
		return nil;

	bool lspEnabled = settings.get("lspEnabled", true);
	if(!lspEnabled)
		return nil;

	// Determine workspace root
	std::string rootPath = settings.get("lspRootPath", "");
	if(rootPath.empty())
		rootPath = detectWorkspaceRoot(filePath);

	NSString* root = to_ns(rootPath);
	LSPClient* client = _clients[root];
	if(client)
		return client;

	// Parse command: first whitespace-delimited token is executable, rest are args
	std::vector<std::string> parts = path::unescape(lspCommand);
	if(parts.empty())
		return nil;

	NSString* executable = to_ns(parts[0]);
	NSMutableArray<NSString*>* args = [NSMutableArray new];
	for(size_t i = 1; i < parts.size(); ++i)
		[args addObject:to_ns(parts[i])];

	std::string initOpts = settings.get("lspInitOptions", "");
	NSString* initOptsJSON = initOpts.empty() ? nil : to_ns(initOpts);

	if([_clearCacheRoots containsObject:root])
	{
		[_clearCacheRoots removeObject:root];
		// Merge clearCache:true into initializationOptions for servers like Intelephense
		if(initOptsJSON.length)
		{
			NSData* data = [initOptsJSON dataUsingEncoding:NSUTF8StringEncoding];
			NSMutableDictionary* opts = [[NSJSONSerialization JSONObjectWithData:data options:0 error:nil] mutableCopy];
			if(!opts)
				opts = [NSMutableDictionary new];
			opts[@"clearCache"] = @YES;
			NSData* merged = [NSJSONSerialization dataWithJSONObject:opts options:0 error:nil];
			initOptsJSON = [[NSString alloc] initWithData:merged encoding:NSUTF8StringEncoding];
		}
		else
		{
			initOptsJSON = @"{\"clearCache\":true}";
		}
	}

	client = [[LSPClient alloc] initWithCommand:executable arguments:args workingDirectory:root initOptions:initOptsJSON];
	client.delegate = self;
	_clients[root] = client;
	[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
	return client;
}

- (void)documentDidOpen:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	if([_openDocuments containsObject:docId])
		return;

	NSString* langId = languageIdForScope(document.fileType);
	if([langId isEqualToString:@"plaintext"] && document.path)
		langId = LSPLanguageIdForExtension(document.path.pathExtension);

	// Don't connect plaintext files — prevents unscoped lspCommand
	// from launching a server for every file type
	if([langId isEqualToString:@"plaintext"])
		return;

	LSPClient* client = [self clientForDocument:document];
	if(!client)
		return;

	[_openDocuments addObject:docId];
	_documentClients[docId]  = client;
	_documentVersions[docId] = @1;

	[client openDocument:document languageId:langId];
}

- (void)documentDidChange:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	if(![_openDocuments containsObject:docId])
		return;

	[_changeTimers[docId] invalidate];

	__weak LSPManager* weakSelf = self;
	_changeTimers[docId] = [NSTimer scheduledTimerWithTimeInterval:0.3 repeats:NO block:^(NSTimer* timer) {
		[weakSelf sendDidChangeForDocument:document];
	}];
}

- (void)sendDidChangeForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	[_changeTimers removeObjectForKey:docId];

	LSPClient* client = _documentClients[docId];
	if(!client)
		return;

	int version = [_documentVersions[docId] intValue] + 1;
	_documentVersions[docId] = @(version);
	[client documentDidChange:document version:version];
}

- (void)documentDidSave:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	if(![_openDocuments containsObject:docId])
		return;

	// Flush any pending change notification
	if(_changeTimers[docId])
	{
		[_changeTimers[docId] invalidate];
		[self sendDidChangeForDocument:document];
	}

	LSPClient* client = _documentClients[docId];
	if(client)
		[client documentDidSave:document];
}

- (void)documentWillClose:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	if(![_openDocuments containsObject:docId])
		return;

	[_changeTimers[docId] invalidate];
	[_changeTimers removeObjectForKey:docId];

	LSPClient* client = _documentClients[docId];
	if(client)
		[client closeDocument:document];

	[_openDocuments removeObject:docId];
	[_documentClients removeObjectForKey:docId];
	[_documentVersions removeObjectForKey:docId];

	NSString* path = document.path;
	if(path)
	{
		NSURL* fileURL = [NSURL fileURLWithPath:path];
		[_diagnosticsByURI removeObjectForKey:fileURL.absoluteString];
		[_diagnosticReportIdentifiers removeObjectForKey:fileURL.absoluteString];
	}
}

- (void)shutdownAll
{
	for(NSTimer* timer in _changeTimers.allValues)
		[timer invalidate];
	[_changeTimers removeAllObjects];

	for(LSPClient* client in _clients.allValues)
		[client shutdown];

	[_clients removeAllObjects];
	[_documentClients removeAllObjects];
	[_documentVersions removeAllObjects];
	[_openDocuments removeAllObjects];
}

- (void)flushPendingChangesForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	if(![_openDocuments containsObject:docId])
		return;

	// Cancel any pending debounce timer
	[_changeTimers[docId] invalidate];
	[_changeTimers removeObjectForKey:docId];

	// Always send current content so server has latest state
	LSPClient* client = _documentClients[docId];
	if(!client)
		return;

	int version = [_documentVersions[docId] intValue] + 1;
	_documentVersions[docId] = @(version);
	[client documentDidChange:document version:version];
}

- (void)requestCompletionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character prefix:(NSString*)prefix completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestCompletionForURI:uri line:line character:character completion:^(NSArray<NSDictionary*>* suggestions) {
		if(callback)
			callback(suggestions);
	}];
}

- (dispatch_block_t)requestLanguageQuery:(NSString*)method forDocument:(OakDocument*)document params:(NSDictionary*)params completion:(void(^)(id, NSString*))callback
{
	NSAssert(NSThread.isMainThread, @"Language queries run on the main thread");
	LSPClient* client = _documentClients[document.identifier];
	if(!client || !client.running || ![_openDocuments containsObject:document.identifier]) { callback(nil, @"server_unavailable"); return nil; }
	if(!client.initialized) { callback(nil, @"server_starting"); return nil; }
	if(![client supportsLanguageQuery:method]) { callback(nil, @"unsupported_capability"); return nil; }
	[self flushPendingChangesForDocument:document];
	int requestId = [client sendCustomRequest:method params:params completion:^(id result){
		callback(result, result ? nil : @"language_request_failed");
	}];
	__weak LSPClient* originalClient = client;
	return ^{ [originalClient cancelRequest:requestId]; };
}

- (void)requestDefinitionForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestDefinitionForURI:uri line:line character:character completion:callback];
}

- (int)requestHoverForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(nil);
		return 0;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(nil);
		return 0;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	return [client requestHoverForURI:uri line:line character:character completion:callback];
}

- (void)cancelRequest:(int)requestId forDocument:(OakDocument*)document
{
	if(requestId == 0)
		return;

	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	[client cancelRequest:requestId];
}

- (void)requestReferencesForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(@[]);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestReferencesForURI:uri line:line character:character completion:callback];
}

- (void)requestFormattingForDocument:(OakDocument*)document tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client || !client.documentFormattingProvider)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestFormattingForURI:uri tabSize:tabSize insertSpaces:insertSpaces completion:callback];
}

- (void)requestRangeFormattingForDocument:(OakDocument*)document startLine:(NSUInteger)startLine startCharacter:(NSUInteger)startCharacter endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client || !client.documentRangeFormattingProvider)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestRangeFormattingForURI:uri startLine:startLine startCharacter:startCharacter endLine:endLine endCharacter:endCharacter tabSize:tabSize insertSpaces:insertSpaces completion:callback];
}

- (void)resolveCompletionItem:(NSDictionary*)item forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(nil);
		return;
	}

	[client resolveCompletionItem:item completion:^(NSDictionary* resolved) {
		if(callback)
			callback(resolved);
	}];
}

- (BOOL)serverSupportsCompletionResolveForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.completionResolveProvider;
}

- (BOOL)serverSupportsFormattingForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.documentFormattingProvider;
}

- (BOOL)serverSupportsRangeFormattingForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.documentRangeFormattingProvider;
}

- (BOOL)serverSupportsRenameForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.renameProvider;
}

- (void)requestPrepareRenameForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary* _Nullable))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client prepareRenameForURI:uri line:line character:character completion:callback];
}

- (void)requestRenameForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character newName:(NSString*)newName completion:(void(^)(NSDictionary* _Nullable))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback)
			callback(nil);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	[client requestRenameForURI:uri line:line character:character newName:newName completion:callback];
}

- (BOOL)serverSupportsCodeActionsForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.codeActionProvider;
}

- (BOOL)serverSupportsCodeActionResolveForDocument:(OakDocument*)document
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	return client && client.codeActionResolveProvider;
}

- (void)requestCodeActionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter completion:(void(^)(NSArray<NSDictionary*>*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback) callback(nil);
		return;
	}

	NSString* path = document.path;
	if(!path)
	{
		if(callback) callback(nil);
		return;
	}

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;

	NSArray<NSDictionary*>* diagnostics = [self diagnosticsForDocument:document atLine:line character:character endLine:endLine endCharacter:endCharacter];

	[self flushPendingChangesForDocument:document];
	[client requestCodeActionsForURI:uri line:line character:character endLine:endLine endCharacter:endCharacter diagnostics:diagnostics completion:callback];
}

- (void)resolveCodeAction:(NSDictionary*)codeAction forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback) callback(nil);
		return;
	}
	[client resolveCodeAction:codeAction completion:callback];
}

- (void)executeCommand:(NSString*)command arguments:(NSArray*)arguments forDocument:(OakDocument*)document completion:(void(^)(id))callback
{
	NSUUID* docId = document.identifier;
	LSPClient* client = _documentClients[docId];
	if(!client)
	{
		if(callback) callback(nil);
		return;
	}
	[client executeCommand:command arguments:arguments completion:callback];
}

- (BOOL)hasClientForDocument:(OakDocument*)document
{
	return document && _documentClients[document.identifier] != nil;
}

- (NSDictionary<NSString*, NSNumber*>*)diagnosticCountsForDocument:(OakDocument*)document
{
	NSUInteger errors = 0, warnings = 0, info = 0;
	NSString* path = document.path;
	if(path)
	{
		NSURL* fileURL = [NSURL fileURLWithPath:path];
		NSArray<NSDictionary*>* diags = _diagnosticsByURI[fileURL.absoluteString];
		for(NSDictionary* diag in diags)
		{
			switch([diag[@"severity"] intValue])
			{
				case 1:  errors++;   break;
				case 2:  warnings++; break;
				case 3:
				case 4:  info++;     break;
				default: break;
			}
		}
	}
	return @{ @"errors": @(errors), @"warnings": @(warnings), @"info": @(info) };
}

- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document
{
	NSString* path = document.path;
	if(!path)
		return @[];

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	return _diagnosticsByURI[fileURL.absoluteString] ?: @[];
}

- (NSDictionary*)diagnosticReportForDocument:(OakDocument*)document
{
	NSAssert(NSThread.isMainThread, @"Capture diagnostics on the main thread");
	NSString* uri = document.path ? [NSURL fileURLWithPath:document.path].absoluteString : nil;
	NSArray* diagnostics = uri ? _diagnosticsByURI[uri] : nil;
	NSString* serverStatus = uri ? ([self serverStatusForDocument:document] ?: @"unavailable") : @"unavailable";
	return @{ @"diagnostics": [diagnostics copy] ?: @[],
	          @"reportId": (uri ? _diagnosticReportIdentifiers[uri] : nil) ?: (id)NSNull.null,
	          @"status": diagnostics ? @"reported" : ([serverStatus isEqual:@"unavailable"] ? @"unavailable" : @"pending"),
	          @"serverStatus": serverStatus,
	          @"pendingChanges": @(_changeTimers[document.identifier] != nil),
	          // LSP diagnostics do not always carry a document version. The
	          // current buffer revision must not be presented as their revision.
	          @"freshness": @"unknown" };
}

- (NSString*)serverStatusForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return nil;
	if(client.initialized && client.indexing)
		return @"indexing";
	if(client.initialized)
		return @"running";
	if(client.running)
		return @"starting";
	return nil;
}

- (NSString*)serverNameForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return nil;

	NSString* path = document.path;
	if(!path)
		return nil;

	std::string filePath  = to_s(path);
	std::string fileType  = to_s(document.fileType);
	std::string directory = to_s(document.directory ?: [path stringByDeletingLastPathComponent]);

	settings_t settings = settings_for_path(filePath, fileType, directory);
	std::string lspCommand = settings.get("lspCommand", "");
	if(lspCommand.empty())
		return nil;

	std::vector<std::string> parts = path::unescape(lspCommand);
	if(parts.empty())
		return nil;

	return [[NSString stringWithCxxString:parts[0]] lastPathComponent];
}

- (void)restartServerForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return;

	// Collect affected documents and clean up state synchronously
	// so documentDidOpen: can re-register them with a fresh client
	NSMutableArray<OakDocument*>* affectedDocs = [NSMutableArray new];
	for(NSUUID* docId in _documentClients)
	{
		if(_documentClients[docId] == client)
		{
			OakDocument* doc = [OakDocument documentWithIdentifier:docId];
			if(doc && doc.isLoaded)
				[affectedDocs addObject:doc];
		}
	}

	// Remove client from _clients so a new one will be created
	NSString* rootToRemove = [self rootForClient:client];
	if(rootToRemove)
		[_clients removeObjectForKey:rootToRemove];

	// Dissociate documents before shutdown
	for(OakDocument* doc in affectedDocs)
	{
		NSUUID* docId = doc.identifier;
		[_changeTimers[docId] invalidate];
		[_changeTimers removeObjectForKey:docId];
		[_documentClients removeObjectForKey:docId];
		[_documentVersions removeObjectForKey:docId];
		[_openDocuments removeObject:docId];
	}

	// Old process shuts down asynchronously; lspClientDidTerminate: will be a no-op
	[client shutdown];

	// Re-open with fresh client immediately
	for(OakDocument* doc in affectedDocs)
		[self documentDidOpen:doc];

	[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
}

- (void)reindexWorkspaceForDocument:(OakDocument*)document
{
	LSPClient* client = _documentClients[document.identifier];
	if(!client)
		return;

	// Check if server advertises a reindex-like command
	for(NSString* cmd in client.executeCommands)
	{
		NSString* lower = cmd.lowercaseString;
		if([lower containsString:@"reindex"] || [lower containsString:@"index.workspace"] || [lower containsString:@"index-workspace"])
		{
			NSLog(@"[LSP:%@] Re-index: using server command '%@'", client.serverName, cmd);
			[client executeCommand:cmd arguments:nil completion:^(id result) {
				if(!result)
					NSLog(@"[LSP:%@] Re-index command '%@' failed or returned null", client.serverName, cmd);
			}];
			return;
		}
	}
	NSLog(@"[LSP:%@] Re-index: restarting with clearCache", client.serverName);

	// Fallback: restart with clearCache in initializationOptions
	NSString* rootToFlag = [self rootForClient:client];
	if(rootToFlag)
		[_clearCacheRoots addObject:rootToFlag];

	[self restartServerForDocument:document];
}

- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document atLine:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter
{
	NSString* path = document.path;
	if(!path)
		return @[];

	NSURL* fileURL = [NSURL fileURLWithPath:path];
	NSString* uri = fileURL.absoluteString;
	NSArray<NSDictionary*>* allDiags = _diagnosticsByURI[uri];
	if(!allDiags)
		return @[];

	NSMutableArray<NSDictionary*>* result = [NSMutableArray array];
	for(NSDictionary* diag in allDiags)
	{
		NSUInteger dLine    = [diag[@"line"] unsignedIntegerValue];
		NSUInteger dEndLine = [diag[@"endLine"] unsignedIntegerValue];

		// Simple line-range overlap check
		if(dEndLine >= line && dLine <= endLine)
			[result addObject:diag];
	}
	return result;
}

#pragma mark - LSPClientDelegate

- (void)lspClient:(LSPClient*)client didReceiveApplyEditRequest:(NSDictionary*)workspaceEdit requestId:(id)requestId
{
	dispatch_async(dispatch_get_main_queue(), ^{
		NSDictionary* userInfo = @{
			@"workspaceEdit": workspaceEdit,
			@"requestId": requestId ?: [NSNull null],
			@"client": client
		};
		[NSNotificationCenter.defaultCenter postNotificationName:@"LSPApplyEditRequest" object:self userInfo:userInfo];
	});
}

- (void)lspClientDidTerminate:(LSPClient*)client
{
	NSLog(@"[LSP:%@] Handling server termination, cleaning up client", client.serverName);

	// Find and remove the dead client from _clients
	NSString* rootToRemove = [self rootForClient:client];
	if(rootToRemove)
		[_clients removeObjectForKey:rootToRemove];

	// Dissociate all documents that were using this client
	NSMutableArray<NSUUID*>* docIdsToRemove = [NSMutableArray new];
	for(NSUUID* docId in _documentClients)
	{
		if(_documentClients[docId] == client)
			[docIdsToRemove addObject:docId];
	}

	for(NSUUID* docId in docIdsToRemove)
	{
		[_changeTimers[docId] invalidate];
		[_changeTimers removeObjectForKey:docId];
		[_documentClients removeObjectForKey:docId];
		[_documentVersions removeObjectForKey:docId];
		[_openDocuments removeObject:docId];
	}

	[NSNotificationCenter.defaultCenter postNotificationName:LSPServerStatusDidChangeNotification object:self];
}

- (void)lspClient:(LSPClient*)client didReceiveDiagnostics:(NSArray<NSDictionary*>*)diagnostics forDocumentURI:(NSString*)uri
{
	// Cache full diagnostics for codeAction requests
	_diagnosticsByURI[uri] = [diagnostics copy];
	_diagnosticReportIdentifiers[uri] = NSUUID.UUID.UUIDString;

	NSURL* url = [NSURL URLWithString:uri];
	NSString* filePath = url.path;
	if(!filePath)
		return;

	OakDocument* doc = [OakDocument documentWithPath:filePath];
	if(!doc || !doc.isLoaded)
		return;

	[doc removeAllMarksOfType:@"error"];
	[doc removeAllMarksOfType:@"warning"];
	[doc removeAllMarksOfType:@"note"];

	for(NSDictionary* diag in diagnostics)
	{
		NSNumber* severity = diag[@"severity"];
		NSString* message  = diag[@"message"];
		NSNumber* line     = diag[@"line"];

		if(!message || !line)
			continue;

		NSString* markType;
		switch(severity.intValue)
		{
			case 1:  markType = @"error";   break;
			case 2:  markType = @"warning"; break;
			default: markType = @"note";    break;
		}

		text::pos_t pos(line.unsignedIntegerValue, 0);
		[doc setMarkOfType:markType atPosition:pos content:message];
	}

	[NSNotificationCenter.defaultCenter postNotificationName:LSPDiagnosticsDidChangeNotification object:self userInfo:@{ @"uri": uri }];
}

- (NSSet<NSString*>*)lspClientOpenDocumentPaths:(LSPClient*)client
{
	NSMutableSet<NSString*>* paths = [NSMutableSet new];
	for(NSUUID* docId in _openDocuments)
	{
		if(_documentClients[docId] != client)
			continue;

		OakDocument* doc = [OakDocument documentWithIdentifier:docId];
		if(doc.path)
			[paths addObject:doc.path];
	}
	return paths;
}
@end
