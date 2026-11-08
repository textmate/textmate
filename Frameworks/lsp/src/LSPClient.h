#ifndef LSP_CLIENT_H_POC
#define LSP_CLIENT_H_POC

#import <document/OakDocument.h>

@class LSPClient;

@protocol LSPClientDelegate <NSObject>
- (void)lspClient:(LSPClient*)client didReceiveDiagnostics:(NSArray<NSDictionary*>*)diagnostics forDocumentURI:(NSString*)uri;
@optional
- (void)lspClientDidTerminate:(LSPClient*)client;
- (void)lspClient:(LSPClient*)client didReceiveApplyEditRequest:(NSDictionary*)workspaceEdit requestId:(id)requestId;
- (void)lspClientDidInitialize:(LSPClient*)client;
- (id)lspClient:(LSPClient*)client handleServerRequest:(NSString*)method params:(NSDictionary*)params;
- (void)lspClient:(LSPClient*)client didReceiveNotification:(NSString*)method params:(NSDictionary*)params;
- (void)lspClient:(LSPClient*)client didRequestShowDocument:(NSString*)path takeFocus:(BOOL)takeFocus;
- (NSSet<NSString*>*)lspClientOpenDocumentPaths:(LSPClient*)client;
@end

extern NSString* const LSPLogNotification;
extern NSString* const LSPShowMessageNotification;
extern NSString* const LSPProgressNotification;
extern NSString* const LSPShowMessageRequestNotification;

@interface LSPClient : NSObject
@property (nonatomic, weak) id<LSPClientDelegate> delegate;
@property (nonatomic, readonly) NSString* serverName;
@property (nonatomic, readonly) BOOL initialized;
@property (nonatomic, readonly) BOOL running;
@property (nonatomic, readonly) BOOL indexing;
@property (nonatomic, readonly) BOOL documentFormattingProvider;
@property (nonatomic, readonly) BOOL documentRangeFormattingProvider;
@property (nonatomic, readonly) BOOL completionResolveProvider;
@property (nonatomic, readonly) BOOL renameProvider;
@property (nonatomic, readonly) BOOL codeActionProvider;
@property (nonatomic, readonly) BOOL codeActionResolveProvider;
@property (nonatomic, readonly) NSArray<NSString*>* executeCommands;
- (instancetype)initWithCommand:(NSString*)command arguments:(NSArray<NSString*>*)arguments workingDirectory:(NSString*)workingDirectory initOptions:(NSString*)initOptionsJSON;
- (void)openDocument:(OakDocument*)document languageId:(NSString*)languageId;
- (void)documentDidChange:(OakDocument*)document version:(int)version;
- (void)documentDidSave:(OakDocument*)document;
- (void)closeDocument:(OakDocument*)document;
- (void)shutdown;
- (void)requestCompletionForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestDefinitionForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (int)requestHoverForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback;
- (void)requestReferencesForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestFormattingForURI:(NSString*)uri tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestRangeFormattingForURI:(NSString*)uri startLine:(NSUInteger)startLine startCharacter:(NSUInteger)startCharacter endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)resolveCompletionItem:(NSDictionary*)item completion:(void(^)(NSDictionary*))callback;
- (void)prepareRenameForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback;
- (void)requestRenameForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character newName:(NSString*)newName completion:(void(^)(NSDictionary*))callback;
- (void)requestCodeActionsForURI:(NSString*)uri line:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter diagnostics:(NSArray<NSDictionary*>*)diagnostics completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)resolveCodeAction:(NSDictionary*)codeAction completion:(void(^)(NSDictionary*))callback;
- (void)executeCommand:(NSString*)command arguments:(NSArray*)arguments completion:(void(^)(id))callback;
- (void)cancelRequest:(int)requestId;
- (void)respondToApplyEdit:(id)requestId applied:(BOOL)applied failureReason:(NSString*)reason;
- (void)respondToShowMessageRequest:(id)requestId action:(NSDictionary*)action;

// Static capabilities for the read-only language queries exposed to editor clients.
- (BOOL)supportsLanguageQuery:(NSString*)method;

// Generic JSON-RPC methods for non-standard LSP extensions (e.g., Copilot)
- (int)sendCustomRequest:(NSString*)method params:(NSDictionary*)params completion:(void(^)(id))callback;
- (void)sendCustomNotification:(NSString*)method params:(NSDictionary*)params;
@end

// Shared extension → LSP language ID mapping used by LSPManager and CopilotManager
NSString* LSPLanguageIdForExtension(NSString* ext);

#endif /* LSP_CLIENT_H_POC */
