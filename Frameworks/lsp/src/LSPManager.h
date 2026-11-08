#ifndef LSP_MANAGER_H_POC
#define LSP_MANAGER_H_POC

#import <document/OakDocument.h>

extern NSString* const LSPDiagnosticsDidChangeNotification;
extern NSString* const LSPServerStatusDidChangeNotification;

@interface LSPManager : NSObject
+ (instancetype)sharedManager;
- (void)documentDidOpen:(OakDocument*)document;
- (void)documentDidChange:(OakDocument*)document;
- (void)documentDidSave:(OakDocument*)document;
- (void)documentWillClose:(OakDocument*)document;
- (void)shutdownAll;
- (void)flushPendingChangesForDocument:(OakDocument*)document;
- (void)requestCompletionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character prefix:(NSString*)prefix completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestDefinitionForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (int)requestHoverForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback;
- (void)cancelRequest:(int)requestId forDocument:(OakDocument*)document;
- (void)requestReferencesForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestFormattingForDocument:(OakDocument*)document tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)requestRangeFormattingForDocument:(OakDocument*)document startLine:(NSUInteger)startLine startCharacter:(NSUInteger)startCharacter endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter tabSize:(NSUInteger)tabSize insertSpaces:(BOOL)insertSpaces completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)resolveCompletionItem:(NSDictionary*)item forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback;
- (BOOL)serverSupportsCompletionResolveForDocument:(OakDocument*)document;
- (BOOL)serverSupportsFormattingForDocument:(OakDocument*)document;
- (BOOL)serverSupportsRangeFormattingForDocument:(OakDocument*)document;
- (BOOL)serverSupportsRenameForDocument:(OakDocument*)document;
- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document atLine:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter;
- (void)requestPrepareRenameForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character completion:(void(^)(NSDictionary*))callback;
- (void)requestRenameForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character newName:(NSString*)newName completion:(void(^)(NSDictionary*))callback;
- (BOOL)serverSupportsCodeActionsForDocument:(OakDocument*)document;
- (BOOL)serverSupportsCodeActionResolveForDocument:(OakDocument*)document;
- (void)requestCodeActionsForDocument:(OakDocument*)document line:(NSUInteger)line character:(NSUInteger)character endLine:(NSUInteger)endLine endCharacter:(NSUInteger)endCharacter completion:(void(^)(NSArray<NSDictionary*>*))callback;
- (void)resolveCodeAction:(NSDictionary*)codeAction forDocument:(OakDocument*)document completion:(void(^)(NSDictionary*))callback;
- (void)executeCommand:(NSString*)command arguments:(NSArray*)arguments forDocument:(OakDocument*)document completion:(void(^)(id))callback;
// Read-only, capability-checked requests on an existing server. Nil is a
// request failure; NSNull is a successful empty response. Never starts a server.
- (dispatch_block_t)requestLanguageQuery:(NSString*)method forDocument:(OakDocument*)document params:(NSDictionary*)params completion:(void(^)(id result, NSString* errorCode))callback;
- (BOOL)hasClientForDocument:(OakDocument*)document;
- (NSDictionary<NSString*, NSNumber*>*)diagnosticCountsForDocument:(OakDocument*)document;
- (NSArray<NSDictionary*>*)diagnosticsForDocument:(OakDocument*)document;
// Cached report only. A received empty report differs from no report yet.
// Call on the main thread; does not start or contact a language server.
- (NSDictionary*)diagnosticReportForDocument:(OakDocument*)document;
- (NSString*)serverStatusForDocument:(OakDocument*)document;
- (NSString*)serverNameForDocument:(OakDocument*)document;
- (void)restartServerForDocument:(OakDocument*)document;
- (void)reindexWorkspaceForDocument:(OakDocument*)document;
@end

#endif /* LSP_MANAGER_H_POC */
