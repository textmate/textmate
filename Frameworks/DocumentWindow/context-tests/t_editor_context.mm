#import <Cocoa/Cocoa.h>
#import <EditorContextProtocol.h>
#import "../src/EditorContextService.h"
#import "../src/EditorContextBuffer.h"
#import "../src/EditorContextDiagnostics.h"
#import "../src/EditorLanguageIntelligence.h"
#import "../src/EditorContextChanges.h"
#import "../src/BufferEditProposal.h"
#import "../src/ProjectChangesWindowController.h"
#import <selection/selection.h>
#import <document/OakDocument Private.h>
#import "../src/TerminalPanelViewController.h"
#import "../src/DocumentWindowController.h"
#import <OakTextView/OakTextView.h>
#import <document/OakDocument.h>
#import <lsp/LSPManager.h>
#import <lsp/LSPClient.h>
#include <sys/un.h>

#import "EditorContextTestAccess.h"

static NSDictionary* context_test_query(NSDictionary* environment, NSString* method = @"get_editor_context", NSDictionary* arguments = @{}) {
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    editor_context::configure_socket(fd);
    sockaddr_un address = {};
    address.sun_family = AF_UNIX;
    strlcpy(address.sun_path, [environment[@"TM_CONTEXT_SOCKET"] fileSystemRepresentation], sizeof(address.sun_path));
    OAK_ASSERT(connect(fd, (sockaddr*)&address, sizeof(address)) == 0);
    editor_context::write_json(fd, @{ @"session": environment[@"TM_CONTEXT_SESSION"], @"token": environment[@"TM_CONTEXT_TOKEN"],
                                     @"method": method, @"arguments": arguments });
    NSDictionary* result = editor_context::parse(editor_context::read_line(fd));
    close(fd);
    return result;
}

void test_editor_context_session_routing_and_revocation ()
{
    @autoreleasepool {
        __block NSDictionary* environment;
        NSUUID* session = NSUUID.UUID;
        dispatch_sync(dispatch_get_main_queue(), ^{
            environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* arguments){ return [EditorContextSnapshot response:@{ @"window": @"first" }]; }];
        });
        OAK_ASSERT([context_test_query(environment)[@"window"] isEqual:@"first"]);
        NSMutableDictionary* forged = [environment mutableCopy];
        forged[@"TM_CONTEXT_TOKEN"] = @"wrong";
        OAK_ASSERT(context_test_query(forged)[@"error"] != nil);
        dispatch_sync(dispatch_get_main_queue(), ^{
            NSDictionary* moved = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* arguments){ return [EditorContextSnapshot response:@{ @"window": @"second" }]; }];
            OAK_ASSERT([moved isEqual:environment]);
        });
        OAK_ASSERT([context_test_query(environment)[@"window"] isEqual:@"second"]);
        dispatch_sync(dispatch_get_main_queue(), ^{ [EditorContextService.sharedService revokeSession:session]; });
        OAK_ASSERT(context_test_query(environment)[@"error"] != nil);
    }
}

void test_editor_context_live_buffers_and_unicode ()
{
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block BOOL loaded = NO;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithString:@"one\nA😀B\nlast" fileType:@"text.plain" customName:@"context test"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            loaded = status == OakDocumentIOResultSuccess;
            dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC)) == 0);
    OAK_ASSERT(loaded);
    dispatch_sync(dispatch_get_main_queue(), ^{ @autoreleasepool {
        [window setDocuments:@[document]];
        [window setSelectedDocument:document];
        window.textView.selectionString = @"2:2-2:6";
        NSDictionary* context = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}];
        OAK_ASSERT([context[@"document"][@"documentId"] isEqual:document.identifier.UUIDString]);
        OAK_ASSERT([context[@"selections"][0][@"text"] isEqual:@"😀"]);
        OAK_ASSERT([context[@"cursor"][@"offset"] unsignedIntegerValue] == 7);
        window.textView.selectionString = @"2:6-2:2";
        context = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}];
        OAK_ASSERT([context[@"cursor"][@"offset"] unsignedIntegerValue] == 5);
        window.textView.selectionString = @"1:1-1:4&3:1-3:5";
        context = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}];
        OAK_ASSERT([context[@"selections"] count] == 2);
        OAK_ASSERT([context[@"selections"][0][@"text"] isEqual:@"one"]);
        OAK_ASSERT([context[@"selections"][1][@"text"] isEqual:@"last"]);
        NSDictionary* read = [window testEditorContextForMethod:@"read_document" arguments:@{ @"documentId": document.identifier.UUIDString, @"offset": @5, @"length": @2 }];
        OAK_ASSERT([read[@"text"] isEqual:@"😀"]);
        OAK_ASSERT([read[@"nextOffset"] unsignedIntegerValue] == 7);
        OAK_ASSERT(([window testEditorContextForMethod:@"read_document" arguments:@{ @"documentId": document.identifier.UUIDString, @"offset": @6 }][@"error"] != nil));
        OAK_ASSERT([window testEditorContextForMethod:@"read_document" arguments:@{ @"documentId": NSUUID.UUID.UUIDString }][@"error"] != nil);
        id revision = read[@"document"][@"revision"];
        document.content = @"changed without saving";
        read = [window testEditorContextForMethod:@"read_document" arguments:@{ @"documentId": document.identifier.UUIDString }];
        OAK_ASSERT([read[@"text"] isEqual:@"changed without saving"]);
        OAK_ASSERT(![read[@"document"][@"revision"] isEqual:revision]);
        document.content = @"ab\ncd\nef";
        window.textView.selectionString = @"1:1x3:2";
        context = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}];
        OAK_ASSERT([context[@"selections"] count] == 3);
        OAK_ASSERT([context[@"selections"][0][@"text"] isEqual:@"a"]);
        OAK_ASSERT([context[@"selections"][1][@"text"] isEqual:@"c"]);
        OAK_ASSERT([context[@"selections"][2][@"text"] isEqual:@"e"]);
        [window setSelectedDocument:nil];
        OAK_ASSERT([window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"] == NSNull.null);
        [window setDocuments:@[]];
        [document close];
        [window close];
        [DocumentWindowController enableSessionSave];
    } });
}

void test_editor_context_diagnostics_reports_and_window_boundary ()
{
    NSString* directory = [@"/tmp/tm-diagnostic-test-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:directory withIntermediateDirectories:YES attributes:nil error:nil]);
    NSString* path = [directory stringByAppendingPathComponent:@"a file 😀.txt"];
    OAK_ASSERT([@"A😀B\nlast\n" writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block OakDocument* foreign;
    __block EditorContextSnapshot* snapshot;
    __block BOOL loaded = NO;
    __block NSString* capturedReport;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithPath:path];
        document.fileType = @"text.plain";
        foreign = [OakDocument documentWithString:@"not in this window" fileType:@"text.plain" customName:@"foreign"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            loaded = status == OakDocumentIOResultSuccess;
            dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0 && loaded);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]];
        [window setSelectedDocument:document];
        LSPManager* manager = LSPManager.sharedManager;
        NSDictionary* result = [window testEditorContextForMethod:@"get_diagnostics" arguments:@{}];
        OAK_ASSERT([result[@"status"] isEqual:@"unavailable"]);
        OAK_ASSERT(result[@"reportId"] == NSNull.null);
        ContextDiagnosticTestClient* client = [ContextDiagnosticTestClient new];
        client.running = client.initialized = YES;
        NSMutableDictionary* clients = [manager valueForKey:@"documentClients"];
        clients[document.identifier] = client;
        clients[foreign.identifier] = client;
        OAK_ASSERT([[manager diagnosticReportForDocument:foreign][@"status"] isEqual:@"unavailable"]);
        [clients removeObjectForKey:foreign.identifier];
        result = [window testEditorContextForMethod:@"get_diagnostics" arguments:@{}];
        OAK_ASSERT([result[@"status"] isEqual:@"pending"]);
        OAK_ASSERT([result[@"serverStatus"] isEqual:@"running"]);
        NSString* uri = [NSURL fileURLWithPath:path].absoluteString;
        [(id<LSPClientDelegate>)manager lspClient:nil didReceiveDiagnostics:@[] forDocumentURI:uri];
        result = [window testEditorContextForMethod:@"get_diagnostics" arguments:@{}];
        OAK_ASSERT([result[@"status"] isEqual:@"reported"] && [result[@"totalCount"] integerValue] == 0);
        NSString* emptyReport = result[@"reportId"];
        NSMutableArray* diagnostics = [NSMutableArray array];
        for(NSUInteger severity = 1; severity <= 4; ++severity)
            [diagnostics addObject:@{ @"line": @0, @"character": @3, @"endLine": @1, @"endCharacter": @2,
                                     @"severity": @(severity), @"message": @"diagnostic 😀", @"code": severity == 1 ? (id)@123 : @"TEST",
                                     @"source": @"test-server", @"data": @{ @"private": @"not exported" } }];
        [(id<LSPClientDelegate>)manager lspClient:nil didReceiveDiagnostics:diagnostics forDocumentURI:uri];
        [diagnostics removeAllObjects]; // The manager owns an immutable copy.
        result = [window testEditorContextForMethod:@"get_diagnostics" arguments:@{ @"limit": @2 }];
        OAK_ASSERT([result[@"diagnostics"] count] == 2);
        OAK_ASSERT([result[@"totalCount"] integerValue] == 4 && [result[@"truncated"] boolValue]);
        OAK_ASSERT([result[@"nextOffset"] integerValue] == 2);
        NSDictionary* first = result[@"diagnostics"][0];
        OAK_ASSERT([first[@"severity"] isEqual:@"error"] && [first[@"code"] integerValue] == 123);
        OAK_ASSERT([first[@"source"] isEqual:@"test-server"] && first[@"data"] == nil);
        OAK_ASSERT([first[@"range"][@"start"][@"line"] integerValue] == 1 && [first[@"range"][@"start"][@"column"] integerValue] == 4);
        OAK_ASSERT([first[@"location"] isEqual:[path stringByAppendingString:@":1"]]);
        OAK_ASSERT([result[@"freshness"] isEqual:@"unknown"]);
        capturedReport = result[@"reportId"];
        OAK_ASSERT(![capturedReport isEqual:emptyReport]);
        result = [window testEditorContextForMethod:@"get_diagnostics" arguments:@{ @"documentId": document.identifier.UUIDString, @"offset": @2, @"reportId": capturedReport }];
        OAK_ASSERT(![result[@"truncated"] boolValue] && [result[@"diagnostics"] count] == 2);
        OAK_ASSERT([result[@"diagnostics"][0][@"severity"] isEqual:@"information"]);
        OAK_ASSERT([result[@"diagnostics"][1][@"severity"] isEqual:@"hint"]);
        OAK_ASSERT([[window testEditorContextForMethod:@"get_diagnostics" arguments:@{ @"documentId": foreign.identifier.UUIDString }][@"code"] isEqual:@"document_unavailable"]);
        OAK_ASSERT([[window testEditorContextForMethod:@"get_diagnostics" arguments:@{ @"offset": @5 }][@"code"] isEqual:@"invalid_offset"]);
        snapshot = [window editorContextForMethod:@"get_diagnostics" arguments:@{ @"limit": @100 }];
        document.content = @"unsaved changes after capture";
        [(id<LSPClientDelegate>)manager lspClient:nil didReceiveDiagnostics:@[] forDocumentURI:uri];
        OAK_ASSERT([[window testEditorContextForMethod:@"get_diagnostics" arguments:@{ @"reportId": capturedReport }][@"code"] isEqual:@"diagnostics_changed"]);
        [clients removeObjectForKey:document.identifier];
    });
    NSDictionary* captured = [snapshot renderBefore:editor_context::now()+3];
    OAK_ASSERT([captured[@"diagnostics"] count] == 4 && [captured[@"reportId"] isEqual:capturedReport]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setSelectedDocument:nil];
        [window setDocuments:@[]];
        [document close];
        [foreign close];
        [window close];
        // These fixture reports have no real server lifecycle to clear them.
        NSString* uri = [NSURL fileURLWithPath:path].absoluteString;
        [[LSPManager.sharedManager valueForKey:@"diagnosticsByURI"] removeObjectForKey:uri];
        [[LSPManager.sharedManager valueForKey:@"diagnosticReportIdentifiers"] removeObjectForKey:uri];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:directory error:nil]);
}

void test_editor_context_diagnostics_bounds_and_invalid_ranges ()
{
    NSString* message = [[@"x" stringByPaddingToLength:4095 withString:@"x" startingAtIndex:0] stringByAppendingString:@"😀suffix"];
    NSDictionary* item = @{ @"line": @0, @"character": @1, @"endLine": @0, @"endCharacter": @2,
                            @"message": message, @"severity": @2 };
    NSMutableArray* many = [NSMutableArray array];
    for(int i = 0; i < 100; ++i) [many addObject:item];
    NSDictionary* report = @{ @"diagnostics": many, @"reportId": @"bounded", @"status": @"reported", @"serverStatus": @"running", @"pendingChanges": @NO, @"freshness": @"unknown" };
    NSDictionary* result = EditorContextDiagnostics(report, @{}, @{}, @{ @"limit": @100 }, editor_context::now()+3);
    OAK_ASSERT([result[@"diagnostics"] count] > 0 && [result[@"diagnostics"] count] < 100);
    OAK_ASSERT([result[@"textTruncated"] boolValue] && [result[@"truncated"] boolValue]);
    OAK_ASSERT([result[@"nextOffset"] integerValue] == [result[@"diagnostics"] count]);
    OAK_ASSERT([result[@"diagnostics"][0][@"message"] length] == 4095); // No half of a surrogate pair.
    OAK_ASSERT([NSJSONSerialization dataWithJSONObject:result options:0 error:nil].length < editor_context::message_limit);
    NSMutableDictionary* invalid = [item mutableCopy];
    invalid[@"line"] = @(-1);
    NSMutableDictionary* invalidReport = [report mutableCopy];
    invalidReport[@"diagnostics"] = @[invalid];
    result = EditorContextDiagnostics(invalidReport, @{}, @{}, @{}, editor_context::now()+3);
    OAK_ASSERT(result[@"diagnostics"][0][@"range"] == NSNull.null);
    OAK_ASSERT([result[@"diagnostics"][0][@"invalidRange"] boolValue]);
    OAK_ASSERT([EditorContextDiagnostics(report, @{}, @{}, @{}, editor_context::now()-1)[@"code"] isEqual:@"timeout"]);
}

void test_editor_context_navigation_files_ranges_and_focus ()
{
    NSString* directory = [@"/tmp/tm-navigation-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:directory withIntermediateDirectories:YES attributes:nil error:nil]);
    NSString* file = [directory stringByAppendingPathComponent:@"a 'quoted' file 😀.txt"];
    NSString* content = @"first\nA😀B\nlast\n";
    OAK_ASSERT([content writeToFile:file atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block DocumentWindowController* window;
    __block DocumentWindowController* other;
    __block OakDocument* document;
    __block OakDocument* foreign;
    __block NSTextView* terminalFocus;
    __block NSDictionary* environment;
    NSUUID* session = NSUUID.UUID;
    dispatch_group_t loaded = dispatch_group_create();
    __block BOOL success = YES;
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        other = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithString:@"A😀B\n\té z\nlast\n" fileType:@"text.plain" customName:@"unsaved navigation"];
        foreign = [OakDocument documentWithString:@"other window" fileType:@"text.plain" customName:@"foreign navigation"];
        for(OakDocument* doc in @[document, foreign]) {
            dispatch_group_enter(loaded);
            [doc loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                success = success && status == OakDocumentIOResultSuccess;
                dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0 && success);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        [other setDocuments:@[foreign]]; [other setSelectedDocument:foreign];
        // navigate_editor only opens paths inside the window's project, so the
        // file this test opens must live in that root.
        [window setValue:directory forKey:@"projectPath"];
        terminalFocus = [[NSTextView alloc] initWithFrame:NSMakeRect(0,0,10,10)];
        [window.window.contentView addSubview:terminalFocus];
        [window.window makeFirstResponder:terminalFocus];
        __weak DocumentWindowController* owner = window;
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [owner editorContextForMethod:method arguments:args]; }];
    });
    NSDictionary* result = context_test_query(environment, @"navigate_editor", @{ @"documentId": document.identifier.UUIDString, @"line": @1, @"column": @2, @"endLine": @1, @"endColumn": @4 });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && ![result[@"opened"] boolValue]);
    OAK_ASSERT([result[@"range"][@"start"][@"offset"] integerValue] == 1 && [result[@"range"][@"end"][@"offset"] integerValue] == 3);
    OAK_ASSERT([context_test_query(environment)[@"selections"][0][@"text"] isEqual:@"😀"]);
    dispatch_sync(dispatch_get_main_queue(), ^{ OAK_ASSERT(window.window.firstResponder == terminalFocus); });
    result = context_test_query(environment, @"navigate_editor", @{ @"path": file, @"line": @2, @"column": @4, @"endLine": @2, @"endColumn": @5 });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && [result[@"opened"] boolValue]);
    NSString* openedId = result[@"document"][@"documentId"];
    NSDictionary* context = context_test_query(environment);
    OAK_ASSERT([context[@"document"][@"documentId"] isEqual:openedId]);
    OAK_ASSERT([context[@"selections"][0][@"text"] isEqual:@"B"]);
    result = context_test_query(environment, @"navigate_editor", @{ @"path": file });
    OAK_ASSERT(![result[@"opened"] boolValue] && result[@"range"] == NSNull.null);
    OAK_ASSERT([context_test_query(environment)[@"selections"][0][@"text"] isEqual:@"B"]);
    for(NSDictionary* args in @[
        @{ @"documentId": document.identifier.UUIDString, @"line": @99 },
        @{ @"documentId": document.identifier.UUIDString, @"line": @1, @"column": @3 },
        @{ @"documentId": document.identifier.UUIDString, @"revision": @"outdated", @"line": @1 },
        @{ @"documentId": foreign.identifier.UUIDString },
        @{ @"path": [directory stringByAppendingPathComponent:@"missing.txt"] }, @{ @"path": directory }
    ]) {
        OAK_ASSERT(context_test_query(environment, @"navigate_editor", args)[@"error"] != nil);
        OAK_ASSERT([context_test_query(environment)[@"document"][@"documentId"] isEqual:openedId]);
    }
    result = context_test_query(environment, @"navigate_editor", @{ @"documentId": document.identifier.UUIDString, @"line": @2, @"column": @2, @"endLine": @2, @"endColumn": @3, @"focus": @YES });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && [result[@"focused"] boolValue]);
    OAK_ASSERT([context_test_query(environment)[@"selections"][0][@"text"] isEqual:@"é"]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT(window.window.firstResponder == window.textView);
        OAK_ASSERT(window.documents.count == 2 && other.selectedDocument == foreign);
        OAK_ASSERT([document.content isEqual:@"A😀B\n\té z\nlast\n"]);
        [EditorContextService.sharedService revokeSession:session];
        [window close]; [other close];
        [document close]; [foreign close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([[NSString stringWithContentsOfFile:file encoding:NSUTF8StringEncoding error:nil] isEqual:content]);
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:directory error:nil]);
}

void test_editor_context_navigation_late_actions_and_session_transfer ()
{
    __block NSInteger effects = 0;
    dispatch_semaphore_t started = dispatch_semaphore_create(0);
    dispatch_semaphore_t completed = dispatch_semaphore_create(0);
    NSUUID* session = NSUUID.UUID;
    __block NSDictionary* environment;
    dispatch_sync(dispatch_get_main_queue(), ^{
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString*, NSDictionary*){
            return [EditorContextSnapshot action:^(double deadline, BOOL (^canCommit)(void), void (^completion)(NSDictionary*)){
                dispatch_semaphore_signal(started);
                dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 200*NSEC_PER_MSEC), dispatch_get_main_queue(), ^{
                    if(canCommit()) ++effects;
                    completion(canCommit() ? @{ @"status": @"navigated" } : editor_context::failure(@"Session changed", @"navigation_cancelled"));
                    dispatch_semaphore_signal(completed);
                });
            }];
        }];
    });
    __block NSDictionary* result;
    dispatch_group_t query = dispatch_group_create();
    dispatch_group_async(query, dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0), ^{ result = context_test_query(environment, @"navigate_editor", @{ @"path": @"/tmp/unused" }); });
    long startStatus = dispatch_semaphore_wait(started, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC));
    if(startStatus) NSLog(@"Navigation test did not start: %@", result);
    OAK_ASSERT(startStatus == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        // Transfer keeps credentials but changes the registered owning provider.
        [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString*, NSDictionary*){ return [EditorContextSnapshot response:@{ @"window": @"destination" }]; }];
    });
    OAK_ASSERT(dispatch_group_wait(query, dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_SEC)) == 0);
    OAK_ASSERT([result[@"code"] isEqual:@"navigation_cancelled"] && effects == 0);
    OAK_ASSERT(dispatch_semaphore_wait(completed, dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_SEC)) == 0);
    // renderBefore hops to the main queue before it invokes the action, and the
    // queued block re-checks the deadline, so a budget shorter than main-queue
    // latency in parallel runs drops the action uninvoked and it can never
    // signal completion. The deferral must still outlast the render deadline.
    EditorContextSnapshot* late = [EditorContextSnapshot action:^(double deadline, BOOL (^canCommit)(void), void (^completion)(NSDictionary*)){
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 2*NSEC_PER_SEC), dispatch_get_main_queue(), ^{
            if(canCommit()) ++effects;
            completion(@{});
            dispatch_semaphore_signal(completed);
        });
    }];
    OAK_ASSERT([[late renderBefore:editor_context::now()+1.0][@"code"] isEqual:@"timeout"]);
    OAK_ASSERT(dispatch_semaphore_wait(completed, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    OAK_ASSERT(effects == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{ [EditorContextService.sharedService revokeSession:session]; });
}

void test_editor_context_follow_changes_and_bounded_history ()
{
    __block DocumentWindowController* window;
    __block OakDocument* first;
    __block OakDocument* second;
    dispatch_group_t loaded = dispatch_group_create();
    __block BOOL success = YES;
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        first = [OakDocument documentWithString:@"A😀B\n" fileType:@"text.plain" customName:@"first follow"];
        second = [OakDocument documentWithString:@"second" fileType:@"text.plain" customName:@"second follow"];
        for(OakDocument* document in @[first, second]) {
            dispatch_group_enter(loaded);
            [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                success = success && status == OakDocumentIOResultSuccess;
                dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0 && success);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[first, second]]; [window setSelectedDocument:first];
        NSDictionary* initial = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{}];
        OAK_ASSERT([initial[@"reset"] boolValue] && [initial[@"changed"] boolValue]);
        OAK_ASSERT([initial[@"context"][@"document"][@"documentId"] isEqual:first.identifier.UUIDString]);
        NSString* cursor = initial[@"cursor"];
        OAK_ASSERT([cursor isEqual:initial[@"context"][@"changeCursor"]]);
        NSDictionary* unchanged = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT(![unchanged[@"changed"] boolValue] && unchanged[@"context"] == nil);
        OAK_ASSERT([[window testEditorContextForMethod:@"get_editor_change_token" arguments:@{}][@"cursor"] isEqual:cursor]);
        window.textView.selectionString = @"1:2-1:6";
        NSDictionary* selection = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT([selection[@"changed"] boolValue] && ![selection[@"reset"] boolValue]);
        OAK_ASSERT([[selection[@"events"] valueForKey:@"type"] containsObject:@"selection"]);
        OAK_ASSERT([selection[@"context"][@"selections"][0][@"text"] isEqual:@"😀"]);
        cursor = selection[@"cursor"];
        first.content = @"edited without saving";
        NSDictionary* content = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT([[content[@"events"] valueForKey:@"type"] containsObject:@"content"]);
        OAK_ASSERT(![content[@"context"][@"document"][@"revision"] isEqual:initial[@"context"][@"document"][@"revision"]]);
        cursor = content[@"cursor"];
        [window setSelectedDocument:second];
        NSDictionary* switched = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT([[switched[@"events"] valueForKey:@"type"] containsObject:@"document"]);
        OAK_ASSERT([switched[@"context"][@"document"][@"documentId"] isEqual:second.identifier.UUIDString]);
        cursor = switched[@"cursor"];
        first.content = @"inactive document edit";
        NSDictionary* inactive = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT([[inactive[@"events"] valueForKey:@"documentId"] containsObject:first.identifier.UUIDString]);
        OAK_ASSERT([inactive[@"context"][@"document"][@"documentId"] isEqual:second.identifier.UUIDString]);
        cursor = inactive[@"cursor"];
        [window setDocuments:@[second]];
        NSDictionary* tabs = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": cursor }];
        OAK_ASSERT([[tabs[@"events"] valueForKey:@"type"] containsObject:@"documents"]);
        NSDictionary* foreign = [window testEditorContextForMethod:@"get_editor_changes" arguments:@{ @"cursor": @"another-window/0" }];
        OAK_ASSERT([foreign[@"reset"] boolValue] && foreign[@"context"] != nil);
        EditorContextChanges* journal = [EditorContextChanges new];
        NSString* old = journal.cursor;
        for(int i = 0; i < 200; ++i) [journal record:@"selection" documentId:second.identifier.UUIDString];
        OAK_ASSERT([[journal changesAfter:old][@"reset"] boolValue]);
        NSString* recent = journal.cursor;
        [journal record:@"content" documentId:second.identifier.UUIDString];
        NSDictionary* delta = [journal changesAfter:recent];
        OAK_ASSERT(![delta[@"reset"] boolValue] && [delta[@"events"] count] == 1);
        OAK_ASSERT(![[journal changesAfter:journal.cursor][@"changed"] boolValue]);
        [window setSelectedDocument:nil]; [window setDocuments:@[]];
        [first close]; [second close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
}

void test_editor_context_large_immutable_snapshot ()
{
    __block OakDocument* document;
    __block EditorContextBuffer* snapshot;
    __block BOOL loaded = NO;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    NSUInteger repetitions = 200000;
    NSString* original = [@"abc😀\n" stringByPaddingToLength:repetitions*6 withString:@"abc😀\n" startingAtIndex:0];
    dispatch_sync(dispatch_get_main_queue(), ^{
        document = [OakDocument documentWithString:original fileType:@"text.plain" customName:@"large snapshot"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            loaded = status == OakDocumentIOResultSuccess;
            dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC)) == 0);
    OAK_ASSERT(loaded);
    dispatch_sync(dispatch_get_main_queue(), ^{
        snapshot = [EditorContextBuffer bufferForDocument:document revision:nil];
        document.content = @"edited after snapshot";
    });
    // Render on this worker thread after the live document changed.
    NSDictionary* read = [snapshot responseForMethod:@"read_document" arguments:@{ @"offset": @(repetitions*6-12), @"length": @12 }
        base:@{} document:@{} ranges:ng::ranges_t() caret:0 deadline:editor_context::now()+3];
    OAK_ASSERT([read[@"text"] isEqual:@"abc😀\nabc😀\n"]);
    OAK_ASSERT([read[@"totalLength"] unsignedIntegerValue] == repetitions*6);
    OAK_ASSERT(![read[@"truncated"] boolValue]);
    ng::ranges_t rectangle(ng::range_t(0, (repetitions-1)*8+1, true));
    NSDictionary* context = [snapshot responseForMethod:@"get_editor_context" arguments:@{} base:@{} document:@{}
        ranges:rectangle caret:repetitions*8 deadline:editor_context::now()+3];
    OAK_ASSERT([context[@"selections"] count] == 128);
    OAK_ASSERT([context[@"selectionsTruncated"] boolValue]);
    OAK_ASSERT([context[@"selections"][127][@"text"] isEqual:@"a"]);
    OAK_ASSERT([context[@"cursor"][@"offset"] unsignedIntegerValue] == repetitions*6);
    OAK_ASSERT([context[@"cursor"][@"line"] unsignedIntegerValue] == repetitions+1);
    NSDictionary* timeout = [snapshot responseForMethod:@"read_document" arguments:@{} base:@{} document:@{}
        ranges:ng::ranges_t() caret:0 deadline:editor_context::now()-1];
    OAK_MASSERT([timeout.description UTF8String] ?: "No timeout response was received", [timeout[@"code"] isEqual:@"timeout"]);
    dispatch_sync(dispatch_get_main_queue(), ^{ [document close]; });
}

void test_editor_context_stray_continuation_byte_offsets ()
{
    __block OakDocument* document;
    __block EditorContextBuffer* snapshot;
    __block BOOL loaded = NO;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        document = [OakDocument documentWithString:@"AB" fileType:@"text.plain" customName:@"stray continuation byte"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            loaded = status == OakDocumentIOResultSuccess;
            dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5 * NSEC_PER_SEC)) == 0);
    OAK_ASSERT(loaded);
    dispatch_sync(dispatch_get_main_queue(), ^{
        // The open path rejects undecodable files, so the raw buffer is edited directly.
        document.buffer.insert(1, "\x80\x80", 2);
        OAK_ASSERT(document.buffer.size() == 4);
        snapshot = [EditorContextBuffer bufferForDocument:document revision:nil];
    });
    auto read = ^NSDictionary*(NSUInteger offset, NSUInteger length){
        return [snapshot responseForMethod:@"read_document" arguments:@{ @"offset": @(offset), @"length": @(length) }
            base:@{} document:@{} ranges:ng::ranges_t() caret:0 deadline:editor_context::now()+3];
    };
    NSUInteger total = [read(0, 4)[@"totalLength"] unsignedIntegerValue];
    OAK_ASSERT(total == 2);
    // Every reported offset must map back to the byte it came from.
    for(NSUInteger offset = 0; offset <= total; ++offset)
        OAK_ASSERT_EQ([read(offset, 0)[@"nextOffset"] unsignedIntegerValue], offset);
    // Reading the buffer in order must reach the end instead of stalling.
    NSUInteger offset = 0;
    for(size_t i = 0; i < 8 && offset < total; ++i) {
        NSUInteger next = [read(offset, 1)[@"nextOffset"] unsignedIntegerValue];
        OAK_ASSERT(next > offset);
        offset = next;
    }
    OAK_ASSERT(offset == total);
    dispatch_sync(dispatch_get_main_queue(), ^{ [document close]; });
}

void test_editor_context_real_terminal_transfer_and_window_close ()
{
    __block DocumentWindowController* first;
    __block DocumentWindowController* second;
    __block OakDocument* firstDocument;
    __block OakDocument* secondDocument;
    __block NSDictionary* firstEnvironment;
    __block NSDictionary* secondEnvironment;
    __block TerminalPanelViewController* firstPanel;
    __block TerminalPanelViewController* secondPanel;
    __block NSUUID* movedSession;
    dispatch_group_t loaded = dispatch_group_create();
    __block BOOL success = YES;
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        first = [[DocumentWindowController alloc] init];
        second = [[DocumentWindowController alloc] init];
        firstDocument = [OakDocument documentWithString:@"first window unsaved" fileType:@"text.plain" customName:@"first document"];
        secondDocument = [OakDocument documentWithString:@"second window unsaved" fileType:@"text.plain" customName:@"second document"];
        for(OakDocument* document in @[firstDocument, secondDocument]) {
            dispatch_group_enter(loaded);
            [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                success = success && status == OakDocumentIOResultSuccess;
                dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    OAK_ASSERT(success);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [first setDocuments:@[firstDocument]]; [first setSelectedDocument:firstDocument];
        [second setDocuments:@[secondDocument]]; [second setSelectedDocument:secondDocument];
        firstPanel = [first valueForKey:@"terminalPanelController"];
        secondPanel = [second valueForKey:@"terminalPanelController"];
        firstPanel.terminalDirectoryProvider = ^{ return NSTemporaryDirectory(); };
        secondPanel.terminalDirectoryProvider = ^{ return NSTemporaryDirectory(); };
        [firstPanel startTerminal];
        [secondPanel startTerminal];
        movedSession = [[firstPanel valueForKey:@"terminalController"] valueForKey:@"sessionIdentifiers"][0];
        NSUUID* secondSession = [[secondPanel valueForKey:@"terminalController"] valueForKey:@"sessionIdentifiers"][0];
        firstEnvironment = firstPanel.contextEnvironmentProvider(movedSession);
        secondEnvironment = secondPanel.contextEnvironmentProvider(secondSession);
    });
    __block NSDictionary* firstResult;
    __block NSDictionary* secondResult;
    dispatch_group_t queries = dispatch_group_create();
    dispatch_group_async(queries, dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0), ^{ firstResult = context_test_query(firstEnvironment); });
    dispatch_group_async(queries, dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0), ^{ secondResult = context_test_query(secondEnvironment); });
    OAK_ASSERT(dispatch_group_wait(queries, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    OAK_ASSERT([firstResult[@"document"][@"documentId"] isEqual:firstDocument.identifier.UUIDString]);
    OAK_ASSERT([secondResult[@"document"][@"documentId"] isEqual:secondDocument.identifier.UUIDString]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [secondPanel takeTerminalSession:movedSession fromPanel:firstPanel];
        [first close];
    });
    OAK_ASSERT([context_test_query(firstEnvironment)[@"document"][@"documentId"] isEqual:secondDocument.identifier.UUIDString]);
    NSDictionary* transferredChanges = context_test_query(firstEnvironment, @"get_editor_changes", @{ @"cursor": firstResult[@"changeCursor"] });
    OAK_ASSERT([transferredChanges[@"reset"] boolValue]);
    OAK_ASSERT([transferredChanges[@"context"][@"document"][@"documentId"] isEqual:secondDocument.identifier.UUIDString]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [second close];
        [firstDocument close]; [secondDocument close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([context_test_query(firstEnvironment)[@"code"] isEqual:@"session_unavailable"]);
    OAK_ASSERT([context_test_query(secondEnvironment)[@"code"] isEqual:@"session_unavailable"]);
}

void test_project_changes_unsaved_snapshot_and_navigation ()
{
    NSString* directory = [@"/tmp/textmate-changes-buffer-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:directory withIntermediateDirectories:YES attributes:nil error:nil]);
    NSString* path = [directory stringByAppendingPathComponent:@"buffer.txt"];
    OAK_ASSERT([@"saved text\n" writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    NSTask* git = [[NSTask alloc] init];
    git.executableURL = [NSURL fileURLWithPath:@"/usr/bin/git"];
    git.arguments = @[@"-C", directory, @"init", @"--quiet"];
    OAK_ASSERT([git launchAndReturnError:nil]); [git waitUntilExit];
    OAK_ASSERT(git.terminationStatus == 0);
    __block OakDocument* document;
    __block ProjectChangesBuffer* buffer;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        document = [OakDocument documentWithPath:path];
        document.fileType = @"text.plain";
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        document.content = @"unsaved 😀\n";
        buffer = [ProjectChangesBuffer captureDocument:document];
        document.content = @"later typing\n";
    });
    NSDictionary* buffers = @{ path: buffer };
    NSDictionary* snapshot = ProjectChangesSnapshot(directory, buffers);
    OAK_ASSERT(!snapshot[@"error"] && [snapshot[@"files"] count] == 1);
    NSDictionary* file = snapshot[@"files"][0];
    OAK_ASSERT([file[@"unsaved"] boolValue]);
    NSDictionary* diff = ProjectChangesDiff(snapshot, file, buffers);
    OAK_ASSERT([diff[@"text"] containsString:@"+unsaved 😀"]);
    OAK_ASSERT(![diff[@"text"] containsString:@"later typing"]);
    OAK_ASSERT([[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil] isEqual:@"saved text\n"]);
    __block ProjectChangesWindowController* review;
    __block BOOL opened = NO;
    dispatch_sync(dispatch_get_main_queue(), ^{
        review = [[ProjectChangesWindowController alloc] initWithDirectory:directory buffers:^NSDictionary*{ return buffers; } openFile:^(NSString* openedPath, NSUInteger line){
            opened = [openedPath.lastPathComponent isEqual:@"buffer.txt"] && line == 1;
        }];
        [review showWindow:nil];
    });
    __block BOOL loaded = NO;
    for(int i = 0; i < 300; ++i) {
        dispatch_sync(dispatch_get_main_queue(), ^{
            loaded = [[review valueForKey:@"files"] count] == 1 && ![[review valueForKey:@"loadingDiff"] boolValue] && [review valueForKey:@"displayedDiff"] != nil;
        });
        if(loaded) break;
        usleep(10000);
    }
    OAK_ASSERT(loaded);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([[review valueForKey:@"diffView"] string] != nil);
        OAK_ASSERT([[[review valueForKey:@"diffView"] string] containsString:@"+unsaved 😀"]);
        NSButton* open = [review valueForKey:@"openButton"];
        OAK_ASSERT(open.enabled);
        [open performClick:nil];
        OAK_ASSERT(opened);
        [review close]; [document close];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:directory error:nil]);
}


void test_editor_language_normalization_bounds_and_server_data ()
{
    NSDictionary* range = @{ @"start": @{ @"line": @0, @"character": @3 }, @"end": @{ @"line": @0, @"character": @4 } };
    NSDictionary* metadata = @{ @"path": @"/tmp/language 😀.txt" };
    NSDictionary* link = @{ @"targetUri": @"file:///tmp/other.txt", @"targetSelectionRange": range };
    NSDictionary* result = EditorLanguageNormalize(@"get_definition", @[link, @{}, @7], metadata, editor_context::now()+3);
    OAK_ASSERT([result[@"items"] count] == 1 && [result[@"invalidCount"] integerValue] == 2);
    OAK_ASSERT([result[@"items"][0][@"range"][@"start"][@"column"] integerValue] == 4);
    OAK_ASSERT([result[@"items"][0][@"location"] isEqual:@"/tmp/other.txt:1"]);
    OAK_ASSERT(([EditorLanguageNormalize(@"get_definition", NSNull.null, metadata, editor_context::now()+3)[@"items"] count] == 0));
    OAK_ASSERT((EditorLanguageNormalize(@"find_references", @{}, metadata, editor_context::now()+3)[@"error"] != nil));
    NSDictionary* unresolved = @{ @"name": @"Unresolved", @"kind": @12, @"location": @{ @"uri": @"file:///tmp/other.txt" } };
    result = EditorLanguageNormalize(@"search_workspace_symbols", @[unresolved], metadata, editor_context::now()+3);
    OAK_ASSERT([result[@"items"][0][@"unresolved"] boolValue] && result[@"items"][0][@"range"] == NSNull.null);
    NSMutableArray* many = [NSMutableArray array];
    for(int i = 0; i < 1100; ++i) [many addObject:link];
    result = EditorLanguageNormalize(@"get_definition", many, metadata, editor_context::now()+3);
    OAK_ASSERT([result[@"items"] count] <= 1000 && [result[@"resultsTruncated"] boolValue]);
    NSString* large = [[@"a" stringByPaddingToLength:8191 withString:@"a" startingAtIndex:0] stringByAppendingString:@"😀"];
    result = EditorLanguageNormalize(@"get_hover", @{ @"contents": large }, metadata, editor_context::now()+3);
    OAK_ASSERT([result[@"hover"][@"textTruncated"] boolValue]);
    OAK_ASSERT([result[@"hover"][@"text"] length] == 8191);
    OAK_ASSERT((EditorLanguageNormalize(@"get_definition", @[], metadata, editor_context::now()-1)[@"error"] != nil));
}

void test_editor_language_queries_real_server_and_session_boundary ()
{
    NSString* path = [@"/tmp/textmate-language-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([@"A😀B\n" writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block OakDocument* document;
    __block DocumentWindowController* window;
    __block LSPClient* client;
    __block NSDictionary* environment;
    NSUUID* session = NSUUID.UUID;
    dispatch_semaphore_t loaded = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithPath:path]; document.fileType = @"text.plain";
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(loaded);
        }];
        NSString* fixture = [@(__FILE__).stringByDeletingLastPathComponent stringByAppendingPathComponent:@"language_server.py"];
        client = [[LSPClient alloc] initWithCommand:@"/usr/bin/python3" arguments:@[fixture] workingDirectory:@"/tmp" initOptions:nil];
    });
    OAK_ASSERT(dispatch_semaphore_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    __block BOOL initialized = NO;
    for(int i = 0; i < 500; ++i) {
        dispatch_sync(dispatch_get_main_queue(), ^{ initialized = client.initialized; });
        if(initialized) break;
        usleep(10000);
    }
    OAK_ASSERT(initialized);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        [client openDocument:document languageId:@"plaintext"];
        LSPManager* manager = LSPManager.sharedManager;
        [manager valueForKey:@"documentClients"][document.identifier] = client;
        [[manager valueForKey:@"openDocuments"] addObject:document.identifier];
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [window editorContextForMethod:method arguments:args]; }];
        document.content = @"A😀B unsaved\n";
        window.textView.selectionString = @"1:6";
    });
    NSDictionary* definition = context_test_query(environment, @"get_definition");
    OAK_ASSERT(!definition[@"error"] && [definition[@"items"] count] == 1);
    OAK_ASSERT([definition[@"items"][0][@"range"][@"start"][@"column"] integerValue] == 4);
    OAK_ASSERT([context_test_query(environment, @"get_definition", @{ @"line": @1, @"column": @3 })[@"code"] isEqual:@"invalid_position"]);
    OAK_ASSERT([context_test_query(environment, @"get_definition", @{ @"documentId": NSUUID.UUID.UUIDString })[@"code"] isEqual:@"document_unavailable"]);
    NSDictionary* hover = context_test_query(environment, @"get_hover");
    NSDictionary* echoed = editor_context::parse([hover[@"hover"][@"text"] dataUsingEncoding:NSUTF8StringEncoding]);
    OAK_ASSERT([echoed[@"text"] isEqual:@"A😀B unsaved\n"]);
    OAK_ASSERT([echoed[@"position"][@"character"] integerValue] == 3); // byte 5 → UTF-16 3
    NSDictionary* references = context_test_query(environment, @"find_references", @{ @"limit": @100 });
    OAK_ASSERT(!references[@"error"] && [references[@"items"] count] == 100 && [references[@"availableCount"] integerValue] == 205);
    NSDictionary* page = context_test_query(environment, @"find_references", @{ @"resultId": references[@"resultId"], @"offset": @100, @"limit": @100 });
    OAK_ASSERT([page[@"items"] count] == 100 && [page[@"nextOffset"] integerValue] == 200);
    page = context_test_query(environment, @"find_references", @{ @"resultId": references[@"resultId"], @"offset": @200 });
    OAK_ASSERT([page[@"items"] count] == 5 && page[@"nextOffset"] == NSNull.null);
    NSDictionary* symbols = context_test_query(environment, @"get_document_symbols");
    OAK_ASSERT([symbols[@"items"] count] == 2 && [symbols[@"items"][1][@"container"] isEqual:@"Outer"]);
    NSDictionary* workspace = context_test_query(environment, @"search_workspace_symbols", @{ @"query": @"Example" });
    OAK_ASSERT([workspace[@"items"][0][@"location"] isEqual:@"/tmp/workspace-symbol.txt:5"]);
    OAK_ASSERT([context_test_query(environment, @"search_workspace_symbols", @{ @"query": @"error" })[@"code"] isEqual:@"language_request_failed"]);
    NSDictionary* timeout = context_test_query(environment, @"search_workspace_symbols", @{ @"query": @"timeout" });
    OAK_MASSERT([timeout.description UTF8String] ?: "No timeout response was received", [timeout[@"code"] isEqual:@"timeout"]);
    dispatch_async(dispatch_get_main_queue(), ^{
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 100*NSEC_PER_MSEC), dispatch_get_main_queue(), ^{ document.content = @"changed while querying"; });
    });
    OAK_ASSERT([context_test_query(environment, @"search_workspace_symbols", @{ @"query": @"slow" })[@"code"] isEqual:@"document_changed"]);
    OAK_ASSERT([context_test_query(environment, @"find_references", @{ @"resultId": references[@"resultId"], @"offset": @100 })[@"code"] isEqual:@"result_expired"]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 100*NSEC_PER_MSEC), dispatch_get_main_queue(), ^{ [EditorContextService.sharedService revokeSession:session]; });
    });
    OAK_ASSERT([context_test_query(environment, @"search_workspace_symbols", @{ @"query": @"slow" })[@"error"] isKindOfClass:NSString.class]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [LSPManager.sharedManager documentWillClose:document];
        [client shutdown];
        [window setSelectedDocument:nil]; [window setDocuments:@[]]; [document close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:path error:nil]);
}


void test_editor_language_queries_refused_for_non_utf16_server ()
{
    NSString* path = [@"/tmp/textmate-language-encoding-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([@"A😀B\n" writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block OakDocument* document;
    __block DocumentWindowController* window;
    __block LSPClient* client;
    __block NSDictionary* environment;
    NSUUID* session = NSUUID.UUID;
    dispatch_semaphore_t loaded = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithPath:path]; document.fileType = @"text.plain";
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(loaded);
        }];
        NSString* fixture = [@(__FILE__).stringByDeletingLastPathComponent stringByAppendingPathComponent:@"language_server.py"];
        client = [[LSPClient alloc] initWithCommand:@"/usr/bin/python3" arguments:@[fixture, @"--position-encoding=utf-8"] workingDirectory:@"/tmp" initOptions:nil];
    });
    OAK_ASSERT(dispatch_semaphore_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    for(int i = 0; i < 500; ++i) {
        __block BOOL initialized = NO;
        dispatch_sync(dispatch_get_main_queue(), ^{ initialized = client.initialized; });
        if(initialized) break;
        usleep(10000);
    }
    dispatch_sync(dispatch_get_main_queue(), ^{
        // The handshake succeeded, so a refusal below comes from the position gate and not from a dead server.
        OAK_ASSERT(client.initialized);
        // LSPClient keeps a query only when the server leaves positionEncoding out or advertises
        // "utf-16", so a server advertising "utf-8" loses every language-query method.
        OAK_ASSERT(![client supportsLanguageQuery:@"textDocument/definition"]);
        OAK_ASSERT(![client supportsLanguageQuery:@"textDocument/references"]);
        OAK_ASSERT(![client supportsLanguageQuery:@"textDocument/hover"]);
        OAK_ASSERT(![client supportsLanguageQuery:@"textDocument/documentSymbol"]);
        OAK_ASSERT(![client supportsLanguageQuery:@"workspace/symbol"]);
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        [client openDocument:document languageId:@"plaintext"];
        LSPManager* manager = LSPManager.sharedManager;
        [manager valueForKey:@"documentClients"][document.identifier] = client;
        [[manager valueForKey:@"openDocuments"] addObject:document.identifier];
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [window editorContextForMethod:method arguments:args]; }];
        window.textView.selectionString = @"1:1";
    });
    for(NSString* method in @[@"get_definition", @"get_hover", @"find_references"]) {
        NSDictionary* refusal = context_test_query(environment, method);
        // LSPManager calls back with "unsupported_capability" before sending the request, and
        // EditorLanguageIntelligence turns that into a failure response with no result payload.
        OAK_MASSERT([refusal.description UTF8String] ?: "No refusal was returned", [refusal[@"code"] isEqual:@"unsupported_capability"]);
        OAK_ASSERT([refusal[@"error"] containsString:@"using UTF-16 positions"]);
        OAK_ASSERT([refusal[@"items"] count] == 0 && refusal[@"hover"] == nil);
    }
    dispatch_sync(dispatch_get_main_queue(), ^{
        [LSPManager.sharedManager documentWillClose:document];
        [client shutdown];
        [window setSelectedDocument:nil]; [window setDocuments:@[]]; [document close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:path error:nil]);
}

void test_document_edit_plan_unicode_insert_delete_and_undo ()
{
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block NSDictionary* metadata;
    __block EditorContextBuffer* buffer;
    dispatch_semaphore_t loaded = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication]; [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithString:@"A😀B\nkeep\n" fileType:@"text.plain" customName:@"live edit buffer"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(loaded);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        metadata = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        buffer = [EditorContextBuffer bufferForDocument:document revision:metadata[@"revision"]];
    });
    NSDictionary* (^arguments)(NSArray*) = ^NSDictionary*(NSArray* edits){ return @{ @"documentId": metadata[@"documentId"], @"revision": metadata[@"revision"], @"summary": @"Edit the live buffer", @"edits": edits }; };
    NSDictionary* plan = [buffer editPlanForArguments:arguments(@[
        @{ @"offset": @5, @"length": @4, @"text": @"stay" },
        @{ @"offset": @1, @"length": @2, @"text": @"🛠️" },
        @{ @"offset": @10, @"length": @0, @"text": @"tail\n" },
        @{ @"offset": @3, @"length": @1, @"text": @"" }]) document:metadata deadline:editor_context::now()+3];
    OAK_ASSERT(plan[@"error"] == nil);
    NSString* error = nil;
    BufferEditProposal* proposal = [[BufferEditProposal alloc] initWithEditPlan:plan summary:@"Update the buffer" error:&error];
    OAK_ASSERT(proposal != nil && error == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\n"]);
        OAK_ASSERT([proposal applyToDocument:document]);
        OAK_ASSERT([document.content isEqual:@"A🛠️\nstay\ntail\n"]);
        OAK_ASSERT(![proposal applyToDocument:document]);
        [document undo];
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\n"]);
    });
    NSArray* invalidRanges = @[
        @[@{ @"offset": @2, @"length": @0, @"text": @"x" }],
        @[@{ @"offset": @1, @"length": @1, @"text": @"x" }],
        @[@{ @"offset": @11, @"length": @0, @"text": @"x" }],
        @[@{ @"offset": @10, @"length": @1, @"text": @"x" }],
        @[@{ @"offset": @0, @"length": @3, @"text": @"x" }, @{ @"offset": @1, @"length": @2, @"text": @"y" }],
        @[@{ @"offset": @0, @"length": @0, @"text": @"x" }, @{ @"offset": @0, @"length": @0, @"text": @"y" }]
    ];
    for(NSArray* edits in invalidRanges) {
        NSDictionary* invalid = [buffer editPlanForArguments:arguments(edits) document:metadata deadline:editor_context::now()+3];
        OAK_ASSERT(invalid[@"error"] != nil);
    }
    plan = [buffer editPlanForArguments:arguments(@[@{ @"offset": @0, @"length": @0, @"text": @"" }]) document:metadata deadline:editor_context::now()+3];
    OAK_ASSERT([[BufferEditProposal alloc] initWithEditPlan:plan summary:@"No change" error:&error] == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        document.content = @"";
        metadata = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        buffer = [EditorContextBuffer bufferForDocument:document revision:metadata[@"revision"]];
    });
    plan = [buffer editPlanForArguments:arguments(@[@{ @"offset": @0, @"length": @0, @"text": @"new 😀" }]) document:metadata deadline:editor_context::now()+3];
    proposal = [[BufferEditProposal alloc] initWithEditPlan:plan summary:@"Start an empty document" error:&error];
    OAK_ASSERT(proposal != nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([proposal applyToDocument:document]);
        OAK_ASSERT([document.content isEqual:@"new 😀"]);
        [document undo]; OAK_ASSERT([document.content isEqual:@""]);
    });
    // A later edit invalidates even an insertion into the empty buffer.
    proposal = [[BufferEditProposal alloc] initWithEditPlan:plan summary:@"Stale change" error:&error];
    dispatch_sync(dispatch_get_main_queue(), ^{
        document.content = @"user typing";
        OAK_ASSERT(![proposal applyToDocument:document] && [proposal.status isEqual:@"stale"]);
        [window setSelectedDocument:nil]; [window setDocuments:@[]]; [document close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
}

static void document_edit_test_end_review(DocumentWindowController* window, NSModalResponse response)
{
    dispatch_sync(dispatch_get_main_queue(), ^{
        EditReviewWindowController* review = [window valueForKey:@"editReviewController"];
        OAK_ASSERT(review.window.isVisible && window.window.attachedSheet == nil);
        if(response == NSAlertFirstButtonReturn) [review apply:nil];
        else [review.window performClose:nil]; // Closing the window discards, too.
    });
    // Wait for proposal review completion before inspecting its effects.
    for(int i = 0; i < 100; ++i) {
        __block BOOL closed;
        dispatch_sync(dispatch_get_main_queue(), ^{
            closed = [window valueForKey:@"editReviewController"] == nil;
            for(BufferEditProposal* proposal in [[window valueForKey:@"editProposals"] allValues])
                if([proposal.status isEqual:@"pending_review"]) closed = NO;
        });
        if(closed) break;
        usleep(10000);
    }
}

void test_document_edit_proposals_live_review_window_scope_and_disk ()
{
    NSString* path = [@"/tmp/textmate-buffer-proposal-" stringByAppendingString:NSUUID.UUID.UUIDString];
    OAK_ASSERT([@"one\n" writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block OakDocument* untitled;
    __block NSDictionary* environment;
    NSUUID* session = NSUUID.UUID;
    dispatch_group_t loaded = dispatch_group_create();
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication]; [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithPath:path]; document.fileType = @"text.plain";
        untitled = [OakDocument documentWithString:@"other" fileType:@"text.plain" customName:@"untitled target"];
        for(OakDocument* target in @[document, untitled]) {
            dispatch_group_enter(loaded);
            [target loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document, untitled]]; [window setSelectedDocument:untitled]; [window showWindow:nil];
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [window editorContextForMethod:method arguments:args]; }];
    });
    NSDictionary* source = context_test_query(environment, @"read_document", @{ @"documentId": document.identifier.UUIDString });
    NSDictionary* args = @{ @"documentId": document.identifier.UUIDString, @"revision": source[@"document"][@"revision"], @"summary": @"Update the inactive buffer",
        @"edits": @[@{ @"offset": @0, @"length": @3, @"text": @"two" }, @{ @"offset": @4, @"length": @0, @"text": @"tail\n" }] };
    NSMutableDictionary* invalid = [args mutableCopy]; invalid[@"documentId"] = NSUUID.UUID.UUIDString;
    OAK_ASSERT([context_test_query(environment, @"propose_document_edits", invalid)[@"code"] isEqual:@"document_unavailable"]);
    invalid = [args mutableCopy]; invalid[@"revision"] = @"old revision";
    OAK_ASSERT([context_test_query(environment, @"propose_document_edits", invalid)[@"code"] isEqual:@"document_changed"]);
    NSDictionary* proposal = context_test_query(environment, @"propose_document_edits", args);
    OAK_ASSERT([proposal[@"status"] isEqual:@"pending_review"] && ![proposal[@"applied"] boolValue]);
    NSDictionary* statusArgs = @{ @"proposalId": proposal[@"proposalId"] };
    OAK_ASSERT([context_test_query(environment, @"get_edit_proposal_status", statusArgs)[@"status"] isEqual:@"pending_review"]);
    OAK_ASSERT([context_test_query(environment, @"propose_document_edits", args)[@"code"] isEqual:@"review_unavailable"]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([document.content isEqual:@"one\n"] && [untitled.content isEqual:@"other"]);
        OAK_ASSERT(window.selectedDocument == untitled);
        DocumentWindowController* foreign = [[DocumentWindowController alloc] init];
        OAK_ASSERT([foreign testEditorContextForMethod:@"get_edit_proposal_status" arguments:statusArgs][@"error"] != nil);
        [foreign close];
    });
    document_edit_test_end_review(window, NSAlertFirstButtonReturn);
    NSDictionary* status = context_test_query(environment, @"get_edit_proposal_status", statusArgs);
    OAK_ASSERT([status[@"status"] isEqual:@"applied"]);
    OAK_ASSERT(![status[@"document"][@"revision"] isEqual:source[@"document"][@"revision"]]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([document.content isEqual:@"two\ntail\n"]);
        OAK_ASSERT(document.isDocumentEdited && window.selectedDocument == untitled);
        OAK_ASSERT([[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil] isEqual:@"one\n"]);
        [document undo]; OAK_ASSERT([document.content isEqual:@"one\n"]);
    });
    OAK_ASSERT([context_test_query(environment, @"propose_document_edits", args)[@"code"] isEqual:@"document_changed"]);
    source = context_test_query(environment, @"read_document", @{ @"documentId": untitled.identifier.UUIDString });
    args = @{ @"documentId": untitled.identifier.UUIDString, @"revision": source[@"document"][@"revision"], @"summary": @"Change untitled text",
        @"edits": @[@{ @"offset": @0, @"length": @5, @"text": @"updated" }] };
    proposal = context_test_query(environment, @"propose_document_edits", args);
    OAK_ASSERT([proposal[@"status"] isEqual:@"pending_review"]);
    statusArgs = @{ @"proposalId": proposal[@"proposalId"] };
    document_edit_test_end_review(window, NSAlertSecondButtonReturn);
    OAK_ASSERT([context_test_query(environment, @"get_edit_proposal_status", statusArgs)[@"status"] isEqual:@"discarded"]);
    dispatch_sync(dispatch_get_main_queue(), ^{ OAK_ASSERT([untitled.content isEqual:@"other"]); });
    // Editing while the sheet is open must prevent application.
    proposal = context_test_query(environment, @"propose_document_edits", args);
    OAK_ASSERT([proposal[@"status"] isEqual:@"pending_review"]);
    statusArgs = @{ @"proposalId": proposal[@"proposalId"] };
    dispatch_sync(dispatch_get_main_queue(), ^{
        untitled.content = @"new user typing";
        [(EditReviewWindowController*)[window valueForKey:@"editReviewController"] apply:nil];
    });
    __block BOOL rejected = NO;
    for(int i = 0; i < 200; ++i) {
        dispatch_sync(dispatch_get_main_queue(), ^{
            NSWindow* sheet = window.window.attachedSheet;
            if(sheet && [sheet.title isEqual:@"Edits Could Not Be Applied"]) rejected = YES;
            // NSAlert sheets do not consistently expose their message as title.
            BufferEditProposal* pending = [window valueForKey:@"editProposals"][proposal[@"proposalId"]];
            if([pending.status isEqual:@"stale"]) { rejected = YES; if(sheet) [window.window endSheet:sheet returnCode:NSAlertFirstButtonReturn]; }
        });
        if(rejected) break;
        usleep(10000);
    }
    OAK_ASSERT(rejected);
    OAK_ASSERT([context_test_query(environment, @"get_edit_proposal_status", statusArgs)[@"status"] isEqual:@"stale"]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([untitled.content isEqual:@"new user typing"]);
        [EditorContextService.sharedService revokeSession:session];
        [window setSelectedDocument:nil]; [window setDocuments:@[]];
        [document close]; [untitled close]; [window close]; [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:path error:nil]);
}

void test_editor_context_navigation_project_boundary ()
{
    NSString* root = [@"/tmp/tm-project-root-" stringByAppendingString:NSUUID.UUID.UUIDString];
    NSString* insideDirectory = [root stringByAppendingPathComponent:@"sub"];
    // A sibling whose name shares the root's prefix must not count as inside it.
    NSString* sibling = [root stringByAppendingString:@"2"];
    OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:insideDirectory withIntermediateDirectories:YES attributes:nil error:nil]);
    OAK_ASSERT([NSFileManager.defaultManager createDirectoryAtPath:sibling withIntermediateDirectories:YES attributes:nil error:nil]);
    NSString* insidePath = [insideDirectory stringByAppendingPathComponent:@"inside.txt"];
    NSString* siblingPath = [sibling stringByAppendingPathComponent:@"sibling.txt"];
    // Same-prefix sibling that is not open anywhere, so the boundary test is the only thing that can refuse it.
    NSString* siblingOtherPath = [sibling stringByAppendingPathComponent:@"other.txt"];
    NSString* loosePath = [root stringByAppendingPathComponent:@"loose.txt"];
    OAK_ASSERT([@"inside\n" writeToFile:insidePath atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    OAK_ASSERT([@"sibling\n" writeToFile:siblingPath atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    OAK_ASSERT([@"other\n" writeToFile:siblingOtherPath atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    OAK_ASSERT([@"loose\n" writeToFile:loosePath atomically:YES encoding:NSUTF8StringEncoding error:nil]);
    __block DocumentWindowController* window;
    __block DocumentWindowController* looseWindow;
    __block OakDocument* siblingDocument;
    __block OakDocument* looseDocument;
    __block NSDictionary* environment;
    __block NSDictionary* looseEnvironment;
    NSUUID* session = NSUUID.UUID;
    NSUUID* looseSession = NSUUID.UUID;
    dispatch_group_t loaded = dispatch_group_create();
    __block BOOL success = YES;
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        looseWindow = [[DocumentWindowController alloc] init];
        siblingDocument = [OakDocument documentWithPath:siblingPath];
        siblingDocument.fileType = @"text.plain";
        looseDocument = [OakDocument documentWithPath:loosePath];
        looseDocument.fileType = @"text.plain";
        for(OakDocument* document in @[siblingDocument, looseDocument]) {
            dispatch_group_enter(loaded);
            [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                success = success && status == OakDocumentIOResultSuccess;
                dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0 && success);
    dispatch_sync(dispatch_get_main_queue(), ^{
        // window: the user opened a file that lives outside the project root.
        [window setDocuments:@[siblingDocument]]; [window setSelectedDocument:siblingDocument];
        [window setValue:root forKey:@"projectPath"];
        // looseWindow: a document window without a project.
        [looseWindow setDocuments:@[looseDocument]]; [looseWindow setSelectedDocument:looseDocument];
        __weak DocumentWindowController* owner = window;
        __weak DocumentWindowController* looseOwner = looseWindow;
        environment = [EditorContextService.sharedService environmentForSession:session provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [owner editorContextForMethod:method arguments:args]; }];
        looseEnvironment = [EditorContextService.sharedService environmentForSession:looseSession provider:^EditorContextSnapshot*(NSString* method, NSDictionary* args){ return [looseOwner editorContextForMethod:method arguments:args]; }];
    });
    // Positive control: a file inside the root is opened as a tab and becomes readable.
    NSDictionary* result = context_test_query(environment, @"navigate_editor", @{ @"path": insidePath });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && [result[@"opened"] boolValue]);
    NSString* insideId = result[@"document"][@"documentId"];
    OAK_ASSERT([context_test_query(environment, @"read_document", @{ @"documentId": insideId })[@"text"] isEqual:@"inside\n"]);
    dispatch_sync(dispatch_get_main_queue(), ^{ OAK_ASSERT(window.documents.count == 2); });
    for(NSString* refused in @[@"/etc/hosts", siblingOtherPath]) {
        result = context_test_query(environment, @"navigate_editor", @{ @"path": refused });
        OAK_ASSERT([result[@"code"] isEqual:@"path_outside_project"]);
        OAK_ASSERT([context_test_query(environment)[@"document"][@"documentId"] isEqual:insideId]);
        dispatch_sync(dispatch_get_main_queue(), ^{ OAK_ASSERT(window.documents.count == 2); });
    }
    // A document the user already opened outside the project stays navigable.
    result = context_test_query(environment, @"navigate_editor", @{ @"documentId": siblingDocument.identifier.UUIDString });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && ![result[@"opened"] boolValue]);
    OAK_ASSERT([context_test_query(environment)[@"document"][@"documentId"] isEqual:siblingDocument.identifier.UUIDString]);
    // Without a project root, only documents already open in the window are reachable.
    result = context_test_query(looseEnvironment, @"navigate_editor", @{ @"path": siblingPath });
    OAK_ASSERT([result[@"code"] isEqual:@"path_outside_project"]);
    dispatch_sync(dispatch_get_main_queue(), ^{ OAK_ASSERT(looseWindow.documents.count == 1); });
    result = context_test_query(looseEnvironment, @"navigate_editor", @{ @"documentId": looseDocument.identifier.UUIDString });
    OAK_ASSERT([result[@"status"] isEqual:@"navigated"] && ![result[@"opened"] boolValue]);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [EditorContextService.sharedService revokeSession:session];
        [EditorContextService.sharedService revokeSession:looseSession];
        [window setSelectedDocument:nil]; [window setDocuments:@[]];
        [looseWindow setSelectedDocument:nil]; [looseWindow setDocuments:@[]];
        [siblingDocument close]; [looseDocument close];
        [window close]; [looseWindow close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:root error:nil]);
    OAK_ASSERT([NSFileManager.defaultManager removeItemAtPath:sibling error:nil]);
}

void test_editor_context_buffer_rejects_stale_revision ()
{
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block NSDictionary* metadata;
    __block EditorContextBuffer* current;
    __block NSString* staleRevision;
    dispatch_semaphore_t loaded = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication]; [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithString:@"first\n" fileType:@"text.plain" customName:@"stale revision buffer"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(loaded);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        metadata = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        staleRevision = metadata[@"revision"];
        OAK_ASSERT([EditorContextBuffer bufferForDocument:document revision:staleRevision] != nil);
        document.content = @"second\n";
        metadata = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        OAK_ASSERT(![metadata[@"revision"] isEqual:staleRevision]);
        // Current content under the old label is exactly what must not be handed out.
        OAK_ASSERT([EditorContextBuffer bufferForDocument:document revision:staleRevision] == nil);
        current = [EditorContextBuffer bufferForDocument:document revision:metadata[@"revision"]];
        OAK_ASSERT(current != nil && [[current valueForKey:@"revision"] isEqual:metadata[@"revision"]]);
        // A refused request must leave the cache untouched: the fresh snapshot stays cached.
        OAK_ASSERT([EditorContextBuffer bufferForDocument:document revision:metadata[@"revision"]] == current);
        OAK_ASSERT([EditorContextBuffer bufferForDocument:document revision:staleRevision] == nil);
        [window setSelectedDocument:nil]; [window setDocuments:@[]]; [document close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
}

void test_document_edit_proposal_across_undo_revision_rewind ()
{
    __block DocumentWindowController* window;
    __block OakDocument* document;
    __block NSDictionary* initial;
    __block NSDictionary* edited;
    __block NSDictionary* rewritten;
    __block NSDictionary* decided;
    __block EditorContextBuffer* initialBuffer;
    __block EditorContextBuffer* editedBuffer;
    __block EditorContextBuffer* rewrittenBuffer;
    __block size_t revisionAtStart = 0, revisionAfterEdit = 0, revisionAfterUndo = 0, revisionAfterRewrite = 0;
    dispatch_semaphore_t loaded = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication]; [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        document = [OakDocument documentWithString:@"A😀B\nkeep\n" fileType:@"text.plain" customName:@"undo rewind buffer"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            OAK_ASSERT(status == OakDocumentIOResultSuccess); dispatch_semaphore_signal(loaded);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[document]]; [window setSelectedDocument:document];
        initial = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        initialBuffer = [EditorContextBuffer bufferForDocument:document revision:initial[@"revision"]];
        revisionAtStart = [document buffer].revision();
    });
    OAK_ASSERT(initialBuffer != nil);
    NSDictionary* (^arguments)(NSDictionary*, NSArray*) = ^NSDictionary*(NSDictionary* metadata, NSArray* edits){
        return @{ @"documentId": metadata[@"documentId"], @"revision": metadata[@"revision"], @"summary": @"Edit across an undo rewind", @"edits": edits };
    };
    NSString* error = nil;
    NSDictionary* plan = [initialBuffer editPlanForArguments:arguments(initial, @[@{ @"offset": @1, @"length": @2, @"text": @"🛠️" }]) document:initial deadline:editor_context::now()+3];
    OAK_ASSERT(plan[@"error"] == nil && [plan[@"edits"][0][@"text"] isEqual:@"😀"]);
    BufferEditProposal* first = [[BufferEditProposal alloc] initWithEditPlan:plan summary:@"Replace the emoji" error:&error];
    OAK_ASSERT(first != nil && error == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([first applyToDocument:document]);
        OAK_ASSERT([document.content isEqual:@"A🛠️B\nkeep\n"]);
        edited = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
        revisionAfterEdit = [document buffer].revision();
        editedBuffer = [EditorContextBuffer bufferForDocument:document revision:edited[@"revision"]];
        [document undo];
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\n"]);
        revisionAfterUndo = [document buffer].revision();
        rewritten = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
    });
    // Undo rewinds buffer.revision(), so one numeric revision labels both the state
    // before the undone edit and the state after it; only the generation separates them.
    OAK_ASSERT(revisionAfterUndo == revisionAtStart && revisionAfterEdit > revisionAtStart);
    OAK_ASSERT([[initial[@"revision"] componentsSeparatedByString:@":"].firstObject isEqual:[rewritten[@"revision"] componentsSeparatedByString:@":"].firstObject]);
    OAK_ASSERT(![rewritten[@"revision"] isEqual:initial[@"revision"]]);
    // A proposal built before the edit and applied after the undo: its ranges still
    // describe the restored text, so only the revision label can retire it.
    BufferEditProposal* expired = [[BufferEditProposal alloc] initWithEditPlan:plan summary:@"Replace the emoji" error:&error];
    OAK_ASSERT(expired != nil && error == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT(![expired applyToDocument:document] && [expired.status isEqual:@"stale"]);
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\n"]);
        rewrittenBuffer = [EditorContextBuffer bufferForDocument:document revision:rewritten[@"revision"]];
    });
    // Proposing against the rewound state must land on the content it was built from.
    OAK_ASSERT(rewrittenBuffer != nil);
    NSDictionary* rewindPlan = [rewrittenBuffer editPlanForArguments:arguments(rewritten, @[@{ @"offset": @10, @"length": @0, @"text": @"tail\n" }]) document:rewritten deadline:editor_context::now()+3];
    OAK_ASSERT(rewindPlan[@"error"] == nil && [rewindPlan[@"edits"][0][@"text"] isEqual:@""]);
    BufferEditProposal* afterUndo = [[BufferEditProposal alloc] initWithEditPlan:rewindPlan summary:@"Append after undo" error:&error];
    OAK_ASSERT(afterUndo != nil && error == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT([afterUndo applyToDocument:document]);
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\ntail\n"]);
        revisionAfterRewrite = [document buffer].revision();
        decided = [window testEditorContextForMethod:@"get_editor_context" arguments:@{}][@"document"];
    });
    // This path never revisits a revision number already used, so a divergent buffer
    // sharing buffer.revision() with an earlier one is not reachable through undo.
    OAK_ASSERT(revisionAfterRewrite > revisionAfterEdit);
    OAK_ASSERT(![decided[@"revision"] isEqual:edited[@"revision"]]);
    // Its range still holds the text the proposal was built from, so the substring
    // backstop cannot catch it; it is retired only because its label belongs to an
    // earlier buffer state.
    NSDictionary* editedPlan = [editedBuffer editPlanForArguments:arguments(edited, @[@{ @"offset": @0, @"length": @1, @"text": @"B" }]) document:edited deadline:editor_context::now()+3];
    OAK_ASSERT(editedPlan[@"error"] == nil && [editedPlan[@"edits"][0][@"text"] isEqual:@"A"]);
    BufferEditProposal* fromEarlierState = [[BufferEditProposal alloc] initWithEditPlan:editedPlan summary:@"Edit an earlier state" error:&error];
    OAK_ASSERT(fromEarlierState != nil && error == nil);
    dispatch_sync(dispatch_get_main_queue(), ^{
        OAK_ASSERT(![fromEarlierState applyToDocument:document] && [fromEarlierState.status isEqual:@"stale"]);
        OAK_ASSERT([document.content isEqual:@"A😀B\nkeep\ntail\n"]);
        [window setSelectedDocument:nil]; [window setDocuments:@[]]; [document close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
}

static NSDictionary* context_test_snapshot_read (EditorContextBuffer* snapshot, NSDictionary* arguments)
{
    dispatch_group_t group = dispatch_group_create();
    __block NSDictionary* result = nil;
    dispatch_group_async(group, dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
        result = [snapshot responseForMethod:@"read_document" arguments:arguments base:@{} document:@{}
            ranges:ng::ranges_t() caret:0 deadline:editor_context::now()+10];
    });
    OAK_ASSERT(dispatch_group_wait(group, dispatch_time(DISPATCH_TIME_NOW, 10*NSEC_PER_SEC)) == 0);
    return result;
}

// context_buffer_t captures raw (data, size) pointers into the storage tree and
// dereferences them on a worker thread, which is only safe while the tree shares
// storage copy-on-write and later edits never write inside a captured extent. This
// pins that a snapshot never changes under later edits.
// Limit: without Thread Sanitizer this proves copy semantics under later mutation,
// not the absence of a data race between the main thread's edits and a worker's read.
void test_editor_context_snapshot_survives_later_edits ()
{
    NSString* original = @"head ünïcode 😀\nmid 😀é tail\nend\n";
    __block OakDocument* document;
    __block EditorContextBuffer* snapshot;
    __block BOOL loaded = NO;
    __block BOOL grammarWasLoaded = NO;
    dispatch_semaphore_t ready = dispatch_semaphore_create(0);
    dispatch_sync(dispatch_get_main_queue(), ^{
        document = [OakDocument documentWithString:original fileType:@"text.html.basic" customName:@"snapshot under edits"];
        [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
            loaded = status == OakDocumentIOResultSuccess;
            dispatch_semaphore_signal(ready);
        }];
    });
    OAK_ASSERT(dispatch_semaphore_wait(ready, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0);
    OAK_ASSERT(loaded);

    dispatch_sync(dispatch_get_main_queue(), ^{
        snapshot = [EditorContextBuffer bufferForDocument:document revision:nil];
        grammarWasLoaded = document.buffer.grammar() != nullptr;
    });
    OAK_ASSERT(snapshot != nil);

    NSDictionary* baseline = context_test_snapshot_read(snapshot, @{});
    OAK_ASSERT([baseline[@"text"] isEqual:original]);
    OAK_ASSERT_EQ([baseline[@"totalLength"] unsignedIntegerValue], original.length);
    OAK_ASSERT(![baseline[@"truncated"] boolValue]);
    OAK_ASSERT_EQ([baseline[@"nextOffset"] unsignedIntegerValue], original.length);

    // 120 rounds: the storage tree must split and clone, not merely extend.
    NSUInteger rounds = 120;
    for(NSUInteger round = 0; round < rounds; ++round) {
        __block NSDictionary* concurrent = nil;
        dispatch_group_t group = dispatch_group_create();
        dispatch_group_async(group, dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
            concurrent = [snapshot responseForMethod:@"read_document" arguments:@{} base:@{} document:@{}
                ranges:ng::ranges_t() caret:0 deadline:editor_context::now()+10];
        });
        dispatch_sync(dispatch_get_main_queue(), ^{ @autoreleasepool {
            switch(round % 5) {
                case 0: document.buffer.insert(document.buffer.size(), " tail 😀\n"); break;              // append past the captured extent
                case 1: document.buffer.insert(1, "MID "); break;                                          // split the first piece
                case 2: document.buffer.replace(0, 4, "HEAD"); break;                                      // replace a range
                case 3: if(document.buffer.size() > 8) document.buffer.erase(1, 3); break;                 // delete a range
                default: document.content = [original stringByAppendingString:@"\nrewritten"]; break;      // replace the whole buffer
            }
            // A synchronous parse takes the grammar mutex, so it can run while the
            // worker reads the snapshot. Only reachable when the test process has a
            // grammar for this file type, which the final log line reports.
            if(grammarWasLoaded)
                document.buffer.wait_for_repair();
        } });
        OAK_ASSERT(dispatch_group_wait(group, dispatch_time(DISPATCH_TIME_NOW, 10*NSEC_PER_SEC)) == 0);
        OAK_ASSERT([concurrent[@"text"] isEqual:original]);
        OAK_ASSERT_EQ([concurrent[@"totalLength"] unsignedIntegerValue], original.length);
    }

    // The live buffer really did change, so the snapshot above was not trivially stable.
    __block std::string live;
    dispatch_sync(dispatch_get_main_queue(), ^{ live = document.buffer.substr(0, document.buffer.size()); });
    OAK_ASSERT(live != std::string(original.UTF8String));

    // Repeat reads stay stable, including the astral character's byte/UTF-16 mapping.
    NSUInteger astral = [original rangeOfString:@"😀"].location;
    NSDictionary* probe = context_test_snapshot_read(snapshot, @{ @"offset": @(astral), @"length": @2 });
    OAK_ASSERT([probe[@"text"] isEqual:@"😀"]);
    OAK_ASSERT_EQ([probe[@"nextOffset"] unsignedIntegerValue], astral + 2);
    NSDictionary* again = context_test_snapshot_read(snapshot, @{});
    NSDictionary* third = context_test_snapshot_read(snapshot, @{});
    OAK_ASSERT([again[@"text"] isEqual:original]);
    OAK_ASSERT([third[@"text"] isEqual:again[@"text"]]);

    fprintf(stderr, "snapshot under edits: %lu rounds, %s grammar\n", (unsigned long)rounds, grammarWasLoaded ? "with" : "without");
    dispatch_sync(dispatch_get_main_queue(), ^{ [document close]; });
}

// EditorContextChanges belongs to the window, not to a session: it is a window ivar
// (DocumentWindowController.mm:115, created at :469) and every get_editor_changes
// answers from self.editorChanges (:228), so all sessions registered against one
// window share one event sequence and its 128-event cap (EditorContextChanges.mm:16).
// Events carry type and documentId only, so the journal cannot attribute an event to
// the session that caused it.
void test_editor_context_change_journal_is_per_window ()
{
    __block DocumentWindowController* window;
    __block OakDocument* first;
    __block OakDocument* second;
    __block NSUUID* firstSession;
    __block NSUUID* secondSession;
    __block NSDictionary* firstEnvironment;
    __block NSDictionary* secondEnvironment;
    dispatch_group_t loaded = dispatch_group_create();
    __block BOOL success = YES;
    dispatch_sync(dispatch_get_main_queue(), ^{
        [NSApplication sharedApplication];
        [DocumentWindowController disableSessionSave];
        window = [[DocumentWindowController alloc] init];
        first = [OakDocument documentWithString:@"first window journal\n" fileType:@"text.plain" customName:@"journal first"];
        second = [OakDocument documentWithString:@"second window journal\n" fileType:@"text.plain" customName:@"journal second"];
        for(OakDocument* document in @[first, second]) {
            dispatch_group_enter(loaded);
            [document loadModalForWindow:nil completionHandler:^(OakDocumentIOResult status, NSString*, oak::uuid_t const&){
                success = success && status == OakDocumentIOResultSuccess;
                dispatch_group_leave(loaded);
            }];
        }
    });
    OAK_ASSERT(dispatch_group_wait(loaded, dispatch_time(DISPATCH_TIME_NOW, 5*NSEC_PER_SEC)) == 0 && success);
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setDocuments:@[first, second]];
        [window setSelectedDocument:first];
        // Two distinct sessions, one provider: both resolve this window's state.
        EditorContextProvider provider = ^EditorContextSnapshot*(NSString* method, NSDictionary* arguments){
            return [window editorContextForMethod:method arguments:arguments];
        };
        firstSession = NSUUID.UUID;
        secondSession = NSUUID.UUID;
        firstEnvironment = [EditorContextService.sharedService environmentForSession:firstSession provider:provider];
        secondEnvironment = [EditorContextService.sharedService environmentForSession:secondSession provider:provider];
    });

    // Identity: two sessions reading the same untouched journal get byte-identical
    // cursors, because both were rendered from the same EditorContextChanges instance.
    NSDictionary* initialFirst = context_test_query(firstEnvironment, @"get_editor_changes", @{});
    NSDictionary* initialSecond = context_test_query(secondEnvironment, @"get_editor_changes", @{});
    OAK_ASSERT([initialFirst[@"windowId"] isEqual:initialSecond[@"windowId"]]);
    OAK_ASSERT([initialFirst[@"cursor"] isEqual:initialSecond[@"cursor"]]);
    OAK_ASSERT([initialFirst[@"reset"] boolValue] && [initialSecond[@"reset"] boolValue]);
    NSString* cursor = initialFirst[@"cursor"];

    // A change the first session observes: switch document, then edit content.
    dispatch_sync(dispatch_get_main_queue(), ^{
        [window setSelectedDocument:second];
        first.content = @"edited while the first session was the caller";
    });

    // The second session resumes from the first session's cursor and sees those events
    // without a reset, which a per-session journal could not do.
    NSDictionary* fromSecond = context_test_query(secondEnvironment, @"get_editor_changes", @{ @"cursor": cursor });
    OAK_ASSERT(![fromSecond[@"reset"] boolValue]);
    OAK_ASSERT([fromSecond[@"events"] count] > 0);
    OAK_ASSERT([[fromSecond[@"events"] valueForKey:@"type"] containsObject:@"document"]);
    OAK_ASSERT([[fromSecond[@"events"] valueForKey:@"type"] containsObject:@"content"]);
    OAK_ASSERT([fromSecond[@"context"][@"document"][@"documentId"] isEqual:second.identifier.UUIDString]);
    OAK_ASSERT(fromSecond[@"events"][0][@"session"] == nil);
    NSDictionary* fromFirst = context_test_query(firstEnvironment, @"get_editor_changes", @{ @"cursor": fromSecond[@"cursor"] });
    OAK_ASSERT(![fromFirst[@"reset"] boolValue] && ![fromFirst[@"changed"] boolValue]);

    // One 128-event history: a flood recorded once retires the same cursor for both.
    NSString* beforeFlood = fromSecond[@"cursor"];
    dispatch_sync(dispatch_get_main_queue(), ^{
        EditorContextChanges* journal = [window valueForKey:@"editorChanges"];
        for(int i = 0; i < 400; ++i)
            [journal record:@"selection" documentId:second.identifier.UUIDString];
    });
    NSDictionary* floodedFirst = context_test_query(firstEnvironment, @"get_editor_changes", @{ @"cursor": beforeFlood });
    NSDictionary* floodedSecond = context_test_query(secondEnvironment, @"get_editor_changes", @{ @"cursor": beforeFlood });
    OAK_ASSERT([floodedFirst[@"reset"] boolValue] && [floodedSecond[@"reset"] boolValue]);
    OAK_ASSERT([floodedFirst[@"cursor"] isEqual:floodedSecond[@"cursor"]]);
    OAK_ASSERT_EQ([floodedFirst[@"events"] count], (NSUInteger)0);

    // The retained window is the last 128 events for both sessions: from the
    // post-flood cursor exactly 127 further records come back, and the 128th slot
    // still holds the cursor's own event. A per-session journal, or a smaller cap,
    // would report a reset here instead.
    NSString* retainedStart = floodedFirst[@"cursor"];
    dispatch_sync(dispatch_get_main_queue(), ^{
        EditorContextChanges* journal = [window valueForKey:@"editorChanges"];
        for(int i = 0; i < 127; ++i)
            [journal record:@"content" documentId:second.identifier.UUIDString];
    });
    NSDictionary* retainedFirst = context_test_query(firstEnvironment, @"get_editor_changes", @{ @"cursor": retainedStart });
    NSDictionary* retainedSecond = context_test_query(secondEnvironment, @"get_editor_changes", @{ @"cursor": retainedStart });
    OAK_ASSERT(![retainedFirst[@"reset"] boolValue] && ![retainedSecond[@"reset"] boolValue]);
    OAK_ASSERT_EQ([retainedFirst[@"events"] count], (NSUInteger)127);
    OAK_ASSERT_EQ([retainedSecond[@"events"] count], (NSUInteger)127);

    dispatch_sync(dispatch_get_main_queue(), ^{
        [EditorContextService.sharedService revokeSession:firstSession];
        [EditorContextService.sharedService revokeSession:secondSession];
        [window setSelectedDocument:nil]; [window setDocuments:@[]];
        [first close]; [second close]; [window close];
        [DocumentWindowController enableSessionSave];
    });
    OAK_ASSERT([context_test_query(firstEnvironment)[@"code"] isEqual:@"session_unavailable"]);
    OAK_ASSERT([context_test_query(secondEnvironment)[@"code"] isEqual:@"session_unavailable"]);
}
