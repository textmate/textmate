#import "EditorContextService.h"
#import "EditorContextBuffer.h"
#import <EditorContextProtocol.h>
#import <Cocoa/Cocoa.h>
#import <document/OakDocument.h>
#import <document/OakDocument Private.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <fcntl.h>

@interface EditorContextSnapshot ()
@property (copy) NSDictionary* (^renderer)(double);
@property (copy) EditorContextAction action;
@property (copy) BOOL (^authorization)(void);
@end
@implementation EditorContextSnapshot
+ (instancetype)response:(NSDictionary*)response { return [self renderer:^NSDictionary*(double){ return response; }]; }
+ (instancetype)renderer:(NSDictionary* (^)(double))renderer {
    EditorContextSnapshot* snapshot = [[self alloc] init];
    snapshot.renderer = renderer;
    return snapshot;
}
+ (instancetype)action:(EditorContextAction)action {
    EditorContextSnapshot* snapshot = [[self alloc] init];
    snapshot.action = action;
    return snapshot;
}
- (NSDictionary*)renderBefore:(double)deadline {
    if(editor_context::now() >= deadline) return editor_context::failure(@"The editor request timed out.", @"timeout");
    if(_action) {
        if(NSThread.isMainThread) return editor_context::failure(@"Editor actions must be awaited on a worker thread.", @"invalid_request");
        __block NSDictionary* result;
        dispatch_semaphore_t finished = dispatch_semaphore_create(0);
        dispatch_async(dispatch_get_main_queue(), ^{
            BOOL (^canCommit)(void) = ^{ return editor_context::now() < deadline && (!self.authorization || self.authorization()); };
            if(!canCommit()) {
                result = editor_context::failure(@"The editor action expired or its terminal session changed.", @"navigation_cancelled");
                dispatch_semaphore_signal(finished);
                return;
            }
            self.action(deadline, canCommit, ^(NSDictionary* response){ result = response; dispatch_semaphore_signal(finished); });
        });
        if(dispatch_semaphore_wait(finished, dispatch_time(DISPATCH_TIME_NOW, MAX(0.0, deadline-editor_context::now()) * NSEC_PER_SEC)))
            return editor_context::failure(@"The editor request timed out.", @"timeout");
        return result;
    }
    return _renderer(deadline);
}
@end

@interface EditorContextService ()
@property NSMutableDictionary<NSString*, NSDictionary*>* registrations;
@property NSMapTable<id, NSNumber*>* contentGenerations;
@property NSString* socketPath;
@property NSString* socketDirectory;
@property dispatch_source_t listener;
@end

@implementation EditorContextService
+ (instancetype)sharedService {
    static EditorContextService* service;
    static dispatch_once_t once;
    dispatch_once(&once, ^{ service = [[self alloc] init]; });
    return service;
}
- (instancetype)init {
    if((self = [super init])) {
        _registrations = [NSMutableDictionary dictionary];
        _contentGenerations = [NSMapTable weakToStrongObjectsMapTable];
        [NSNotificationCenter.defaultCenter addObserver:self selector:@selector(contentDidChange:) name:OakDocumentContentDidChangeNotification object:nil];
        // /tmp keeps sockaddr_un within macOS's short path limit.
        char directory[] = "/tmp/textmate-context.XXXXXX";
        if(!mkdtemp(directory)) return self;
        _socketDirectory = @(directory);
        _socketPath = [_socketDirectory stringByAppendingPathComponent:@"context.sock"];
        int fd = socket(AF_UNIX, SOCK_STREAM, 0);
        struct sockaddr_un address = {};
        address.sun_family = AF_UNIX;
        strlcpy(address.sun_path, _socketPath.fileSystemRepresentation, sizeof(address.sun_path));
        if(fd < 0 || bind(fd, (struct sockaddr*)&address, sizeof(address)) || chmod(address.sun_path, 0600) || listen(fd, 8)) {
            if(fd >= 0) close(fd);
            unlink(address.sun_path);
            rmdir(directory);
            _socketPath = nil;
            return self;
        }
        fcntl(fd, F_SETFD, FD_CLOEXEC);
        fcntl(fd, F_SETFL, O_NONBLOCK);
        dispatch_queue_t queue = dispatch_queue_create("org.textmate.editor-context", DISPATCH_QUEUE_SERIAL);
        _listener = dispatch_source_create(DISPATCH_SOURCE_TYPE_READ, fd, 0, queue);
        __weak EditorContextService* weakSelf = self;
        dispatch_semaphore_t slots = dispatch_semaphore_create(8);
        __weak dispatch_source_t weakListener = _listener;
        dispatch_source_set_event_handler(_listener, ^{
            int client = accept(fd, nullptr, nullptr);
            if(client < 0) {
                // The pending connection stays readable while descriptors are exhausted, so
                // the source would otherwise fire again at once and spin this queue.
                if(dispatch_source_t source = weakListener; source && (errno == EMFILE || errno == ENFILE)) {
                    dispatch_suspend(source);
                    dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 100 * NSEC_PER_MSEC), queue, ^{ dispatch_resume(source); });
                }
                return;
            }
            if(dispatch_semaphore_wait(slots, DISPATCH_TIME_NOW)) { close(client); return; }
            double deadline = editor_context::now() + editor_context::request_timeout;
            editor_context::configure_socket(client);
            dispatch_async(dispatch_get_global_queue(QOS_CLASS_UTILITY, 0), ^{
                @autoreleasepool {
                    NSDictionary* request = editor_context::parse(editor_context::read_line(client, deadline));
                    __block EditorContextSnapshot* snapshot;
                    dispatch_semaphore_t captured = dispatch_semaphore_create(0);
                    dispatch_async(dispatch_get_main_queue(), ^{
                        if(editor_context::now() < deadline) snapshot = [weakSelf snapshotForRequest:request];
                        dispatch_semaphore_signal(captured);
                    });
                    int64_t remaining = MAX(0.0, deadline - editor_context::now()) * NSEC_PER_SEC;
                    NSDictionary* response = nil;
                    if(dispatch_semaphore_wait(captured, dispatch_time(DISPATCH_TIME_NOW, remaining)) == 0)
                        response = [snapshot renderBefore:deadline];
                    response = response ?: editor_context::failure(@"The editor request timed out.", @"timeout");
                    auto written = editor_context::write_json_result(client, response, deadline);
                    if(!written.sent) {
                        // The client sees only an empty reply, which it reports as invalid_response or
                        // timeout, so the distinction between an oversized payload and a stalled peer
                        // must be recorded here or it is lost.
                        char const* reason = written.error == editor_context::write_error::too_large ? "payload at or over message_limit" :
                            written.error == editor_context::write_error::unencodable ? "payload is not JSON encodable" : "socket send failed or timed out";
                        NSLog(@"EditorContextService: %@ response not sent (%s): %zu payload bytes, message_limit %zu", request[@"method"], reason, written.length, editor_context::message_limit);
                    }
                    close(client);
                    dispatch_semaphore_signal(slots);
                }
            });
        });
        dispatch_source_set_cancel_handler(_listener, ^{ close(fd); });
        dispatch_resume(_listener);
        [NSNotificationCenter.defaultCenter addObserver:self selector:@selector(shutdown) name:NSApplicationWillTerminateNotification object:nil];
    }
    return self;
}
- (void)contentDidChange:(NSNotification*)notification {
    id document = notification.object;
    [EditorContextBuffer invalidateDocument:document];
    [_contentGenerations setObject:@([[_contentGenerations objectForKey:document] unsignedIntegerValue] + 1) forKey:document];
}
- (NSString*)revisionForDocument:(OakDocument*)document {
    NSUInteger generation = [[_contentGenerations objectForKey:document] unsignedIntegerValue];
    return [NSString stringWithFormat:@"%zu:%lu", [document buffer].revision(), (unsigned long)generation];
}
- (void)shutdown {
    [_registrations removeAllObjects];
    if(_listener) dispatch_source_cancel(_listener);
    if(_socketPath) unlink(_socketPath.fileSystemRepresentation);
    if(_socketDirectory) rmdir(_socketDirectory.fileSystemRepresentation);
    _socketPath = nil;
}
- (NSDictionary*)environmentForSession:(NSUUID*)session provider:(EditorContextProvider)provider {
    NSAssert(NSThread.isMainThread, @"Register editor context on the main thread");
    if(!_socketPath || !provider) return @{};
    NSString* key = session.UUIDString;
    NSString* token = _registrations[key][@"token"] ?: [NSUUID.UUID.UUIDString stringByAppendingString:NSUUID.UUID.UUIDString];
    _registrations[key] = @{ @"token": token, @"provider": [provider copy] };
    return @{ @"TM_CONTEXT_SOCKET": _socketPath, @"TM_CONTEXT_SESSION": key, @"TM_CONTEXT_TOKEN": token };
}
- (void)revokeSession:(NSUUID*)session {
    NSAssert(NSThread.isMainThread, @"Revoke editor context on the main thread");
    [_registrations removeObjectForKey:session.UUIDString];
}
- (EditorContextSnapshot*)snapshotForRequest:(NSDictionary*)request {
    NSString* session = request[@"session"];
    NSString* token = request[@"token"];
    NSString* method = request[@"method"];
    NSDictionary* arguments = request[@"arguments"];
    if(![session isKindOfClass:NSString.class] || ![token isKindOfClass:NSString.class] ||
       ![method isKindOfClass:NSString.class] || ![arguments isKindOfClass:NSDictionary.class])
        return [EditorContextSnapshot response:editor_context::failure(@"Invalid context request", @"invalid_request")];
    // Tokens are 72 characters (two UUIDs) and sessions are 36; bounding both before
    // the lookup keeps a 1 MB request string out of the registrations dictionary.
    if(session.length > 256 || token.length > 256)
        return [EditorContextSnapshot response:editor_context::failure(@"Invalid context request", @"invalid_request")];
    NSDictionary* entry = _registrations[session];
    if(!entry || ![entry[@"token"] isEqual:token]) return [EditorContextSnapshot response:editor_context::failure(@"This terminal session has closed or its credentials are invalid.", @"session_unavailable")];
    EditorContextProvider provider = entry[@"provider"];
    EditorContextSnapshot* snapshot = provider(method, arguments);
    // A terminal transfer replaces its registration even though credentials stay
    // stable. Delayed navigation must not act in its former window after a move.
    __weak EditorContextService* weakSelf = self;
    snapshot.authorization = ^{ return weakSelf.registrations[session] == entry; };
    return snapshot ?: [EditorContextSnapshot response:editor_context::failure(@"The editor window has closed.", @"window_closed")];
}
@end
