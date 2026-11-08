#pragma once
#import <EditorContextProtocol.h>

namespace editor_context {
inline NSDictionary* query(NSString* method, NSDictionary* arguments) {
    NSDictionary* env = NSProcessInfo.processInfo.environment;
    NSString* path = env[@"TM_CONTEXT_SOCKET"];
    NSString* session = env[@"TM_CONTEXT_SESSION"];
    NSString* token = env[@"TM_CONTEXT_TOKEN"];
    sockaddr_un address = {};
    if(!path.length || !session.length || !token.length)
        return failure(@"Open a new TextMate terminal tab; editor connection variables are missing.", @"missing_environment");
    if([path lengthOfBytesUsingEncoding:NSUTF8StringEncoding] >= sizeof(address.sun_path))
        return failure(@"The editor socket path is invalid.", @"invalid_socket");
    address.sun_family = AF_UNIX;
    strlcpy(address.sun_path, path.fileSystemRepresentation, sizeof(address.sun_path));
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if(fd < 0) return failure(@"Cannot create the editor connection.", @"socket_error");
    configure_socket(fd);
    double deadline = now() + request_timeout;
    if(!connect_socket(fd, address, deadline)) {
        int error = errno;
        close(fd);
        if(error == ETIMEDOUT) return failure(@"TextMate did not respond within three seconds.", @"timeout");
        if(error == ENOENT || error == ECONNREFUSED) return failure(@"TextMate's editor service is unavailable. Reopen the terminal in the running app.", @"service_unavailable");
        if(error == EACCES || error == EPERM) return failure(@"The editor connection was denied by filesystem or sandbox permissions.", @"permission_denied");
        return failure(@"Cannot connect to TextMate's editor service.", @"connection_failed");
    }
    NSDictionary* result = nil;
    if(write_json(fd, @{ @"session": session, @"token": token, @"method": method, @"arguments": arguments }, deadline))
        result = parse(read_line(fd, deadline));
    close(fd);
    if(result[@"error"] && ![result[@"error"] isKindOfClass:NSString.class])
        return failure(@"TextMate returned an invalid error response.", @"invalid_response");
    if(result[@"error"] && ![result[@"code"] isKindOfClass:NSString.class])
        return failure(result[@"error"], @"context_error");
    return result ?: (now() >= deadline ? failure(@"TextMate did not respond within three seconds.", @"timeout") :
                     failure(@"TextMate returned an incomplete or invalid response.", @"invalid_response"));
}
inline int check_connection() {
    NSDictionary* result = query(@"get_connection_status", @{});
    if(result[@"error"]) {
        fprintf(stderr, "TextMate connection failed (%s): %s\n", [result[@"code"] UTF8String], [result[@"error"] UTF8String]);
        return 1;
    }
    if(![result[@"windowId"] isKindOfClass:NSString.class] || ![result[@"documentName"] isKindOfClass:NSString.class]) {
        fprintf(stderr, "TextMate connection failed (invalid_response): The editor returned invalid connection details.\n");
        return 1;
    }
    printf("Connected to TextMate window %s\n", [result[@"windowId"] UTF8String] ?: "unknown");
    printf("Current document: %s\n", [result[@"documentName"] UTF8String] ?: "none");
    return 0;
}
}
