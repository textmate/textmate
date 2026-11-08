#pragma once
#import <Foundation/Foundation.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include <poll.h>
#include <fcntl.h>
#include <chrono>
#include <limits>

// Private, bounded, one-request-per-connection protocol. MCP itself uses stdio.
namespace editor_context {
constexpr size_t message_limit = 1024 * 1024;
constexpr double request_timeout = 3;
constexpr NSUInteger identifier_limit = 256;
constexpr NSUInteger path_limit = 4096;

// The one list of MCP tools, in advertised order. tools/list, the tools/call
// allow-list, argument validation and window dispatch all derive from it.
inline NSArray<NSString*>* tool_names() {
    static NSArray* names = @[ @"get_editor_context", @"list_open_documents", @"read_document", @"get_diagnostics",
        @"get_definition", @"find_references", @"get_document_symbols", @"search_workspace_symbols", @"get_hover",
        @"navigate_editor", @"propose_document_edits", @"get_edit_proposal_status", @"get_editor_changes" ];
    return names;
}
inline bool is_language_query(NSString* method) {
    return [@[@"get_definition", @"find_references", @"get_document_symbols", @"search_workspace_symbols", @"get_hover"] containsObject:method];
}
inline bool is_position_query(NSString* method) {
    return [@[@"get_definition", @"find_references", @"get_hover"] containsObject:method];
}
inline bool is_mutating_tool(NSString* method) {
    return [@[@"navigate_editor", @"propose_document_edits"] containsObject:method];
}
inline double now() { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); }
inline bool wait_for(int fd, short events, double deadline) {
    while(now() < deadline) {
        pollfd descriptor = { fd, events, 0 };
        // Capped so an unbounded (infinite) deadline never overflows the int timeout.
        int status = poll(&descriptor, 1, (int)MIN(MAX(1.0, (deadline-now()) * 1000), 60000.0));
        if(status < 0 && errno == EINTR) continue;
        if(status < 0) return false;
        if(status > 0 && (descriptor.revents & (events | POLLHUP))) return true;
        if(status > 0) return false;
    }
    errno = ETIMEDOUT;
    return false;
}
inline NSData* read_line(int fd, double deadline = now() + request_timeout) {
    NSMutableData* data = [NSMutableData data];
    char bytes[4096];
    while(data.length < message_limit && wait_for(fd, POLLIN, deadline)) {
        ssize_t n = recv(fd, bytes, MIN(sizeof(bytes), message_limit - data.length), MSG_DONTWAIT);
        if(n < 0 && (errno == EINTR || errno == EAGAIN)) continue;
        if(n <= 0) return nil;
        char const* newline = static_cast<char const*>(memchr(bytes, '\n', n));
        [data appendBytes:bytes length:newline ? newline - bytes : n];
        if(newline) return data;
    }
    return nil;
}
enum class write_error { none, unencodable, too_large, send_failed };
struct write_result_t { bool sent; size_t length; write_error error; };
// The peer only ever sees a closed socket, so failures are reported in enough
// detail for the server to record why a client got nothing.
inline write_result_t write_json_result(int fd, id object, double deadline = now() + request_timeout) {
    NSData* encoded = [NSJSONSerialization dataWithJSONObject:object options:NSJSONWritingWithoutEscapingSlashes error:nil];
    if(!encoded) return { false, 0, write_error::unencodable };
    size_t length = encoded.length + 1;
    if(encoded.length >= message_limit) return { false, length, write_error::too_large };
    NSMutableData* data = [encoded mutableCopy];
    [data appendBytes:"\n" length:1];
    auto bytes = static_cast<char const*>(data.bytes);
    size_t left = data.length;
    int type;
    socklen_t size = sizeof(type);
    bool socket = getsockopt(fd, SOL_SOCKET, SO_TYPE, &type, &size) == 0;
    // A pipe carries the MCP host's JSON-RPC stream, where a timeout would leave a
    // torn frame. Only sockets, whose peer may be unresponsive, get a deadline.
    if(!socket) deadline = std::numeric_limits<double>::infinity();
    while(left && wait_for(fd, POLLOUT, deadline)) {
        size_t count = MIN(left, (size_t)4096);
        ssize_t n = socket ? send(fd, bytes, count, MSG_DONTWAIT) : write(fd, bytes, count);
        if(n < 0 && (errno == EINTR || errno == EAGAIN)) continue;
        if(n <= 0) return { false, length, write_error::send_failed };
        bytes += n;
        left -= n;
    }
    return { left == 0, length, left ? write_error::send_failed : write_error::none };
}
inline bool write_json(int fd, id object, double deadline = now() + request_timeout) {
    return write_json_result(fd, object, deadline).sent;
}
inline NSDictionary* parse(NSData* data) {
    id value = data ? [NSJSONSerialization JSONObjectWithData:data options:0 error:nil] : nil;
    return [value isKindOfClass:NSDictionary.class] ? value : nil;
}
inline NSDictionary* failure(NSString* message, NSString* code = @"context_error") { return @{ @"error": message, @"code": code }; }
inline void configure_socket(int fd) {
    int yes = 1;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &yes, sizeof(yes));
    fcntl(fd, F_SETFD, FD_CLOEXEC);
}
inline bool connect_socket(int fd, sockaddr_un const& address, double deadline) {
    int flags = fcntl(fd, F_GETFL);
    if(flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) return false;
    int status = connect(fd, (sockaddr const*)&address, sizeof(address));
    if(status < 0 && errno == EINPROGRESS) {
        if(!wait_for(fd, POLLOUT, deadline)) return false;
        int error = 0;
        socklen_t length = sizeof(error);
        if(getsockopt(fd, SOL_SOCKET, SO_ERROR, &error, &length) || error) { if(error) errno = error; return false; }
    }
    else if(status < 0) return false;
    return true;
}
inline bool integer(id value, double maximum) {
    return [value isKindOfClass:NSNumber.class] && CFGetTypeID((__bridge CFTypeRef)value) != CFBooleanGetTypeID() &&
        [value doubleValue] >= 0 && [value doubleValue] <= maximum && floor([value doubleValue]) == [value doubleValue];
}
inline bool identifier(id value, NSUInteger limit = identifier_limit) {
    return [value isKindOfClass:NSString.class] && [value length] && [value length] <= limit;
}
inline NSString* validate_arguments(NSString* method, NSDictionary* arguments) {
    if(![arguments isKindOfClass:NSDictionary.class]) return @"Expected object arguments";
    if([method isEqual:@"get_editor_context"] || [method isEqual:@"list_open_documents"] || [method isEqual:@"get_connection_status"] || [method isEqual:@"get_editor_change_token"])
        return arguments.count ? @"This tool takes no arguments" : nil;
    if([method isEqual:@"get_editor_changes"]) {
        for(NSString* key in arguments) if(![key isEqual:@"cursor"]) return @"Unknown argument";
        if(arguments[@"cursor"] && !identifier(arguments[@"cursor"])) return @"cursor must be a nonempty string of at most 256 characters";
        return nil;
    }
    if(is_language_query(method)) {
        BOOL position = is_position_query(method);
        NSMutableArray* keys = [@[@"documentId", @"revision", @"limit", @"offset", @"resultId"] mutableCopy];
        if(position) [keys addObjectsFromArray:@[@"line", @"column"]];
        if([method isEqual:@"find_references"]) [keys addObject:@"includeDeclaration"];
        if([method isEqual:@"search_workspace_symbols"]) [keys addObject:@"query"];
        for(NSString* key in arguments) if(![keys containsObject:key]) return @"Unknown argument";
        for(NSString* key in @[@"documentId", @"revision", @"resultId"])
            if(arguments[key] && !identifier(arguments[key])) return @"documentId, revision, and resultId must be nonempty strings of at most 256 characters";
        for(NSString* key in @[@"line", @"column"])
            if(arguments[key] && (!integer(arguments[key], 2147483647) || [arguments[key] integerValue] < 1)) return @"Lines and columns must be positive integers";
        if(arguments[@"column"] && !arguments[@"line"]) return @"line is required with column";
        if(arguments[@"offset"] && !integer(arguments[@"offset"], 1000)) return @"offset must be an integer between 0 and 1000";
        if([arguments[@"offset"] integerValue] && !arguments[@"resultId"]) return @"resultId is required to page a previous result";
        if(arguments[@"limit"] && (!integer(arguments[@"limit"], 100) || [arguments[@"limit"] integerValue] < 1)) return @"limit must be an integer between 1 and 100";
        if(arguments[@"includeDeclaration"] && (![arguments[@"includeDeclaration"] isKindOfClass:NSNumber.class] || CFGetTypeID((__bridge CFTypeRef)arguments[@"includeDeclaration"]) != CFBooleanGetTypeID())) return @"includeDeclaration must be a boolean";
        if([method isEqual:@"search_workspace_symbols"] && (!arguments[@"resultId"] || arguments[@"query"]))
            if(!identifier(arguments[@"query"])) return @"query must contain between 1 and 256 characters";
        return nil;
    }
    if([method isEqual:@"propose_document_edits"] || [method isEqual:@"get_edit_proposal_status"]) {
        BOOL proposal = [method isEqual:@"propose_document_edits"];
        NSArray* keys = proposal ? @[@"documentId", @"revision", @"summary", @"edits"] : @[@"proposalId"];
        for(NSString* key in arguments) if(![keys containsObject:key]) return @"Unknown argument";
        for(NSString* key in proposal ? @[@"documentId", @"revision"] : @[@"proposalId"])
            if(!identifier(arguments[key])) return @"Document, revision, and proposal identifiers must be nonempty strings of at most 256 characters";
        if(!proposal) return nil;
        if(![arguments[@"summary"] isKindOfClass:NSString.class] || ![arguments[@"summary"] length] || [arguments[@"summary"] length] > 2048) return @"summary must contain between 1 and 2,048 characters";
        NSArray* edits = arguments[@"edits"];
        if(![edits isKindOfClass:NSArray.class] || !edits.count || edits.count > 128) return @"edits must contain between 1 and 128 ranges";
        NSUInteger oldLength = 0, newLength = 0;
        for(id edit in edits) {
            if(![edit isKindOfClass:NSDictionary.class] || [edit count] != 3 || !integer(edit[@"offset"], 9007199254740991.0) || !integer(edit[@"length"], 131072)) return @"Each edit must contain a nonnegative UTF-16 offset, length, and replacement text";
            if(![edit[@"text"] isKindOfClass:NSString.class] || ![edit[@"text"] canBeConvertedToEncoding:NSUTF8StringEncoding]) return @"Each replacement text must be valid Unicode";
            oldLength += [edit[@"length"] unsignedIntegerValue];
            newLength += [edit[@"text"] length];
            if(oldLength > 131072 || newLength > 32768) return @"Edits may replace at most 131,072 UTF-16 units with at most 32,768 replacement units in total";
        }
        return nil;
    }
    if([method isEqual:@"navigate_editor"]) {
        for(NSString* key in arguments)
            if(![@[@"documentId", @"path", @"line", @"column", @"endLine", @"endColumn", @"revision", @"focus"] containsObject:key]) return @"Unknown argument";
        if(!!arguments[@"documentId"] == !!arguments[@"path"]) return @"Provide exactly one of documentId or path";
        for(NSString* key in @[@"documentId", @"path", @"revision"])
            if(arguments[key] && (!identifier(arguments[key], [key isEqual:@"path"] ? path_limit : identifier_limit) || [arguments[key] rangeOfString:[NSString stringWithFormat:@"%C", (unichar)0]].location != NSNotFound)) return @"documentId, path, and revision must be nonempty strings without NUL characters, within their length limits";
        if(arguments[@"path"] && ![arguments[@"path"] hasPrefix:@"/"]) return @"path must be an absolute local file path";
        for(NSString* key in @[@"line", @"column", @"endLine", @"endColumn"])
            if(arguments[key] && (!integer(arguments[key], 9007199254740991.0) || [arguments[key] doubleValue] < 1)) return @"Lines and columns must be positive integers";
        if(!arguments[@"line"] && (arguments[@"column"] || arguments[@"endLine"] || arguments[@"endColumn"])) return @"line is required when specifying columns or a range";
        if(!!arguments[@"endLine"] != !!arguments[@"endColumn"]) return @"endLine and endColumn must be provided together";
        if(arguments[@"endLine"] && ([arguments[@"endLine"] doubleValue] < [arguments[@"line"] doubleValue] ||
           ([arguments[@"endLine"] isEqual:arguments[@"line"]] && [arguments[@"endColumn"] doubleValue] < (arguments[@"column"] ? [arguments[@"column"] doubleValue] : 1)))) return @"The range end must not precede its start";
        if(arguments[@"focus"] && (![arguments[@"focus"] isKindOfClass:NSNumber.class] || CFGetTypeID((__bridge CFTypeRef)arguments[@"focus"]) != CFBooleanGetTypeID())) return @"focus must be a boolean";
        return nil;
    }
    if([method isEqual:@"get_diagnostics"]) {
        for(NSString* key in arguments)
            if(![@[@"documentId", @"offset", @"limit", @"reportId"] containsObject:key]) return @"Unknown argument";
        for(NSString* key in @[@"documentId", @"reportId"])
            if(arguments[key] && !identifier(arguments[key])) return @"documentId and reportId must be nonempty strings of at most 256 characters when provided";
        if(arguments[@"offset"] && !integer(arguments[@"offset"], 9007199254740991.0)) return @"offset must be a nonnegative integer";
        if(arguments[@"limit"] && (!integer(arguments[@"limit"], 100) || [arguments[@"limit"] integerValue] == 0)) return @"limit must be an integer between 1 and 100";
        return nil;
    }
    if(![method isEqual:@"read_document"]) return @"Unknown tool";
    for(NSString* key in arguments)
        if(![@[@"documentId", @"offset", @"length"] containsObject:key]) return @"Unknown argument";
    if(!identifier(arguments[@"documentId"])) return @"documentId is required and must be at most 256 characters";
    if(arguments[@"offset"] && !integer(arguments[@"offset"], 9007199254740991.0)) return @"offset must be a nonnegative integer";
    if(arguments[@"length"] && !integer(arguments[@"length"], 32768)) return @"length must be an integer between 0 and 32768";
    return nil;
}
}
