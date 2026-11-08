#import <Foundation/Foundation.h>
#import <EditorContextConnection.h>
#import "LauncherUtilities.h"

// OpenCode accepts JSONC. Preserve an existing inline configuration, including
// comments and trailing commas, without changing string values or placeholders.
static NSDictionary* inline_configuration(NSString* value) {
    NSMutableData* data = [[value dataUsingEncoding:NSUTF8StringEncoding] mutableCopy];
    auto bytes = static_cast<unsigned char*>(data.mutableBytes);
    bool string = false, escape = false;
    for(NSUInteger i = 0; i < data.length; ++i) {
        unsigned char ch = bytes[i];
        if(string) {
            if(escape) escape = false;
            else if(ch == '\\') escape = true;
            else if(ch == '"') string = false;
        }
        else if(ch == '"') string = true;
        else if(ch == '/' && i+1 < data.length && bytes[i+1] == '/') {
            while(i < data.length && bytes[i] != '\n' && bytes[i] != '\r') bytes[i++] = ' ';
        }
        else if(ch == '/' && i+1 < data.length && bytes[i+1] == '*') {
            bytes[i++] = ' ';
            bytes[i++] = ' ';
            while(i+1 < data.length && !(bytes[i] == '*' && bytes[i+1] == '/')) {
                if(bytes[i] != '\n' && bytes[i] != '\r') bytes[i] = ' ';
                ++i;
            }
            if(i+1 >= data.length) return nil;
            bytes[i++] = ' '; bytes[i] = ' ';
        }
    }
    string = escape = false;
    for(NSUInteger i = 0; i < data.length; ++i) {
        unsigned char ch = bytes[i];
        if(string) {
            if(escape) escape = false;
            else if(ch == '\\') escape = true;
            else if(ch == '"') string = false;
        }
        else if(ch == '"') string = true;
        else if(ch == ',') {
            NSUInteger next = i+1;
            while(next < data.length && strchr(" \t\r\n", bytes[next])) ++next;
            if(next < data.length && (bytes[next] == '}' || bytes[next] == ']')) bytes[i] = ' ';
        }
    }
    return editor_context::parse(data);
}

int main(int argc, char const* argv[]) {
    @autoreleasepool {
        signal(SIGPIPE, SIG_IGN);
        if(argc == 2 && !strcmp(argv[1], "--help")) {
            puts("Usage: tm-opencode [OpenCode arguments...]\n       tm-opencode --check\n\nStarts OpenCode with editor context and navigation from this TextMate terminal.\nConfiguration is temporary. Arguments, current directory, and other settings are preserved.\nSet TM_OPENCODE_COMMAND to an explicit OpenCode executable if it is not on PATH.");
            return 0;
        }
        if(argc == 2 && !strcmp(argv[1], "--check")) return editor_context::check_connection();
        NSDictionary* connection = editor_context::query(@"get_connection_status", @{});
        if(connection[@"error"]) {
            fprintf(stderr, "tm-opencode (%s): %s\n", [connection[@"code"] UTF8String], [connection[@"error"] UTF8String]);
            return 1;
        }
        NSString* opencode = launcher::find(@"opencode", @"TM_OPENCODE_COMMAND", @[
            [NSHomeDirectory() stringByAppendingPathComponent:@".opencode/bin/opencode"],
            [NSHomeDirectory() stringByAppendingPathComponent:@".local/bin/opencode"],
            @"/opt/homebrew/bin/opencode", @"/usr/local/bin/opencode"]);
        if(!opencode) {
            fprintf(stderr, "tm-opencode: OpenCode CLI was not found. Install OpenCode, add it to PATH, or set TM_OPENCODE_COMMAND to its executable. See https://opencode.ai/docs/\n");
            return 1;
        }
        NSString* helper = launcher::helper();
        if(!helper) { fprintf(stderr, "tm-opencode: The bundled textmate-mcp helper is missing. Rebuild or reinstall TextMate.\n"); return 1; }
        NSString* existing = NSProcessInfo.processInfo.environment[@"OPENCODE_CONFIG_CONTENT"];
        NSDictionary* original = existing.length ? inline_configuration(existing) : @{};
        if(!original || (original[@"mcp"] && ![original[@"mcp"] isKindOfClass:NSDictionary.class])) {
            fprintf(stderr, "tm-opencode: OPENCODE_CONFIG_CONTENT must contain a JSON/JSONC object, with an object-valued mcp entry if present. Existing configuration was not replaced.\n");
            return 1;
        }
        NSMutableDictionary* configuration = [original mutableCopy];
        NSMutableDictionary* servers = [original[@"mcp"] mutableCopy] ?: [NSMutableDictionary dictionary];
        servers[@"textmate"] = @{ @"type": @"local", @"command": @[helper], @"enabled": @YES,
            @"environment": @{ @"TM_CONTEXT_SOCKET": @"{env:TM_CONTEXT_SOCKET}",
                               @"TM_CONTEXT_SESSION": @"{env:TM_CONTEXT_SESSION}",
                               @"TM_CONTEXT_TOKEN": @"{env:TM_CONTEXT_TOKEN}" } };
        configuration[@"mcp"] = servers;
        NSData* data = [NSJSONSerialization dataWithJSONObject:configuration options:NSJSONWritingWithoutEscapingSlashes error:nil];
        NSString* encoded = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        if(!encoded || setenv("OPENCODE_CONFIG_CONTENT", encoded.UTF8String, 1)) {
            fprintf(stderr, "tm-opencode: Cannot set the temporary OpenCode configuration.\n"); return 1;
        }
        NSMutableArray<NSString*>* arguments = [NSMutableArray array];
        for(int i = 1; i < argc; ++i) [arguments addObject:@(argv[i])];
        return launcher::execute(opencode, arguments, @"tm-opencode");
    }
}
