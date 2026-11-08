#import <Foundation/Foundation.h>
#import <EditorContextConnection.h>
#import "LauncherUtilities.h"

int main(int argc, char const* argv[]) {
    @autoreleasepool {
        signal(SIGPIPE, SIG_IGN);
        if(argc == 2 && !strcmp(argv[1], "--help")) {
            puts("Usage: tm-claude [Claude Code session arguments...]\n       tm-claude --check\n\nStarts Claude Code with editor context and navigation from this TextMate terminal.\nConnection configuration is temporary. Arguments and the current directory are preserved.\nSet TM_CLAUDE_COMMAND to an explicit Claude executable if it is not on PATH.");
            return 0;
        }
        if(argc == 2 && !strcmp(argv[1], "--check")) return editor_context::check_connection();
        NSDictionary* connection = editor_context::query(@"get_connection_status", @{});
        if(connection[@"error"]) {
            fprintf(stderr, "tm-claude (%s): %s\n", [connection[@"code"] UTF8String], [connection[@"error"] UTF8String]);
            return 1;
        }
        NSString* claude = launcher::find(@"claude", @"TM_CLAUDE_COMMAND", @[
            [NSHomeDirectory() stringByAppendingPathComponent:@".local/bin/claude"],
            [NSHomeDirectory() stringByAppendingPathComponent:@".claude/local/claude"],
            @"/opt/homebrew/bin/claude", @"/usr/local/bin/claude"]);
        if(!claude) {
            fprintf(stderr, "tm-claude: Claude Code CLI was not found. Install Claude Code, add it to PATH, or set TM_CLAUDE_COMMAND to its executable. See https://code.claude.com/docs/en/setup\n");
            return 1;
        }
        NSString* helper = launcher::helper();
        if(!helper) { fprintf(stderr, "tm-claude: The bundled textmate-mcp helper is missing. Rebuild or reinstall TextMate.\n"); return 1; }
        // References are expanded by Claude, so credentials never appear in argv or a config file.
        NSDictionary* configuration = @{ @"mcpServers": @{ @"textmate": @{
            @"type": @"stdio", @"command": helper, @"args": @[], @"env": @{
                @"TM_CONTEXT_SOCKET": @"${TM_CONTEXT_SOCKET}",
                @"TM_CONTEXT_SESSION": @"${TM_CONTEXT_SESSION}",
                @"TM_CONTEXT_TOKEN": @"${TM_CONTEXT_TOKEN}" } } } };
        NSData* data = [NSJSONSerialization dataWithJSONObject:configuration options:NSJSONWritingWithoutEscapingSlashes error:nil];
        NSString* encoded = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSMutableArray<NSString*>* arguments = [NSMutableArray array];
        int delimiter = argc;
        for(int i = 1; i < argc; ++i) if(!strcmp(argv[i], "--")) { delimiter = i; break; }
        for(int i = 1; i < delimiter; ++i) [arguments addObject:@(argv[i])];
        // --mcp-config is variadic: place it last, before any explicit --, so
        // a positional prompt cannot be mistaken for another config file.
        [arguments addObjectsFromArray:@[@"--mcp-config", encoded]];
        for(int i = delimiter; i < argc; ++i) [arguments addObject:@(argv[i])];
        return launcher::execute(claude, arguments, @"tm-claude");
    }
}
