#import <Foundation/Foundation.h>
#import <EditorContextConnection.h>
#import "LauncherUtilities.h"

static NSString* toml_string(NSString* value) {
    NSData* data = [NSJSONSerialization dataWithJSONObject:@[value] options:NSJSONWritingWithoutEscapingSlashes error:nil];
    NSString* encoded = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    return [encoded substringWithRange:NSMakeRange(1, encoded.length-2)];
}
int main(int argc, char const* argv[]) {
    @autoreleasepool {
        signal(SIGPIPE, SIG_IGN);
        if(argc == 2 && !strcmp(argv[1], "--help")) {
            puts("Usage: tm-codex [Codex arguments...]\n       tm-codex --check\n\nStarts Codex with editor context and navigation from this TextMate terminal.\nConnection configuration is temporary. Codex arguments and the current directory are preserved.\nSet TM_CODEX_COMMAND to an explicit Codex executable if it is not on PATH.");
            return 0;
        }
        if(argc == 2 && !strcmp(argv[1], "--check")) return editor_context::check_connection();
        NSDictionary* connection = editor_context::query(@"get_connection_status", @{});
        if(connection[@"error"]) {
            fprintf(stderr, "tm-codex (%s): %s\n", [connection[@"code"] UTF8String], [connection[@"error"] UTF8String]);
            return 1;
        }
        NSString* codex = launcher::find(@"codex", @"TM_CODEX_COMMAND", @[
            @"/Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex",
            @"/opt/homebrew/bin/codex", @"/usr/local/bin/codex"]);
        if(!codex) {
            fprintf(stderr, "tm-codex: Codex CLI was not found. Install Codex CLI, add it to PATH, or set TM_CODEX_COMMAND to its executable. See https://learn.chatgpt.com/docs/cli\n");
            return 1;
        }
        NSString* helper = launcher::helper();
        if(!helper) { fprintf(stderr, "tm-codex: The bundled textmate-mcp helper is missing. Rebuild or reinstall TextMate.\n"); return 1; }
        NSString* configuration = [NSString stringWithFormat:@"mcp_servers.textmate={command=%@,args=[],env={},env_vars=[\"TM_CONTEXT_SOCKET\",\"TM_CONTEXT_SESSION\",\"TM_CONTEXT_TOKEN\"],enabled=true,required=true}", toml_string(helper)];
        // Global options must precede subcommands such as exec and resume.
        NSMutableArray<NSString*>* arguments = [NSMutableArray arrayWithObjects:@"--no-daemon", @"-c", configuration, nil];
        for(int i = 1; i < argc; ++i) [arguments addObject:@(argv[i])];
        return launcher::execute(codex, arguments, @"tm-codex");
    }
}
