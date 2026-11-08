#import <Foundation/Foundation.h>
#import <EditorContextConnection.h>
#import "LauncherUtilities.h"

int main(int argc, char const* argv[]) {
    @autoreleasepool {
        signal(SIGPIPE, SIG_IGN);
        if(argc == 2 && !strcmp(argv[1], "--help")) {
            puts("Usage: tm-pi [pi arguments...]\n       tm-pi --check\n\nStarts pi with editor context and navigation from this TextMate terminal.\nThe TextMate MCP server is registered for this session only; pi's own configuration is not changed.\nSet TM_PI_COMMAND to an explicit pi executable if it is not on PATH.");
            return 0;
        }
        if(argc == 2 && !strcmp(argv[1], "--check")) return editor_context::check_connection();
        NSDictionary* connection = editor_context::query(@"get_connection_status", @{});
        if(connection[@"error"]) {
            fprintf(stderr, "tm-pi (%s): %s\n", [connection[@"code"] UTF8String], [connection[@"error"] UTF8String]);
            return 1;
        }
        NSString* pi = launcher::find(@"pi", @"TM_PI_COMMAND", @[
            [NSHomeDirectory() stringByAppendingPathComponent:@".local/bin/pi"],
            @"/opt/homebrew/bin/pi", @"/usr/local/bin/pi"]);
        if(!pi) {
            fprintf(stderr, "tm-pi: pi was not found. Install pi, add it to PATH, or set TM_PI_COMMAND to its executable. See https://pi.dev\n");
            return 1;
        }
        NSString* helper = launcher::helper();
        NSString* extension = launcher::resource(@"textmate-pi-extension.js");
        if(!helper || !extension) { fprintf(stderr, "tm-pi: The bundled textmate-mcp helper or pi extension is missing. Rebuild or reinstall TextMate.\n"); return 1; }
        // The extension reads the helper's path from here, so it matches the launcher that started it.
        if(setenv("TM_MCP_SERVER", helper.UTF8String, 1)) { fprintf(stderr, "tm-pi: Cannot set the helper path.\n"); return 1; }
        NSMutableArray<NSString*>* arguments = [NSMutableArray array];
        // These subcommands do not load extensions, and an option placed before one would
        // make pi read the subcommand as a prompt.
        NSArray* subcommands = @[@"install", @"remove", @"uninstall", @"update", @"list", @"config", @"auth", @"mcp"];
        if(argc < 2 || ![subcommands containsObject:@(argv[1])]) [arguments addObjectsFromArray:@[@"--extension", extension]];
        for(int i = 1; i < argc; ++i) [arguments addObject:@(argv[i])];
        return launcher::execute(pi, arguments, @"tm-pi");
    }
}
