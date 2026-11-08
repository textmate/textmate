#import <Foundation/Foundation.h>

// Render a captured LSP report off the main thread, within the MCP size/deadline bounds.
NSDictionary* EditorContextDiagnostics(NSDictionary* report, NSDictionary* base, NSDictionary* document, NSDictionary* arguments, double deadline);
