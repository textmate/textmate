#import <Foundation/Foundation.h>
#import <EditorContextProtocol.h>
#include <stdio.h>

// `--list` prints the compiled tool registry in advertised order. Otherwise it
// reports which of the given names validate_arguments handles, so the verdict
// comes from the compiled branches rather than from re-parsing header source.
int main(int argc, char const* argv[]) {
	@autoreleasepool {
		if(argc == 2 && !strcmp(argv[1], "--list")) {
			for(NSString* name in editor_context::tool_names())
				printf("%s\n", name.UTF8String);
			return 0;
		}
		for(int i = 1; i < argc; ++i) {
			NSString* name = [NSString stringWithUTF8String:argv[i]];
			BOOL accepted = ![editor_context::validate_arguments(name, @{}) isEqual:@"Unknown tool"];
			printf("%s\t%s\n", argv[i], accepted ? "accepted" : "unknown");
		}
	}
	return 0;
}
