#import <Foundation/Foundation.h>
#import <mach-o/dyld.h>
#include <unistd.h>

int main (int argc, char const* argv[])
{
	@autoreleasepool
	{
		if(argc > 1)
		{
			fprintf(stdout, "Usage: tm-theme-editor\nOpens the native TextMate theme editor.\n");
			return strcmp(argv[1], "--help") == 0 ? 0 : 1;
		}
		uint32_t length = 0;
		_NSGetExecutablePath(nullptr, &length);
		std::vector<char> buffer(length);
		_NSGetExecutablePath(buffer.data(), &length);
		NSString* executable = [[NSString stringWithUTF8String:buffer.data()] stringByResolvingSymlinksInPath];
		NSString* contents = executable.stringByDeletingLastPathComponent.stringByDeletingLastPathComponent;
		NSString* editor = [contents stringByAppendingPathComponent:@"Helpers/ThemeEditor.app/Contents/MacOS/ThemeEditor"];
		execl(editor.fileSystemRepresentation, editor.fileSystemRepresentation, nullptr);
		fprintf(stderr, "tm-theme-editor: could not launch %s: %s\n", editor.fileSystemRepresentation, strerror(errno));
		return 1;
	}
}
