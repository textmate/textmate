#import <OSAKit/OSAKit.h>

static double const AppVersion  = 1.0;
static size_t const AppRevision = APP_REVISION;

int main (int argc, char const* argv[])
{
	if(argc == 2 && (strcmp(argv[1], "--version") == 0 || strcmp(argv[1], "-v") == 0))
	{
		fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
		return 0;
	}

	NSAutoreleasePool* pool = [NSAutoreleasePool new];
	NSDictionary* errorInfo = nil;
	OSAScript* script       = [[OSAScript alloc] initWithCompiledData:[[NSFileHandle fileHandleWithStandardInput] readDataToEndOfFile] error:&errorInfo];
	std::string str         = [[script source] UTF8String];
	[pool drain];

	std::replace(str.begin(), str.end(), '\r', '\n');
	write(STDOUT_FILENO, str.data(), str.size());

	return 0;
}
