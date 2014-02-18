#import "OakDebug.h"
#import <oak/oak.h>
#import <text/format.h>

@interface OakExceptionHandlerDelegate : NSObject { }
@end

PUBLIC std::string OakStackDump (int linesToSkip)
{
	void* callstack[256];
	int frames = backtrace(callstack, sizeofA(callstack));

	int n = 0;
	char trace[1024];
	for(int i = 0; i < frames && n < sizeof(trace); ++i)
		n += snprintf(trace + n, sizeof(trace) - n, "%p, ", callstack[i]);

	if(n > 2)
		trace[n - 2] = '\0';

	// ============
	// = Run atos =
	// ============

	std::string cmd = text::format("/usr/bin/xcrun atos -p %d %s | /usr/bin/tail -n +%d | /usr/bin/c++filt", getpid(), trace, linesToSkip + 1);
	char const* argv[] = { "/bin/sh", "-c", cmd.c_str(), NULL };

	int output[2];
	pipe(&output[0]);

	pid_t pid = vfork();
	if(pid == 0)
	{
		close(1); close(2);
		dup(output[1]); dup(output[1]);
		close(output[0]); close(output[1]);

		signal(SIGPIPE, SIG_DFL);

		int mib[2] = { CTL_USER, USER_CS_PATH };
		size_t len = 0;
		sysctl(mib, 2, NULL, &len, NULL, 0);
		char buf[len + 5];
		strcpy(buf, "PATH=");
		sysctl(mib, 2, buf + 5, &len, NULL, 0);

		char const* envp[] = { "LANG=en_US.UTF-8", "LC_CTYPE=en_US.UTF-8", buf, NULL };
		execve(argv[0], (char* const*)argv, (char* const*)envp);
		_exit(-1);
	}
	else if(pid != -1)
	{
		close(output[1]);

		int status = 0;
		if(waitpid(pid, &status, 0) == pid && WIFEXITED(status) && WEXITSTATUS(status) == 0)
		{
			ssize_t len;
			char buf[512];
			std::string res = "";
			while((len = read(output[0], buf, sizeof(buf))) > 0)
				res.insert(res.end(), buf, buf + len);

			close(output[0]);
			return res;
		}
	}
	return "error";
}

static std::string format (char const* format, ...) __attribute__ ((format (printf, 1, 2)));
static std::string format (char const* format, ...)
{
	char* tmp = NULL;

	va_list ap;
	va_start(ap, format);
	vasprintf(&tmp, format, ap);
	va_end(ap);

	std::string res(tmp);
	free(tmp);
	return res;
}

namespace oak
{
	std::string to_s (bool value)               { return value ? "YES" : "NO"; }
	std::string to_s (size_t value)             { return format("%zu", value); }
	std::string to_s (ssize_t value)            { return format("%zd", value); }
	std::string to_s (int value)                { return format("%d", value); }
	std::string to_s (double value)             { return format("%g", value); }
	std::string to_s (char const* value)        { return value == NULL     ? "«NULL»"     : format("\"%s\"", value); }
	std::string to_s (std::string const& value) { return value == NULL_STR ? "«NULL_STR»" : "\"" + value + "\""; }
}

void OakPrintBadAssertion (char const* lhs, char const* op, char const* rhs, std::string const& realLHS, char const* realOp, std::string const& realRHS, char const* file, int line)
{
	fprintf(stderr, "%s:%d: Expected (%s %s %s), found (%s %s %s)\n%s\n", file, line, lhs, op, rhs, realLHS.c_str(), realOp, realRHS.c_str(), OakStackDump(2).c_str());
	_exit(-1);
}

@implementation OakExceptionHandlerDelegate
+ (void)load
{
	@autoreleasepool {
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(enableAllExceptions:) name:NSApplicationDidFinishLaunchingNotification object:NSApp];
	}
}

+ (void)enableAllExceptions:(NSNotification*)aNotification
{
	static OakExceptionHandlerDelegate* exceptionDelegate = [self new];
	[[NSExceptionHandler defaultExceptionHandler] setExceptionHandlingMask:NSLogAndHandleEveryExceptionMask];
	[[NSExceptionHandler defaultExceptionHandler] setDelegate:exceptionDelegate];
}

- (BOOL)exceptionHandler:(NSExceptionHandler*)sender shouldLogException:(NSException*)exception mask:(NSUInteger)mask
{
	if([[exception name] isEqualToString:@"FSExecutionErrorException"])
		return NO;
	bug(" \n*** %s: %s\n", [[exception name] UTF8String], [[exception reason] UTF8String]);
	abort();
	return YES;
}
@end

void OakBadAssertion (char const* name, char const* format, ...)
{
	std::string info = "";
	if(format)
	{
		va_list ap;
		va_start(ap, format);
		char* buf = NULL;
		vasprintf(&buf, format, ap);
		va_end(ap);

		info = buf;
		if(!info.empty() && info[info.size()-1] != '\n')
			info += "\n";

		free(buf);
	}

	bug("------------------------------------------------------------\n"
	    "ASSERTION FAILURE: %s\n"
	    "%s"
	    "------------------------------------------------------------\n"
	    "%s", name, info.c_str(), OakStackDump(2).c_str());
	_exit(-1);
}
