#import "OakDebug.h"
#import <oak/oak.h>
#import <oak/compat.h>
#import <text/format.h>

#include <dlfcn.h>
#include <mach-o/dyld.h>

@interface OakExceptionHandlerDelegate : NSObject { }
@end

std::string OakStackDump (int linesToSkip)
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

	pid_t pid = oak::vfork();
	if(pid == 0)
	{
		close(STDOUT_FILENO); close(STDERR_FILENO);
		dup(output[1]); dup(output[1]);
		close(output[0]); close(output[1]);

		signal(SIGPIPE, SIG_DFL);

		int mib[2] = { CTL_USER, USER_CS_PATH };
		size_t len = 0;
		sysctl(mib, 2, NULL, &len, NULL, 0);
		std::vector<char> buf(len + 5);
		strcpy(buf.data(), "PATH=");
		sysctl(mib, 2, buf.data() + 5, &len, NULL, 0);

		char const* envp[] = { "LANG=en_US.UTF-8", "LC_CTYPE=en_US.UTF-8", buf.data(), NULL };
		execve(argv[0], (char* const*)argv, (char* const*)envp);
		_exit(EXIT_FAILURE);
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
	os_log_error(OS_LOG_DEFAULT, "%{public}s:%d: Expected (%{public}s %{public}s %{public}s), found (%{public}s %{public}s %{public}s)\n%{public}s\n", file, line, lhs, op, rhs, realLHS.c_str(), realOp, realRHS.c_str(), OakStackDump(2).c_str());
	_exit(EXIT_FAILURE);
}

static std::string OakMainExecutablePath ()
{
	static std::string const path = []{
		char buf[PATH_MAX];
		uint32_t size = sizeofA(buf);
		if(_NSGetExecutablePath(buf, &size) != 0)
			return std::string();

		char resolved[PATH_MAX];
		return realpath(buf, resolved) ? std::string(resolved) : std::string(buf);
	}();
	return path;
}

static std::string OakImagePathForAddress (void* address, std::string* symbolName)
{
	Dl_info info;
	if(!dladdr(address, &info))
		return std::string();

	if(symbolName)
		*symbolName = info.dli_sname ? info.dli_sname : "";

	if(!info.dli_fname)
		return std::string();

	char resolved[PATH_MAX];
	return realpath(info.dli_fname, resolved) ? std::string(resolved) : std::string(info.dli_fname);
}

static std::vector<void*> OakExceptionThrowStack (NSException* exception)
{
	std::vector<void*> addresses;
	for(NSValue* value in [exception callStackReturnAddresses])
		addresses.push_back([value pointerValue]);

	if(addresses.empty())
	{
		// Foundation generated an NSRangeException with no callStackSymbols here, so the throw stack
		// only exists as the hex dump NSExceptionHandler stores in userInfo.
		NSString* trace = [[exception userInfo] objectForKey:NSStackTraceKey];
		for(NSString* token in [trace componentsSeparatedByString:@" "])
		{
			if(char const* str = token.UTF8String)
				addresses.push_back(reinterpret_cast<void*>(static_cast<uintptr_t>(strtoull(str, NULL, 16))));
		}
	}

	if(addresses.empty())
	{
		// The delegate is called from inside objc_exception_throw, so the frames below that one
		// are the throw stack.
		void* frames[128];
		int count = backtrace(frames, sizeofA(frames));
		for(int i = 0; i < count; ++i)
		{
			std::string symbolName;
			OakImagePathForAddress(frames[i], &symbolName);
			if(symbolName == "objc_exception_throw")
			{
				addresses.assign(frames + i + 1, frames + count);
				break;
			}
		}
	}

	return addresses;
}

static bool OakShouldSwallowException (std::vector<void*> const& addresses)
{
	if(addresses.empty())
		return false;

	std::string const executable = OakMainExecutablePath();
	bool foundOwnFrame = false;
	for(void* address : addresses)
	{
		std::string symbolName;
		std::string const imagePath = OakImagePathForAddress(address, &symbolName);

		if(imagePath.find("Accessibility") != std::string::npos || symbolName.find("Accessibility") != std::string::npos || symbolName.find("_AX") != std::string::npos)
			return true;

		if(executable.empty() || imagePath != executable)
			continue;

		// main is the entry point every main-thread stack ends in, not a TextMate call site, so it
		// does not make the throw TextMate's responsibility.
		if(symbolName != "main" && symbolName != "_main")
			foundOwnFrame = true;
	}

	return !foundOwnFrame;
}

static void OakLogSwallowedException (NSException* exception, std::vector<void*> const& addresses)
{
	std::string trace;
	for(void* address : addresses)
	{
		std::string symbolName;
		std::string const imagePath = OakImagePathForAddress(address, &symbolName);
		trace += text::format("  %s :: %s\n", imagePath.empty() ? "?" : imagePath.c_str(), symbolName.empty() ? "?" : symbolName.c_str());
	}

	for(NSString* symbol in [exception callStackSymbols])
		trace += text::format("  %s\n", symbol.UTF8String);

	os_log_error(OS_LOG_DEFAULT, "SWALLOWED-APPKIT-INTERNAL %{public}@: %{public}@\n%{public}s", [exception name], [exception reason], trace.c_str());

	// The dev workflow runs the app with stderr redirected to a file, where os_log output is invisible.
	fprintf(stderr, "SWALLOWED-APPKIT-INTERNAL %s: %s\n%s", [exception name].UTF8String ?: "", [exception reason].UTF8String ?: "", trace.c_str());
	fflush(stderr);
}

@implementation OakExceptionHandlerDelegate
+ (void)load
{
	@autoreleasepool {
		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(enableAllExceptions:) name:NSApplicationDidFinishLaunchingNotification object:NSApp];
	}
}

+ (void)enableAllExceptions:(NSNotification*)aNotification
{
	static OakExceptionHandlerDelegate* exceptionDelegate = [self new];
	[NSExceptionHandler.defaultExceptionHandler setExceptionHandlingMask:NSLogAndHandleEveryExceptionMask];
	[NSExceptionHandler.defaultExceptionHandler setDelegate:exceptionDelegate];
}

- (BOOL)exceptionHandler:(NSExceptionHandler*)sender shouldLogException:(NSException*)exception mask:(NSUInteger)mask
{
	if([[exception name] isEqualToString:@"FSExecutionErrorException"])
		return NO;

	std::vector<void*> const addresses = OakExceptionThrowStack(exception);
	if(OakShouldSwallowException(addresses))
	{
		OakLogSwallowedException(exception, addresses);
		return NO;
	}

	os_log_error(OS_LOG_DEFAULT, "%{public}@: %{public}@\n", exception.name, exception.reason);
	abort();
	return YES;
}

- (BOOL)exceptionHandler:(NSExceptionHandler*)sender shouldHandleException:(NSException*)exception mask:(NSUInteger)mask
{
	// Answering NO keeps the handler from applying default handling to an exception we have already
	// logged ourselves; an uncaught exception is terminated by Foundation regardless of the answer.
	if([[exception name] isEqualToString:@"FSExecutionErrorException"])
		return NO;

	return !OakShouldSwallowException(OakExceptionThrowStack(exception));
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
		if(!info.empty() && info.back() != '\n')
			info += "\n";

		free(buf);
	}

	os_log_error(OS_LOG_DEFAULT, "------------------------------------------------------------\n"
	    "ASSERTION FAILURE: %{public}s\n"
	    "%{public}s"
	    "------------------------------------------------------------\n"
	    "%{public}s", name, info.c_str(), OakStackDump(2).c_str());
	_exit(EXIT_FAILURE);
}
