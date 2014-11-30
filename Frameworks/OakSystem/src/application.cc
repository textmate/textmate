#include "application.h"
#include <io/io.h>
#include <cf/cf.h>
#include <text/format.h>
#include <oak/compat.h>
#include <oak/datatypes.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Application);

namespace oak
{
	static std::string _app_name     = NULL_STR;
	static std::string _app_path     = NULL_STR;
	static std::string _support_path = NULL_STR;

	static std::string process_name (pid_t pid)
	{
		struct kinfo_proc procInfo;
		size_t length = sizeof(procInfo);
		int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PID, pid };
		if(sysctl(mib, 4, &procInfo, &length, NULL, 0) == 0 && length > 0)
			return procInfo.kp_proc.p_comm;
		return NULL_STR;
	}

	application_t::application_t (int argc, char const* argv[], bool redirectStdErr)
	{
		_app_name = getenv("OAK_APP_NAME") ?: path::name(argv[0]);
		_app_path = path::join(path::cwd(), argv[0]);

		if(redirectStdErr)
		{
			char const* logPath = getenv("LOG_PATH");
			if(logPath && path::is_absolute(logPath) && path::make_dir(logPath))
			{
				std::string const logFile = path::join(logPath, _app_name + ".log");
				if(FILE* fp = freopen(logFile.c_str(), "w+", stderr))
					setlinebuf(fp);
			}
		}

		std::string const appBinary = path::join("Contents/MacOS", _app_name);
		if(_app_path.size() > appBinary.size() && _app_path.find(appBinary) == _app_path.size() - appBinary.size())
			_app_path.erase(_app_path.end() - appBinary.size(), _app_path.end());

		// ==================================================================================
		// = TODO Remove the rest of this function once most users are on beta 6.5 or later =
		// ==================================================================================

		std::string content = path::content(path::join(path::temp(), (((CFBundleGetMainBundle() && CFBundleGetIdentifier(CFBundleGetMainBundle())) ? cf::to_s(CFBundleGetIdentifier(CFBundleGetMainBundle())) : _app_name) + ".pid")));
		long pid = content != NULL_STR ? strtol(content.c_str(), NULL, 10) : 0;
		if(pid != 0 && process_name(pid) == _app_name)
		{
			int event_queue = kqueue();

			struct kevent changeList = { (uintptr_t)pid, EVFILT_PROC, EV_ADD | EV_ENABLE | EV_ONESHOT, NOTE_EXIT, 0, NULL };
			int res = kevent(event_queue, &changeList, 1, NULL /* event list */, 0 /* number of events */, NULL);
			if(res == -1 && errno == ESRCH)
			{
				D(DBF_Application, bug("old instance no longer running\n"););
			}
			else if(res == 0)
			{
				if(char const* relaunch = getenv("OAK_RELAUNCH"))
				{
					D(DBF_Application, bug("%s is already running (pid %ld), OAK_RELAUNCH = %s\n", _app_name.c_str(), pid, relaunch););
					kill(pid, strcmp(relaunch, "QUICK") == 0 ? SIGTERM : SIGINT);

					struct kevent changed;
					struct timespec timeout = { 30, 0 };
					if(kevent(event_queue, NULL /* change list */, 0 /* number of changes */, &changed /* event list */, 1 /* number of events */, &timeout) == 1)
					{
						D(DBF_Application, bug("other instance terminated\n"););
						ASSERTF((pid_t)changed.ident == pid, "pid %d, changed %d", pid, (pid_t)changed.ident);
						CFPreferencesAppSynchronize(kCFPreferencesCurrentApplication);
					}
					else
					{
						D(DBF_Application, bug("timeout expired, terminate\n"););
						exit(EXIT_FAILURE);
					}
				}
				else
				{
					D(DBF_Application, bug("%s is already running (pid %ld), OAK_RELAUNCH = NO\n", name().c_str(), pid););
					ProcessSerialNumber psn;
					if(noErr == GetProcessForPID(pid, &psn))
					{
						SetFrontProcess(&psn);
						exit(EXIT_SUCCESS);
					}
					// procNotFound
				}
			}
			close(event_queue);
		}
	}

	std::string application_t::name ()
	{
		if(_app_name == NULL_STR)
		{
			if(CFBundleRef mainBundle = CFBundleGetMainBundle())
			{
				if(CFTypeRef appName = CFBundleGetValueForInfoDictionaryKey(mainBundle, CFSTR("CFBundleName")))
				{
					if(CFGetTypeID(appName) == CFStringGetTypeID())
						_app_name = cf::to_s((CFStringRef)appName);
				}
			}
		}
		return _app_name;
	}

	std::string application_t::path (std::string const& relativePath)
	{
		if(_app_path == NULL_STR)
		{
			if(CFBundleRef mainBundle = CFBundleGetMainBundle())
			{
				if(CFURLRef bundleURL = CFBundleCopyBundleURL(mainBundle))
				{
					if(CFStringRef bundlePath = CFURLCopyFileSystemPath(bundleURL, kCFURLPOSIXPathStyle))
					{
						_app_path = cf::to_s(bundlePath);
						CFRelease(bundlePath);
					}
					CFRelease(bundleURL);
				}
			}
		}
		return path::join(_app_path, relativePath);
	}

	void application_t::set_name (std::string const& newName)
	{
		_app_name = newName;
	}

	void application_t::set_path (std::string const& newPath)
	{
		_app_path = newPath;
	}

	void application_t::set_support (std::string const& newPath)
	{
		_support_path = newPath;
	}

	std::string application_t::support (std::string const& relativePath)
	{
		if(_support_path == NULL_STR)
			_support_path = path::join({ path::home(), "Library/Application Support", name() });
		return path::join(_support_path, relativePath);
	}

	std::string application_t::version ()
	{
		if(CFBundleRef mainBundle = CFBundleGetMainBundle())
		{
			if(CFTypeRef bundleVersion = CFBundleGetValueForInfoDictionaryKey(mainBundle, CFSTR("CFBundleShortVersionString")) ?: CFBundleGetValueForInfoDictionaryKey(mainBundle, CFSTR("CFBundleVersion")))
			{
				if(CFGetTypeID(bundleVersion) == CFStringGetTypeID())
					return cf::to_s((CFStringRef)bundleVersion);
			}
		}
		return "???";
	}

	void application_t::relaunch ()
	{
		ASSERT(_app_path != NULL_STR);
		D(DBF_Application, bug("%s\n", _app_path.c_str()););
		std::string script = text::format("{ kill %1$d; while ps -xp %1$d; do if (( ++n == 300 )); then exit; fi; sleep .2; done; open '%2$s' --args -disableSessionRestore NO; } &>/dev/null &", getpid(), _app_path.c_str());
		io::exec("/bin/sh", "-c", script.c_str(), nullptr);
	}

} /* oak */
