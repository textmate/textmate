#include "application.h"
#include <io/io.h>
#include <cf/cf.h>
#include <text/format.h>
#include <oak/datatypes.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(Application);

namespace oak
{
	static std::string _app_name     = NULL_STR;
	static std::string _app_path     = NULL_STR;
	static std::string _support_path = NULL_STR;

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
