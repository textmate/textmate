#include "install.h"
#include "CFxx.h"
#include <text/format.h>
#include <io/io.h>
#include <authorization/constants.h>

static char const* const kPlistFormatString =
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
	"<plist version=\"1.0\">\n"
	"	<dict>\n"
	"		<key>Disabled</key>\n"
	"		<false/>\n"
	"		<key>Label</key>\n"
	"		<string>%s</string>\n"
	"		<key>OnDemand</key>\n"
	"		<true/>\n"
	"		<key>ProgramArguments</key>\n"
	"		<array>\n"
	"			<string>%s</string>\n"
	"		</array>\n"
	"		<key>ServiceIPC</key>\n"
	"		<true/>\n"
	"		<key>Sockets</key>\n"
	"		<dict>\n"
	"			<key>MasterSocket</key>\n"
	"			<dict>\n"
	"				<key>SockFamily</key>\n"
	"				<string>Unix</string>\n"
	"				<key>SockPathName</key>\n"
	"				<string>%s</string>\n"
	"				<key>SockPathMode</key>\n"
	"				<integer>438</integer>\n"
	"			</dict>\n"
	"		</dict>\n"
	"	</dict>\n"
	"</plist>\n"; // job label, tool path, socket

static std::string plist_content ()
{
	return text::format(kPlistFormatString, kAuthJobName, kAuthToolPath, kAuthSocketPath);
}

static void launch_control (char const* command, std::string const& argument)
{
	pid_t pid = vfork();
	if(pid == 0)
	{
		execl("/bin/launchctl", "/bin/launchctl", command, argument.c_str(), nullptr);
		perror("execl(\"/bin/launchctl\")");
		_exit(EXIT_FAILURE);
	}

	int status = 0;
	waitpid(pid, &status, 0);
}

static void remove_policy ()
{
	AuthorizationRef authRef;
	if(noErr == AuthorizationCreate(nullptr, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, &authRef))
	{
		AuthorizationRightRemove(authRef, kAuthRightName);
		AuthorizationFree(authRef, kAuthorizationFlagDefaults);
	}
}

static void add_policy ()
{
	remove_policy();

	AuthorizationRef authRef;
	if(noErr == AuthorizationCreate(nullptr, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, &authRef))
	{
		cf::dictionary rightDefinition;
		rightDefinition["class"]      = cf::string("user");
		rightDefinition["group"]      = cf::string("admin");
		rightDefinition["allow-root"] = CFRetain(kCFBooleanTrue);
		rightDefinition["timeout"]    = cf::number(900);

		int errStatus = AuthorizationRightSet(authRef, kAuthRightName, rightDefinition, nullptr /* description key */, nullptr, nullptr);
		if(errStatus != noErr)
			fprintf(stderr, "*** error adding policy (‘%s’): %d\n", kAuthRightName, errStatus);

		AuthorizationFree(authRef, kAuthorizationFlagDefaults);
	}
}

int install_tool (std::string const& toolPath)
{
	if(!path::is_absolute(toolPath))
	{
		fprintf(stderr, "need to be run with absolute path\n");
		abort();
	}

	setuid(geteuid());
	setgid(getegid());

	if(!path::make_dir(path::parent(kAuthToolPath)))
		return EX_CANTCREAT;
	unlink(kAuthToolPath);
	if(copyfile(toolPath.c_str(), kAuthToolPath, nullptr, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) != 0)
		return EX_IOERR;
	chown(kAuthToolPath, 0, 0);
	if(access(kAuthPlistPath, F_OK) == 0)
		launch_control("unload", kAuthPlistPath);
	if(!path::set_content(kAuthPlistPath, plist_content()))
		return EX_IOERR;
	launch_control("load", kAuthPlistPath);

	add_policy();
	return EX_OK;
}

int uninstall_tool ()
{
	remove_policy();
	if(path::exists(kAuthPlistPath))
	{
		launch_control("unload", kAuthPlistPath);
		unlink(kAuthPlistPath);
	}
	unlink(kAuthToolPath);
	return EX_OK;
}
