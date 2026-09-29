#include "server.h"
#include "constants.h"
#include <io/src/path.h>
#include <io/src/exec.h>
#include <regexp/src/regexp.h>
#include <OakSystem/src/application.h>
#include <oak/debug.h>
#include <oak/compat.h>

static std::string auth_tool_source_path ()
{
	return oak::application_t::path("Contents/Resources/PrivilegedTool");
}

static bool install_auth_tool (osx::authorization_t const& auth)
{
	bool res = false;

	std::string const toolPath = auth_tool_source_path();
	ASSERT(path::exists(toolPath));

	if(auth.obtain_right("system.privilege.admin"))
	{
		char const* arguments[] = { "--install", nullptr };
		FILE* fp = nullptr;
		if(oak::execute_with_privileges(auth, toolPath, kAuthorizationFlagDefaults, (char**)arguments, &fp) == errAuthorizationSuccess)
		{
			int status;
			int pid = wait(&status);
			if(pid != -1 && WIFEXITED(status) && WEXITSTATUS(status) == 0)
					res = true;
			else	errno = WEXITSTATUS(status);

			char buf[1024];
			while(char* str = fgets(&buf[0], sizeof(buf), fp))
				fprintf(stderr, "%s\n", str);
		}
	}
	return res;
}

static double version_of_tool (std::string const& toolPath)
{
	std::string res = io::exec(toolPath, "--version", nullptr);
	if(regexp::match_t const& m = regexp::search("\\A[^\\s]+ ([\\d.]+)", res))
		return std::stod(m[1]);
	return 0;
}

static bool auth_server_too_old ()
{
	bool hasNewToolToInstall = path::exists(auth_tool_source_path());
	if(hasNewToolToInstall && path::exists(kAuthToolPath))
	{
		double oldVersion = version_of_tool(kAuthToolPath);
		double newVersion = version_of_tool(auth_tool_source_path());
		return oldVersion < newVersion;
	}
	return hasNewToolToInstall;
}

connection_t connect_to_auth_server (osx::authorization_t const& auth, bool retry)
{
	if(!path::exists(kAuthToolPath) || !path::exists(kAuthPlistPath) || auth_server_too_old())
	{
		if(!install_auth_tool(auth))
			return connection_t();
	}

	if(auth.obtain_right(kAuthRightName))
	{
		int fd = socket(AF_UNIX, SOCK_STREAM, 0);
		if(fd != -1)
		{
			struct sockaddr_un addr = { 0, AF_UNIX, kAuthSocketPath };
			addr.sun_len = SUN_LEN(&addr);

			if(connect(fd, (sockaddr*)&addr, sizeof(addr)) != -1)
			{
				connection_t res(fd);
				std::string server;
				int major, minor;
				res >> server >> major >> minor;
				if(major != kAuthServerMajor)
				{
					res << "quit" << "legacy" << "legacy" << "legacy";

					if(retry || !install_auth_tool(auth))
						return connection_t();

					return connect_to_auth_server(auth, true);
				}

				res << "auth" << (std::string)auth;
				return res;
			}
			else
			{
				perror("auth_server: connect");
			}

			close(fd);
		}
		else
		{
			perror("auth_server: socket");
		}
	}
	return connection_t();
}
