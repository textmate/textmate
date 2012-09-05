#include "launchd.h"
#include "install.h"
#include <authorization/connection.h>
#include <authorization/constants.h>
#include <authorization/authorization.h>
#include <io/io.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(AuthServer);

static double const AppVersion  = 1.1;
static size_t const AppRevision = APP_REVISION;

extern char* optarg;
extern int optind;

static bool running = true;

static void handle_signal (int theSignal)
{
	D(DBF_AuthServer, bug("%s\n", strsignal(theSignal)););
	running = false;
}

static void reap_children (int theSignal)
{
	D(DBF_AuthServer, bug("%s\n", strsignal(theSignal)););
	pid_t pid;
	int status;
	while((pid = waitpid(-1, &status, WNOHANG)) > 0)
	{
		D(DBF_AuthServer, bug("child %d terminated\n", pid););
	}
}

static void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

static void usage (FILE* io = stdout)
{
	fprintf(io,
		"%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n"
		"Usage: %1$s [-siuhv]\n"
		"Description:\n"
		" Server for authenticated file system operations.\n"
		"Options:\n"
		" -s, --server    Run server.\n"
		" -i, --install   Install server.\n"
		" -u, --uninstall Uninstall server.\n"
		" -h, --help      Show this information.\n"
		" -v, --version   Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
}

static int setup_socket ()
{
	unlink(kAuthSocketPath);

	int fd = socket(AF_UNIX, SOCK_STREAM, 0);
	struct sockaddr_un addr = { 0, AF_UNIX, kAuthSocketPath };
	addr.sun_len = SUN_LEN(&addr);
	int rc = bind(fd, (sockaddr*)&addr, sizeof(addr));
	chmod(kAuthSocketPath, S_IRWXU|S_IRWXG|S_IRWXO);
	assert(rc != -1);
	rc = listen(fd, 5);
	assert(rc != -1);

	return fd;
}

static void handle_connection (int fd)
{
	D(DBF_AuthServer, bug("child %d\n", getpid()););

	connection_t conn(fd);
	conn << "AuthServer" << kAuthServerMajor << kAuthServerMinor;

	std::string command;
	conn >> command;
	D(DBF_AuthServer, bug("> %s\n", command.c_str()););

	if(command == "auth")
	{
		std::string authString;
		conn >> authString;

		D(DBF_AuthServer, bug("> auth: %s\n", authString.c_str()););

		osx::authorization_t auth(authString);
		if(!auth.check_right(kAuthRightName))
		{
			D(DBF_AuthServer, bug("failed authentication\n"););
			return;
		}
	}
	else
	{
		D(DBF_AuthServer, bug("no authentication\n"););
		return;
	}

	std::string action;
	conn >> action;

	D(DBF_AuthServer, bug("> %s\n", action.c_str()););
	if(action == "read")
	{
		std::string path;
		conn >> path;

		conn << path::content(path) << path::attributes(path);
	}
	else if(action == "write")
	{
		std::string path, content, error = NULL_STR;
		std::map<std::string, std::string> attributes;
		conn >> path >> content >> attributes;

		if(!path::set_content(path, content))
			error = text::format("set_content() failed: %s", strerror(errno));
		else if(!path::set_attributes(path, attributes))
			error = text::format("set_attributes() failed: %s", strerror(errno));

		conn << error;
	}
	else
	{
		D(DBF_AuthServer, bug("unknown action: ‘%s’\n", action.c_str()););
	}
}

static void close_socket (int fd)
{
	close(fd);
	// unlink(kAuthSocketPath); // when running via launchd we do not want to remove the socket, since launchd will not re-create it if our server process is restarted
}

int main (int argc, char const* argv[])
{
	D(DBF_AuthServer, bug("\n"););

	signal(SIGINT,  &handle_signal);
	signal(SIGTERM, &handle_signal);
	signal(SIGCHLD, &reap_children);
	signal(SIGPIPE, SIG_IGN);

	static struct option const longopts[] = {
		{ "server",           no_argument,         0,      's'   },
		{ "install",          no_argument,         0,      'i'   },
		{ "uninstall",        no_argument,         0,      'u'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	bool server = false, install = false, uninstall = false;

	unsigned int ch;
	while((ch = getopt_long(argc, (char* const*)argv, "siuhv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 's': server = true;    break;
			case 'i': install = true;   break;
			case 'u': uninstall = true; break;
			case 'h': usage();          return 0;
			case 'v': version();        return 0;
			default:  usage(stderr);    return 1;
		}
	}

	if(geteuid() != 0)
	{
		fprintf(stderr, "auth_server: must run as root\n");
		abort();
	}

	if(install)
		return install_tool(argv[0]);
	else if(uninstall)
		return uninstall_tool();

	int fd = server ? setup_socket() : launchd_sockets();

	while(running)
	{
		fd_set readfds;
		FD_ZERO(&readfds);
		FD_SET(fd, &readfds);
		int rc = select(fd+1, &readfds, NULL, NULL, NULL);
		if(rc == -1)
		{
			D(DBF_AuthServer, bug("select: %s\n", strerror(errno)););
			continue;
		}

		if(FD_ISSET(fd, &readfds))
		{
			char dummy[256];
			socklen_t len = sizeof(dummy);
			int newFd = accept(fd, (sockaddr*)&dummy[0], &len);
			// if(fork() == 0)
			{
				D(DBF_AuthServer, bug("new connection\n"););
				handle_connection(newFd);
				// _exit(0);
			}
		}
	}

	close_socket(fd);
	return 0;
}
