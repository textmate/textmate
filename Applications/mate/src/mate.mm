#include <authorization/constants.h>
#include <authorization/authorization.h>
#include <oak/oak.h>
#include <text/format.h>
#include <text/parse.h>
#include <io/path.h>
#include <plist/uuid.h>

static char const* const AppVersion = "2.13.3";

static char const* socket_path ()
{
	int uid = getuid();
	if(getenv("SUDO_UID"))
		uid = atoi(getenv("SUDO_UID"));

	static std::string const str = text::format("/tmp/textmate-%d.sock", uid);
	return str.c_str();
}

// mate returns when all files specified has been opened.
// If - is used for filename, stdin is read and opened as a new buffer.
// If -w is given, mate waits for all files to be closed, before it returns.
// If -w is used with -, the buffer is written to stdout
// If -w is used without any file names, - is implied

struct disable_sudo_helper_t
{
	disable_sudo_helper_t () : _uid(geteuid()), _gid(getegid())
	{
		if(_uid != 0)
			return;

		if(getenv("SUDO_UID") && getenv("SUDO_GID"))
		{
			setegid(atoi(getenv("SUDO_GID")));
			seteuid(atoi(getenv("SUDO_UID")));
		}
	}

	~disable_sudo_helper_t ()
	{
		if(_uid != 0)
			return;

		seteuid(_uid);
		setegid(_gid);
	}

private:
	uid_t _uid;
	gid_t _gid;
};

static NSURL* find_app ()
{
	disable_sudo_helper_t helper;

	if(NSURL* url = [NSWorkspace.sharedWorkspace URLForApplicationWithBundleIdentifier:@"com.macromates.TextMate"])
		return url;

	fprintf(stderr, "Can’t find TextMate.app\n");
	exit(EX_UNAVAILABLE);
}

static void launch_app (bool disableUntitled)
{
	disable_sudo_helper_t helper;

	NSError* error;
	if(![NSWorkspace.sharedWorkspace launchApplicationAtURL:find_app() options:NSWorkspaceLaunchWithoutActivation|NSWorkspaceLaunchWithoutAddingToRecents configuration:(disableUntitled ? @{ NSWorkspaceLaunchConfigurationArguments: @[ @"-disableNewDocumentAtStartup", @"1" ] } : nil) error:&error])
	{
		fprintf(stderr, "Can’t launch TextMate.app: %s\n", error.localizedDescription.UTF8String);
		exit(EX_UNAVAILABLE);
	}
}

static void install_auth_tool ()
{
	if(geteuid() == 0 && (!path::exists(kAuthToolPath) || !path::exists(kAuthPlistPath) || AuthorizationRightGet(kAuthRightName, nullptr) == errAuthorizationDenied))
	{
		NSURL* toolURL = [find_app() URLByAppendingPathComponent:@"Contents/Resources/PrivilegedTool"];

		char const* arg0 = toolURL.fileSystemRepresentation;
		if(access(arg0, X_OK) != 0)
		{
			fprintf(stderr, "No such executable file: ‘%s’\n", arg0);
			exit(EX_UNAVAILABLE);
		}

		pid_t pid = vfork();
		if(pid == 0)
		{
			execl(arg0, arg0, "--install", nullptr);
			_exit(errno);
		}

		if(pid != -1)
		{
			int status = 0;
			if(waitpid(pid, &status, 0) == pid && WIFEXITED(status) && WEXITSTATUS(status) != 0)
				fprintf(stderr, "%s: %s\n", arg0, strerror(WEXITSTATUS(status)));
		}
	}
}

static void usage (FILE* io)
{
	std::string pad(8 - std::min(strlen(getprogname()), size_t(8)), ' ');

	fprintf(io,
		"%1$s %2$s (" __DATE__ ")\n"
		"Usage: %1$s [-wl<selection>t<filetype>m<name>rehv] [-u<identifier> | file ...]\n"
		"       %1$s [-c<mark>] -s<mark>:<value> -l<line> [-u<identifier> | file ...]\n"
		"       %1$s -c<mark> [-l<line>] [-u<identifier> | file ...]\n"
		"\n"
		"Options:\n"
		" -w, --[no-]wait                 Wait for file to be closed by TextMate.\n"
		" -l, --line <selection>          Setup <selection> after loading file.\n"
		" -t, --type <filetype>           Treat file as having <filetype>.\n"
		" -m, --name <name>               The display name shown in TextMate.\n"
		" -r, --[no-]recent               Add file to Open Recent menu.\n"
		" -u, --uuid <identifier>         Reference already open document with\n"
		"                                 <identifier>.\n"
		" -e, --[no-]escapes              Set this to preserve ANSI escapes from stdin.\n"
		" -s, --set-mark <mark>[:<value>] Set a mark with optional <value> (requires --line).\n"
		" -c, --clear-mark <mark>         Clear a mark (clears all marks without --line).\n"
		" -h, --help                      Show this information.\n"
		" -v, --version                   Print version information.\n"
		"\n"
		"Files opened via %1$s are added to the recent menu unless\n"
		"the file starts with a period, --wait or --no-recent is\n"
		"specified, or the file is in the system’s temporary directory.\n"
		"\n"
		"By default %1$s will wait for files to be closed if the command name\n"
		"has a \"_wait\" suffix (e.g. via a symbolic link) or when used as a\n"
		"filter as in these examples:\n"
		"\n"
		"    ls *.tex|%1$s|sh%3$s-w implied\n"
		"    %1$s -|cat -n   %3$s-w implied (read from stdin)\n"
		"\n"
		"The -l/--line option requires a selection in the following format:\n"
		"\n"
		"    selection    = <range> ('&' <range>)*\n"
		"    range        = <pos> | <normal_range> | <column_range>\n"
		"    pos          = <line> (':' <column>)? ('+' <offset>)?\n"
		"    normal_range = <pos> '-' <pos>\n"
		"    column_range = <pos> 'x' <pos>\n"
		"    line         = [1-9][0-9]*\n"
		"    column       = [1-9][0-9]*\n"
		"    offset       = [1-9][0-9]*\n"
		"\n", getprogname(), AppVersion, pad.c_str()
	);
}

static void version ()
{
	fprintf(stdout, "%1$s %2$s (" __DATE__ ")\n", getprogname(), AppVersion);
}

static void append (std::string const& str, std::vector<std::string>& v)
{
	std::string::size_type from = 0;
	while(from < str.size() && from != std::string::npos)
	{
		std::string::size_type to = str.find(',', from);
		v.push_back(std::string(str.begin() + from, to == std::string::npos ? str.end() : str.begin() + to));
		from = to == std::string::npos ? to : to + 1;
	}
}

static std::string escape_value (std::string const& value)
{
	if(!value.find('\\') && !value.find('\n'))
		return value;

	std::string escaped;
	for(auto const& ch : value)
	{
		if(ch == '\\')
			escaped += "\\\\";
		else if(ch == '\n')
			escaped += "\\n";
		else
			escaped += ch;
	}
	return escaped;
}

static void write_key_pair (int fd, std::string const& key, std::string const& value)
{
	std::string const str = key + ": " + escape_value(value) + "\r\n";
	write(fd, str.data(), str.size());
}

static bool is_temporary_file (std::string path)
{
	path = path::resolve(path);
	return path::is_child(path, path::resolve("/tmp")) || path::is_child(path, path::resolve(path::temp()));
}

static std::string const kUUIDPrefix = "uuid://";

namespace
{
	enum class boolean {
		kUnset, kEnable, kDisable
	};

	std::string to_s (boolean flag)
	{
		return flag == boolean::kEnable ? "yes" : "no";
	}

	enum class escape_state_t {
		kPlain, kEscape, kANSI
	};
}

template <typename _InputIter>
_InputIter remove_ansi_escapes (_InputIter it, _InputIter last, escape_state_t* state)
{
	auto dst = it;
	for(; it != last; ++it)
	{
		auto const& ch = *it;
		switch(*state)
		{
			case escape_state_t::kPlain:
			{
				if(ch == '\e')
					*state = escape_state_t::kEscape;
				else if(it == dst)
					++dst;
				else
					*dst++ = *it;
			}
			break;

			case escape_state_t::kEscape:
			{
				if(ch == '[')
						*state = escape_state_t::kANSI;
				else	*state = escape_state_t::kPlain;
			}
			break;

			case escape_state_t::kANSI:
			{
				if(0x40 <= ch && ch <= 0x7E)
					*state = escape_state_t::kPlain;
			}
			break;
		}
	}
	return dst;
}

int main (int argc, char const* argv[])
{
	extern char* optarg;
	extern int optind;
	extern int optreset;

	static struct option const longopts[] = {
		{ "async",            no_argument,         0,      'a'   },
		{ "clear-mark",       required_argument,   0,      'c'   },
		{ "change-dir",       no_argument,         0,      'd'   },
		{ "escapes",          no_argument,         0,      'e'   },
		{ "no-escapes",       no_argument,         0,      'E'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "line",             required_argument,   0,      'l'   },
		{ "name",             required_argument,   0,      'm'   },
		{ "project",          required_argument,   0,      'p'   },
		{ "recent",           no_argument,         0,      'r'   },
		{ "no-recent",        no_argument,         0,      'R'   },
		{ "set-mark",         required_argument,   0,      's'   },
		{ "type",             required_argument,   0,      't'   },
		{ "uuid",             required_argument,   0,      'u'   },
		{ "version",          no_argument,         0,      'v'   },
		{ "wait",             no_argument,         0,      'w'   },
		{ "no-wait",          no_argument,         0,      'W'   },
		{ 0,                  0,                   0,      0     }
	};

	osx::authorization_t auth;
	std::vector<std::string> files, lines, types, names, projects, setMarks, clearMarks;
	oak::uuid_t uuid;

	boolean changeDir   = boolean::kDisable;
	boolean shouldWait  = boolean::kUnset;
	boolean addToRecent = boolean::kUnset;
	boolean keepEscapes = boolean::kUnset;

	if(strlen(getprogname()) > 5 && strcmp(getprogname() + strlen(getprogname()) - 5, "_wait") == 0)
		shouldWait = boolean::kEnable;

	install_auth_tool();

	std::vector<std::string> options = text::split(getenv("MATEFLAGS") ?: "", " ");
	char const** flags = new char const*[options.size()+1];
	for(size_t i = 0; i < options.size(); ++i)
		flags[i+1] = options[i].c_str();

	struct { int argc; char const** argv; } args[] =
	{
		{ (int)options.size()+1, flags }, { argc, argv }
	};

	for(auto const& arg : args)
	{
		optind = 1;

		int ch;
		while((ch = getopt_long(arg.argc, (char**)arg.argv, "ac:dehl:m:p:rs:t:u:vw", longopts, nullptr)) != -1)
		{
			switch(ch)
			{
				case 'a': shouldWait = boolean::kDisable;  break;
				case 'c': clearMarks.push_back(optarg);    break;
				case 'd': changeDir = boolean::kEnable;    break;
				case 'e': keepEscapes = boolean::kEnable;  break;
				case 'E': keepEscapes = boolean::kDisable; break;
				case 'h': usage(stdout);            return EX_OK;
				case 'l': append(optarg, lines);    break;
				case 'm': append(optarg, names);    break;
				case 'p': append(optarg, projects); break;
				case 'r': addToRecent = boolean::kEnable;  break;
				case 'R': addToRecent = boolean::kDisable; break;
				case 's': setMarks.push_back(optarg); break;
				case 't': append(optarg, types);    break;
				case 'u': uuid = optarg;            break;
				case 'v': version();                return EX_OK;
				case 'w': shouldWait = boolean::kEnable;  break;
				case 'W': shouldWait = boolean::kDisable; break;
				case '?': /* unknown option */      return EX_USAGE;
				case ':': /* missing option */      return EX_USAGE;
				default:  usage(stderr);            return EX_USAGE;
			}
		}

		optreset = 1;
	}

	argc -= optind;
	argv += optind;

	for(int i = 0; i < argc; ++i)
	{
		char const* path = argv[i];
		if(path[0] == 0)
			continue;

		if(strcmp(path, "-") != 0 && !path::is_absolute(path)) // relative path, make absolute
		{
			if(char* cwd = getcwd(nullptr, (size_t)-1))
			{
				char* tmp;
				asprintf(&tmp, "%s/%s", cwd, path);
				path = tmp;
				free(cwd);
			}
			else
			{
				fprintf(stderr, "failed to get current working directory\n");
				exit(EX_OSERR);
			}
		}

		files.push_back(path);
	}

	std::string defaultProject = projects.empty() ? (getenv("TM_PROJECT_UUID") ?: "") : projects.back();

	bool stdinIsAPipe = isatty(STDIN_FILENO) == 0;
	if(files.empty() && !uuid && (!setMarks.empty() || (!clearMarks.empty() && !lines.empty())) && getenv("TM_DOCUMENT_UUID"))
		uuid = getenv("TM_DOCUMENT_UUID");

	if(files.empty() && (uuid || (setMarks.empty() && clearMarks.empty())))
	{
		if(uuid)
			files.push_back(kUUIDPrefix + to_s(uuid));
		else if(shouldWait == boolean::kEnable || stdinIsAPipe)
			files.push_back("-");
	}

	int fd = socket(AF_UNIX, SOCK_STREAM, 0);
	struct sockaddr_un addr = { 0, AF_UNIX };
	strcpy(addr.sun_path, socket_path());
	addr.sun_len = SUN_LEN(&addr);

	int rc;
	for(size_t i = 0; i < 10; ++i)
	{
		rc = connect(fd, (sockaddr*)&addr, sizeof(addr));
		if(rc == 0)
			break;
		if(i == 0)
			launch_app(!files.empty());
		usleep(500000);
	}

	if(rc == -1)
	{
		perror("unable to bind to socket");
		exit(EX_IOERR);
	}

	char buf[1024];
	ssize_t len = read(fd, buf, sizeof(buf));
	if(len == -1)
		exit(EX_IOERR);

	if(!clearMarks.empty())
	{
		size_t n = setMarks.empty() ? std::max(clearMarks.size(), std::max(files.size(), lines.size())) : clearMarks.size();
		for(size_t i = 0; i < n; ++i)
		{
			write(fd, "clear-mark\r\n", 12);

			write_key_pair(fd, "mark", clearMarks[i % clearMarks.size()]);

			if(!lines.empty() && setMarks.empty())
				write_key_pair(fd, "line", lines[i % lines.size()]);

			if(!files.empty())
			{
				if(files[i % files.size()].compare(0, kUUIDPrefix.size(), kUUIDPrefix) == 0)
						write_key_pair(fd, "uuid", files[i % files.size()].substr(kUUIDPrefix.size()));
				else	write_key_pair(fd, "path", files[i % files.size()]);
			}

			write(fd, "\r\n", 2);
		}
	}

	if(!setMarks.empty() && !files.empty() && !lines.empty())
	{
		size_t n = std::max(setMarks.size(), std::max(files.size(), lines.size()));
		for(size_t i = 0; i < n; ++i)
		{
			write(fd, "set-mark\r\n", 10);

			write_key_pair(fd, "mark", setMarks[i % setMarks.size()]);
			write_key_pair(fd, "line", lines[i % lines.size()]);

			if(files[i % files.size()].compare(0, kUUIDPrefix.size(), kUUIDPrefix) == 0)
					write_key_pair(fd, "uuid", files[i % files.size()].substr(kUUIDPrefix.size()));
			else	write_key_pair(fd, "path", files[i % files.size()]);

			write(fd, "\r\n", 2);
		}
	}

	if(clearMarks.empty() && setMarks.empty())
	{
		for(size_t i = 0; i < files.size(); ++i)
		{
			write(fd, "open\r\n", 6);

			if(files[i] == "-")
			{
				if(!stdinIsAPipe)
					fprintf(stderr, "Reading from stdin, press ^D to stop\n");

				ssize_t total = 0;
				bool didStripEscapes = false;
				escape_state_t state = escape_state_t::kPlain;
				while(ssize_t len = read(STDIN_FILENO, buf, sizeof(buf)))
				{
					if(len == -1)
						break;

					if(keepEscapes != boolean::kEnable)
					{
						size_t oldLen = std::exchange(len, remove_ansi_escapes(buf, buf + len, &state) - buf);
						didStripEscapes = didStripEscapes || oldLen != len;
					}

					write_key_pair(fd, "data", std::to_string(len));
					total += len;
					write(fd, buf, len);
				}

				if(didStripEscapes && keepEscapes == boolean::kUnset)
					fprintf(stderr, "WARNING: Removed ANSI escape codes. Use -e/--[no-]escapes.\n");

				if(stdinIsAPipe && total == 0 && shouldWait != boolean::kEnable && getenv("TM_DOCUMENT_UUID"))
				{
					write_key_pair(fd, "uuid", getenv("TM_DOCUMENT_UUID"));
				}
				else
				{
					bool stdoutIsAPipe = isatty(STDOUT_FILENO) == 0;
					bool wait = shouldWait == boolean::kEnable || (shouldWait == boolean::kUnset && stdoutIsAPipe);
					write_key_pair(fd, "display-name",        i < names.size()      ? names[i] : "untitled (stdin)");
					write_key_pair(fd, "data-on-close",       wait && stdoutIsAPipe ? "yes" : "no");
					write_key_pair(fd, "wait",                wait                  ? "yes" : "no");
					write_key_pair(fd, "re-activate",         wait                  ? "yes" : "no");
				}
			}
			else if(files[i].compare(0, kUUIDPrefix.size(), kUUIDPrefix) == 0)
			{
				write_key_pair(fd, "uuid", files[i].substr(kUUIDPrefix.size()));
			}
			else
			{
				write_key_pair(fd, "path",             files[i]);
				write_key_pair(fd, "display-name",     i < names.size() ? names[i] : "");
				write_key_pair(fd, "wait",             to_s(shouldWait));
				write_key_pair(fd, "re-activate",      to_s(shouldWait));

				if(addToRecent == boolean::kUnset && shouldWait != boolean::kEnable && path::name(files[i]).front() != '.' && !is_temporary_file(files[i]))
					write_key_pair(fd, "add-to-recents", "yes");
			}

			if(addToRecent != boolean::kUnset)
				write_key_pair(fd, "add-to-recents", to_s(addToRecent));

			if(geteuid() == 0 && auth.obtain_right(kAuthRightName))
				write_key_pair(fd, "authorization", auth);

			write_key_pair(fd, "selection",        i < lines.size()    ? lines[i] : "");
			write_key_pair(fd, "file-type",        i < types.size()    ? types[i] : "");
			write_key_pair(fd, "project-uuid",     i < projects.size() ? projects[i] : defaultProject);
			write_key_pair(fd, "change-directory", to_s(changeDir));

			write(fd, "\r\n", 2);
		}
	}

	write(fd, ".\r\n", 3);

	// =========================
	// = Now wait for messages =
	// =========================

	enum { command, arguments, data, done } state = command;
	ssize_t bytesLeft = 0;

	std::string line;
	while(state != done && (len = read(fd, buf, sizeof(buf))))
	{
		if(len == -1)
		{
			perror("read");
			break;
		}

		if(state == data)
		{
			ssize_t dataLen = std::min(len, bytesLeft);
			write(STDOUT_FILENO, buf, dataLen);
			memmove(buf, buf + dataLen, len - dataLen);
			bytesLeft -= dataLen;
			len -= dataLen;

			state = bytesLeft == 0 ? arguments : data;
		}

		line.insert(line.end(), buf, buf + len);

		if(state == data && bytesLeft != 0)
			continue;

		while(line.find('\n') != std::string::npos)
		{
			std::string::size_type eol = line.find('\n');
			std::string str = line.substr(0, eol);
			if(!str.empty() && str.back() == '\r')
				str.resize(str.size()-1);
			line.erase(line.begin(), line.begin() + eol + 1);

			if(str.empty())
			{
				state = command;
			}
			else if(state == command)
			{
				if(str == "close")
				{
					state = arguments;
				}
			}
			else if(state == arguments)
			{
				std::string::size_type n = str.find(':');
				if(n != std::string::npos)
				{
					std::string key   = str.substr(0, n);
					std::string value = str.substr(n+2);

					if(key == "data")
					{
						bytesLeft = strtol(value.c_str(), nullptr, 10);

						size_t dataLen = std::min((ssize_t)line.size(), bytesLeft);
						write(STDOUT_FILENO, line.data(), dataLen);
						line.erase(line.begin(), line.begin() + dataLen);
						bytesLeft -= dataLen;

						state = bytesLeft == 0 ? arguments : data;
					}
				}
			}
		}
	}

	close(fd);
	return EX_OK;
}
