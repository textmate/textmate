//
//  client.mm
//  Created by Allan Odgaard on 2007-09-22.
//

#import "Dialog2.h"

static double const AppVersion = 2.0;

id connect ()
{
	NSString* portName = kDialogServerConnectionName;
	if(char const* var = getenv("DIALOG_PORT_NAME"))
		portName = @(var);

	id proxy = [NSConnection rootProxyForConnectionWithRegisteredName:portName host:nil];
	[proxy setProtocolForProxy:@protocol(DialogServerProtocol)];
	return proxy;
}

char const* create_pipe (char const* name)
{
	char* filename;
	asprintf(&filename, "%s/dialog_fifo_%d_%s", getenv("TMPDIR") ?: "/tmp", getpid(), name);
	int res = mkfifo(filename, 0666);
	if((res == -1) && (errno != EEXIST))
	{
		perror("Error creating the named pipe");
		exit(EX_OSERR);
   }
	return filename;
}

int open_pipe (char const* name, int oflag)
{
	int fd = open(name, oflag);
	if(fd == -1)
	{
		perror("Error opening the named pipe");
		exit(EX_IOERR);
	}
	return fd;
}

int main (int argc, char const* argv[])
{
	if(argc == 2 && strcmp(argv[1], "--version") == 0)
	{
		fprintf(stderr, "%1$s %2$.1f (" __DATE__ ")\n", getprogname(), AppVersion);
		return EX_OK;
	}

	// If the argument list starts with a switch then assume it’s meant for trunk dialog
	// and pass it off
	if(argc > 1 && *argv[1] == '-')
		execv(getenv("DIALOG_1"), (char* const*)argv);

	@autoreleasepool{
		id<DialogServerProtocol> proxy = connect();
		if(!proxy)
		{
			fprintf(stderr, "error reaching server\n");
			exit(EX_UNAVAILABLE);
		}

		char const* stdinName  = create_pipe("stdin");
		char const* stdoutName = create_pipe("stdout");
		char const* stderrName = create_pipe("stderr");

		NSMutableArray* args = [NSMutableArray array];
		for(size_t i = 0; i < argc; ++i)
			[args addObject:@(argv[i])];

		NSDictionary* dict = @{
			@"stdin":       @(stdinName),
			@"stdout":      @(stdoutName),
			@"stderr":      @(stderrName),
			@"cwd":         @(getcwd(NULL, 0)),
			@"environment": [[NSProcessInfo processInfo] environment],
			@"arguments":   args,
		};

		[proxy connectFromClientWithOptions:dict];

		int inputFd  = open_pipe(stdinName, O_WRONLY);
		int outputFd = open_pipe(stdoutName, O_RDONLY);
		int errorFd = open_pipe(stderrName, O_RDONLY);

		std::map<int, int> fdMap;
		fdMap[STDIN_FILENO] = inputFd;
		fdMap[outputFd]     = STDOUT_FILENO;
		fdMap[errorFd]      = STDERR_FILENO;

		if(isatty(STDIN_FILENO) != 0)
		{
			fdMap.erase(fdMap.find(STDIN_FILENO));
			close(inputFd);
		}

		while(fdMap.size() > 1 || (fdMap.size() == 1 && fdMap.find(STDIN_FILENO) == fdMap.end()))
		{
			fd_set readfds, writefds;
			FD_ZERO(&readfds); FD_ZERO(&writefds);

			int fdCount = 0;
			for(auto const& pair : fdMap)
			{
				FD_SET(pair.first, &readfds);
				fdCount = std::max(fdCount, pair.first + 1);
			}

			int i = select(fdCount, &readfds, &writefds, NULL, NULL);
			if(i == -1)
			{
				perror("Error from select");
				continue;
			}

			std::vector<int> toRemove;
			for(auto const& pair : fdMap)
			{
				if(FD_ISSET(pair.first, &readfds))
				{
					char buf[1024];
					ssize_t len = read(pair.first, buf, sizeof(buf));

					if(len == 0)
							toRemove.push_back(pair.first); // we can’t remove as long as we need the iterator for the ++
					else	write(pair.second, buf, len);
				}
			}

			for(int key : toRemove)
			{
				if(fdMap[key] == inputFd)
					close(inputFd);
				fdMap.erase(key);
			}
		}

		close(outputFd);
		close(errorFd);
		unlink(stdinName);
		unlink(stdoutName);
		unlink(stderrName);
	}

	return EX_OK;
}
