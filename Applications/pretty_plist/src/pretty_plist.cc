#include <plist/ascii.h>

static double const AppVersion = 2.1;

static void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" __DATE__ ")\n", getprogname(), AppVersion);
}

static void usage (FILE* io)
{
	fprintf(io,
		"%1$s %2$.1f (" __DATE__ ")\n"
		"Usage: %1$s [-axhv] file\n"
		"Description:\n"
		" Reads a property list and outputs ASCII version to stdout.\n"
		" If no file is specified, stdin is used.\n"
		"Options:\n"
		" -a, --ascii     Parse TextMate’s ASCII property list variant.\n"
		" -x, --extended  Output TextMate’s ASCII property list variant.\n"
		" -h, --help      Show this information.\n"
		" -v, --version   Print version information.\n"
		"\n", getprogname(), AppVersion
	);
}

static void parse_plist (FILE* fp, bool ascii, bool extended)
{
	std::string data;

	char buf[1024];
	while(size_t len = fread(buf, 1, sizeof(buf), fp))
		data.insert(data.end(), buf, buf + len);

	if(data.empty())
	{
		fprintf(stderr, "empty property list\n");
		exit(EX_DATAERR);
	}

	if(ascii)
	{
		if(CFPropertyListRef cfPlist = plist::create_cf_property_list(plist::parse_ascii(data)))
		{
			if(CFURLRef url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, CFSTR("/dev/stdout"), kCFURLPOSIXPathStyle, false))
			{
				if(CFWriteStreamRef writeStream = CFWriteStreamCreateWithFile(kCFAllocatorDefault, url))
				{
					CFErrorRef error = nullptr;
					CFWriteStreamOpen(writeStream);
					CFPropertyListWrite(cfPlist, writeStream, kCFPropertyListXMLFormat_v1_0, 0, &error);
					CFWriteStreamClose(writeStream);
					CFRelease(writeStream);
				}
				CFRelease(url);
			}
			CFRelease(cfPlist);
		}
	}
	else
	{
		std::string const& str = to_s(plist::parse(data), extended ? plist::kPreferSingleQuotedStrings : 0) + "\n";
		write(STDOUT_FILENO, str.data(), str.size());
	}
}

int main (int argc, char* const* argv)
{
	// extern char* optarg;
	extern int optind;

	static struct option const longopts[] = {
		{ "ascii",            no_argument,         0,      'a'   },
		{ "extended",         no_argument,         0,      'x'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	bool ascii = false, extended = false;

	int ch;
	while((ch = getopt_long(argc, argv, "axhv", longopts, nullptr)) != -1)
	{
		switch(ch)
		{
			case 'a': ascii = true;        break;
			case 'x': extended = true;     break;
			case 'h': usage(stdout);       return EX_OK;
			case 'v': version();           return EX_OK;
			default:  usage(stderr);       return EX_USAGE;
		}
	}

	argc -= optind;
	argv += optind;

	if(argc == 0)
		parse_plist(stdin, ascii, extended);

	for(int i = 0; i < argc; ++i)
	{
		if(FILE* fp = fopen(argv[i], "r"))
		{
			parse_plist(fp, ascii, extended);
			fclose(fp);
		}
		else
		{
			perror("fopen");
			exit(EX_NOINPUT);
		}
	}

	return EX_OK;
}
