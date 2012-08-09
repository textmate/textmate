#include <plist/ascii.h>

static double const AppVersion  = 2.0;
static size_t const AppRevision = APP_REVISION;

void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

void usage (FILE* io)
{
	fprintf(io,
		"%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n"
		"Usage: %1$s [-axhv]\n"
		"Description:\n"
		" Reads a property list from standard input and outputs ASCII version to stdout.\n"
		"Options:\n"
		" -a, --ascii     Parse TextMate’s ASCII property list variant.\n"
		" -x, --extended  Output TextMate’s ASCII property list variant.\n"
		" -h, --help      Show this information.\n"
		" -v, --version   Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
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
	while((ch = getopt_long(argc, argv, "axhv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'a': ascii = true;        break;
			case 'x': extended = true;     break;
			case 'h': usage(stdout);       return 0;
			case 'v': version();           return 0;
			default:  usage(stderr);       return 1;
		}
	}

	argc -= optind;
	argv += optind;

	std::string data;

	char buf[1024];
	while(size_t len = fread(buf, 1, sizeof(buf), stdin))
		data.insert(data.end(), buf, buf + len);

	if(ascii)
	{
		if(CFPropertyListRef cfPlist = plist::create_cf_property_list(plist::parse_ascii(data)))
		{
			if(CFURLRef url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, CFSTR("/dev/stdout"), kCFURLPOSIXPathStyle, false))
			{
				if(CFWriteStreamRef writeStream = CFWriteStreamCreateWithFile(kCFAllocatorDefault, url))
				{
					CFStringRef error = NULL;
					CFWriteStreamOpen(writeStream);
					CFPropertyListWriteToStream(cfPlist, writeStream, kCFPropertyListXMLFormat_v1_0, &error);
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

	return 0;
}
