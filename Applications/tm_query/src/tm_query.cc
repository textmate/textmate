#include <settings/settings.h>

static double const AppVersion  = 1.0;
static size_t const AppRevision = APP_REVISION;

static void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

static void usage (FILE* io = stdout)
{
	fprintf(io,
		"%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n"
		"Usage: %1$s [-hv] ...\n"
		"Options:\n"
		" -h, --help                Show this information.\n"
		" -v, --version             Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
}

int main (int argc, char* const* argv)
{
	// extern char* optarg;
	extern int optind;

	static struct option const longopts[] = {
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	int ch;
	while((ch = getopt_long(argc, argv, "hv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'h': usage();             return 0;
			case 'v': version();           return 0;
			default:  usage(stderr);       return 1;
		}
	}

	argc -= optind;
	argv += optind;

	for(int i = 0; i < argc; ++i)
	{
		std::string file = path::join(path::cwd(), argv[i]);

		// settings_t settings_for_path (std::string const& path = NULL_STR, scope::scope_t const& scope = "", std::string const& directory = NULL_STR, std::map<std::string, std::string> variables = std::map<std::string, std::string>());

		// std::map<std::string, std::string> variables_for_path (std::string const& documentPath = NULL_STR, scope::scope_t const& scope = "", std::map<std::string, std::string> existingVariables = std::map<std::string, std::string>());

		fprintf(stderr, "show settings for %s\n", file.c_str());
	}

	return 0;
}