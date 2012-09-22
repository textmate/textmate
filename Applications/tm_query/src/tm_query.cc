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
		" -k, --key <key>				 Print variable or setting value for a key.\n"
		" -h, --help                Show this information.\n"
		" -v, --version             Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
}

static void print_map(std::map<std::string, std::string> map) {
	std::map<std::string, std::string>::iterator it;
	for (it = map.begin(); it != map.end(); it++) {
		fprintf(stdout, "%s=%s\n", it->first.c_str(), it->second.c_str());
	}
}

int main (int argc, char* const* argv)
{
	// extern char* optarg;
	extern int optind;

	static struct option const longopts[] = {
		{ "key",              required_argument,   0,      'k'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	std::string key = NULL_STR;

	int ch;
	while((ch = getopt_long(argc, argv, "k:hv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'k': key = optarg;        break;
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
		
		settings_t settings = settings_for_path(file);
		std::map<std::string, std::string> variables_map = variables_for_path(file);
		
		// new line before additional files
		if (i > 0) {
			fprintf(stdout, "\n");
		}
		// if more than one file print its name
		if (argc > 1) {
			fprintf(stdout, "%s:\n", file.c_str());
		}
		
		if (key != NULL_STR) {
			std::map<std::string, std::string>::const_iterator variable_it = variables_map.find(key);
			std::string variable_value = (variable_it == variables_map.end()) ? NULL_STR : variable_it->second;
			
			// first find settings, else try variable
			std::string value = settings.get(key, NULL_STR);
			if (value == NULL_STR) {
				value = variable_value;
			}
			
			// if key not found, print error
			if (value == NULL_STR) {
				fprintf(stderr, "Setting or variable '%s' not found\n", key.c_str());
				continue;
			} else {
				fprintf(stdout, "%s\n", value.c_str());
			}
		} else {
			// no key specified. Print all keys and values
			std::map<std::string, std::string> settings_map = settings.all_settings();
			print_map(settings_map);
			print_map(variables_map);
		}
	}
	if (0 == argc) {
		fprintf(stderr, "No files specified. Run %s -h for help.\n", getprogname());
	}

	return 0;
}
