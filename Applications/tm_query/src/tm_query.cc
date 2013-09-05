#include <settings/settings.h>

extern char** environ;

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
		"Usage: %1$s [-s<key>hv] ...\n"
		"Options:\n"
		" -s, --setting <key>       Print setting value for a key.\n"
		" -h, --help                Show this information.\n"
		" -v, --version             Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
}

static void initialize_environment () {
	if(char const* appPath = getenv("TM_APP_PATH"))
	{
		std::string defaultSettings = path::join(appPath, "Contents/Resources/Default.tmProperties");
		settings_t::set_default_settings_path(defaultSettings);
	}

	settings_t::set_global_settings_path(path::join(path::home(), "Library/Application Support/TextMate/Global.tmProperties"));

	std::map<std::string, std::string> env;
	for(char** pair = environ; *pair; ++pair)
	{
		char* value = strchr(*pair, '=');
		if(value && *value == '=')
			env.emplace(std::string(*pair, value), value + 1);
	}
	oak::set_basic_environment(env);
}

static bool print_settings (settings_t const& settings, std::string const& key) {
	if (key != NULL_STR) {
		std::string value = settings.get(key, NULL_STR);
		// if key not found, print error
		if (value == NULL_STR) {
			fprintf(stderr, "Setting or variable '%s' not found\n", key.c_str());
			return false;
		} else {
			fprintf(stdout, "%s\n", value.c_str());
		}
	} else {
		// no key specified. Print all keys and values
		for(auto pair : settings.all_settings()) {
			if(!pair.first.empty() && islower(pair.first[0]))
				fprintf(stdout, "%s=%s\n", pair.first.c_str(), pair.second.c_str());
		}
	}
	return true;
}

int main (int argc, char* const* argv)
{
	// extern char* optarg;
	extern int optind;

	static struct option const longopts[] = {
		{ "setting",          required_argument,   0,      's'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	std::string key = NULL_STR;

	int ch;
	while((ch = getopt_long(argc, argv, "s:hv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 's': key = optarg;        break;
			case 'h': usage();             return 0;
			case 'v': version();           return 0;
			default:  usage(stderr);       return 1;
		}
	}

	argc -= optind;
	argv += optind;

	initialize_environment();

	for(int i = 0; i < argc; ++i)
	{
		// new line before additional files
		if (i > 0) {
			fprintf(stdout, "\n");
		}
		// if more than one file print its name
		if (argc > 1) {
			fprintf(stdout, "%s:\n", argv[i]);
		}

		settings_t const settings = settings_for_path(path::join(path::cwd(), argv[i]));
		if(!print_settings(settings, key))
			return 1;
	}
	if (0 == argc) {
		std::string path  = getenv("TM_FILEPATH")  ?: NULL_STR;
		std::string scope = getenv("TM_SCOPE")     ?: NULL_STR;
		std::string dir   = getenv("TM_DIRECTORY") ?: getenv("TM_PROJECT_DIRECTORY") ?: NULL_STR;

		settings_t const settings = settings_for_path(path, scope, dir);
		if(!print_settings(settings, key))
			return 1;
	}

	return 0;
}
