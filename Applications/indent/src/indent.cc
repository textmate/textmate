#include <text/indent.h>
#include <text/tokenize.h>
#include <io/path.h>
#include <regexp/indent.h>
#include <plist/plist.h>
#include <oak/oak.h>

static double const AppVersion  = 1.0;
static size_t const AppRevision = APP_REVISION;

extern char* optarg;
extern int optind;

static void usage (FILE* io = stdout)
{
	fprintf(io, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
	fprintf(io, "Usage: %s [ptishv] «file» ..\n", getprogname());
	fprintf(io, "\n");
	fprintf(io, "Options:\n");
	fprintf(io, " -p/--patterns «plist» A tmPreferences file with indent settings.\n");
	fprintf(io, " -t/--tab «size»       Set tab size (default: 3).\n");
	fprintf(io, " -i/--indent «size»    Set indent size (default: tab size).\n");
	fprintf(io, " -s/--spaces           Use spaces instead of tabs for indent.\n");
	fprintf(io, " -h/--help             Show this help.\n");
	fprintf(io, " -v/--version          Show version number.\n");
}

static void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

int main (int argc, char const* argv[])
{
	static struct option const longopts[] = {
		{ "patterns",         required_argument,   0,      'p'   },
		{ "tab",              required_argument,   0,      't'   },
		{ "indent",           required_argument,   0,      'i'   },
		{ "spaces",           no_argument,         0,      's'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	std::string patterns = NULL_STR;
	size_t tabSize       = 3;
	size_t indentSize    = 0;
	bool softTabs        = false;

	unsigned int ch;
	while((ch = getopt_long(argc, (char* const*)argv, "p:t:i:shv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'p': patterns = optarg;                      break;
			case 't': tabSize = strtol(optarg, NULL, 10);     break;
			case 'i': indentSize = strtol(optarg, NULL, 10);  break;
			case 's': softTabs = true;                        break;
			case 'h': usage();                                return 0;
			case 'v': version();                              return 0;
			case '?': /* unknown option */                    return 1;
			case ':': /* missing option */                    return 1;
			default:  usage(stderr);                          return 1;
		}
	}

	if(optind == argc)
		return usage(stderr), 0;

	std::vector<std::string> files;
	for(int i = optind; i < argc; ++i)
		files.push_back(argv[i]);

	patterns   = patterns == NULL_STR ? "/Users/duff/Library/Application Support/Avian/Bundles/php.tmbundle/Preferences/Indentation Rules.tmPreferences" : patterns;
	indentSize = indentSize == 0 ? tabSize : indentSize;
	if(files.empty())
		files.push_back("/Users/duff/Desktop/indent-test.html");

	plist::dictionary_t plist = plist::load(patterns);
	static std::map<indent::pattern_type, std::string> const map =
	{
		{ "increaseIndentPattern", indent::pattern_type::kIncrease     },
		{ "decreaseIndentPattern", indent::pattern_type::kDecrease     },
		{ "indentNextLinePattern", indent::pattern_type::kIncreaseNext },
		{ "unIndentedLinePattern", indent::pattern_type::kIgnore       },
		{ "zeroIndentPattern",     indent::pattern_type::kZeroIndent   },
	};

	std::map<indent::pattern_type, regexp::pattern_t> array;
	for(auto pair : map)
	{
		std::string tmp;
		if(plist::get_key_path(plist, "settings." + pair.first, tmp))
			array.emplace(pair.second, tmp);
	}
	
	// ==========
	// = Indent =
	// ==========

	text::indent_t indent(tabSize, indentSize, softTabs);
	for(auto const& file : files)
	{
		indent::fsm_t fsm(1, 1);

		std::string const str = path::content(file);
		std::string::size_type n = 0;
		while(n != std::string::npos)
		{
			while(n < str.size() && (str[n] == ' ' || str[n] == '\t'))
				++n;

			std::string::size_type eol = str.find('\n', n);
			if(eol < str.size())
				++eol;

			std::string line = str.substr(n, eol - n);
			fprintf(stdout, "%s%s", indent.create(0, fsm.scan_line(line, array)).c_str(), line.c_str());

			n = eol;
		}
	}

	return 0;
}
