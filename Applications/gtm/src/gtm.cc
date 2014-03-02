#include <parse/grammar.h>
#include <parse/parse.h>
#include <test/bundle_index.h>
#include <oak/duration.h>
#include <oak/oak.h>
#include <iostream>

static double const AppVersion  = 1.0;
static size_t const AppRevision = APP_REVISION;

void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

void usage (FILE* io)
{
	fprintf(io,
		"%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n"
		"Usage: %1$s [-g<selector>td<string>lhv] grammar ...\n"
		"Options:\n"
		" -g, --grammar <selector>  Which grammar to use.\n"
		" -t, --trim                Show only first letter of each scope fragment.\n"
		" -d, --delimiters <string> Surround scopes using argument. See example.\n"
		" -l, --verbose             Be verbose (output timings).\n"
		" -i, --load-index          Load bundle index (standard grammars available).\n"
		" -h, --help                Show this information.\n"
		" -v, --version             Print version information.\n"
		"\n", getprogname(), AppVersion, AppRevision
	);
}

template <typename _InputIter, typename _OutputIter>
_OutputIter entity_escape (_InputIter first, _InputIter const& last, _OutputIter out)
{
	ASSERT(!(last < first));

	static typename std::iterator_traits<_InputIter>::value_type const special[] = { '<', '>', '&' };
	static std::string const escaped[] = { "&lt;", "&gt;", "&amp;" };

	while(first != last)
	{
		_InputIter it = std::find_first_of(first, last, special, special + sizeofA(special));
		out = std::copy(first, it, out);
		first = it;

		if(first != last)
		{
			size_t idx = std::find(special, special + sizeofA(special), *first) - special;
			out = std::copy(escaped[idx].begin(), escaped[idx].end(), out);
			++first;
		}
	}
	return out;
}

void parse_stdin (std::string const& grammarSelector = "text.plain", bool verbose = false)
{
	for(auto const& item : bundles::query(bundles::kFieldGrammarScope, grammarSelector, scope::wildcard, bundles::kItemTypeGrammar))
	{
		if(parse::grammar_ptr grammar = parse::parse_grammar(item))
		{
			parse::stack_ptr stack = grammar->seed();
			scope::scope_t lastScope(grammarSelector);

			oak::duration_t timer;
			size_t bytes = 0;

			static char buf[16384];
			while(fgets(buf, sizeof(buf), stdin))
			{
				std::map<size_t, scope::scope_t> scopes;
				stack = parse::parse(buf, buf + strlen(buf), stack, scopes, bytes == 0);
				bytes += strlen(buf);

				size_t lastPos = 0;
				for(auto const& it : scopes)
				{
					entity_escape(buf + lastPos, buf + it.first, std::ostream_iterator<char>(std::cout));
					// std::copy(buf + lastPos, buf + it.first, std::ostream_iterator<char>(std::cout));
					std::cout << xml_difference(lastScope, it.second, "<", ">");
					lastScope = it.second;
					lastPos = it.first;
				}
				entity_escape(buf + lastPos, buf + strlen(buf), std::ostream_iterator<char>(std::cout));
				// std::copy(buf + lastPos, buf + strlen(buf), std::ostream_iterator<char>(std::cout));
			}
			std::cout << xml_difference(lastScope, grammarSelector, "<", ">") << std::endl;

			if(verbose)
				fprintf(stderr, "parsed %zu bytes in %.1fs (%.0f bytes/s)\n", bytes, timer.duration(), bytes / timer.duration());

			return;
		}
	}

	fprintf(stderr, "%s: unable to find grammar for selector ‘%s’\n", getprogname(), grammarSelector.c_str());
	exit(1);
}

static void load_bundle_index (bool verbose)
{
	oak::duration_t timer;

	std::string const path = path::join(path::home(), "Library/Caches/com.macromates.TextMate/BundlesIndex.binary");

	plist::cache_t cache;
	cache.load_capnp(path);

	std::vector<std::string> paths;
	for(auto path : bundles::locations())
		paths.push_back(path::join(path, "Bundles"));
	
	auto index = create_bundle_index(paths, cache);
	bundles::set_index(index.first, index.second);

	if(verbose)
		fprintf(stderr, "loaded bundle index in %.2f seconds\n", timer.duration());
}

int main (int argc, char* const* argv)
{
	extern char* optarg;
	extern int optind;

	static struct option const longopts[] = {
		{ "grammar",          required_argument,   0,      'g'   },
		{ "trim",             no_argument,         0,      't'   },
		{ "delimiters",       required_argument,   0,      'd'   },
		{ "verbose",          no_argument,         0,      'l'   },
		{ "load-index",       no_argument,         0,      'i'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	bool verbose = false, trim = false, loadIndex = false;
	std::string grammar = NULL_STR, delimiters = NULL_STR;

	int ch;
	while((ch = getopt_long(argc, argv, "g:td:lihv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'g': grammar = optarg;    break;
			case 't': trim = true;         break; // TODO
			case 'd': delimiters = optarg; break; // TODO
			case 'l': verbose = true;      break;
			case 'i': loadIndex = true;    break;
			case 'h': usage(stdout);       return 0;
			case 'v': version();           return 0;
			default:  usage(stderr);       return 1;
		}
	}

	argc -= optind;
	argv += optind;

	if(loadIndex)
	{
		load_bundle_index(verbose);
	}
	else
	{
		size_t grammars = 0;

		test::bundle_index_t bundleIndex;
		for(int i = 0; i < argc; ++i)
		{
			if(access(argv[i], R_OK) != 0)
			{
				fprintf(stderr, "%s: error reading grammar ‘%s’\n", getprogname(), argv[i]);
				exit(1);
			}

			std::string tmp;
			plist::dictionary_t plist = plist::load(argv[i]);
			if(!plist::get_key_path(plist, "scopeName", tmp))
			{
				fprintf(stderr, "%s: error parsing grammar ‘%s’\n", getprogname(), argv[i]);
				exit(1);
			}

			if(!bundleIndex.add(bundles::kItemTypeGrammar, plist))
			{
				fprintf(stderr, "%s: error parsing grammar ‘%s’\n", getprogname(), argv[i]);
				exit(1);
			}

			if(grammar == NULL_STR)
				grammar = tmp;

			++grammars;
		}

		if(grammars == 0 || !bundleIndex.commit())
		{
			fprintf(stderr, "%s: no grammars loaded\n", getprogname());
			exit(1);
		}
	}

	parse_stdin(grammar, verbose);
	return 0;
}