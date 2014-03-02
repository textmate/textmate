#include <updater/updater.h>
#include <regexp/format_string.h>
#include <regexp/glob.h>
#include <oak/oak.h>
#include <text/case.h>
#include <text/ctype.h>
#include <text/decode.h>
#include <text/format.h>
#include <io/io.h>
#include <OakSystem/application.h>

static double const AppVersion  = 1.2;
static size_t const AppRevision = APP_REVISION;

// example: bl install Apache AppleScript Blogging Bundle\ Development C CSS Diff Git HTML Hyperlink\ Helper JavaScript Mail Make Markdown Math Objective-C PHP Perl Property\ List Ragel Remind Ruby SQL Shell\ Script Source Subversion TODO Text TextMate XML Xcode 

static int get_width ()
{
	if(!isatty(STDIN_FILENO))
		return INT_MAX;

	struct winsize ws; 
	if(ioctl(0, TIOCGWINSZ, &ws) == -1)
	{
		fprintf(stderr, "TIOCGWINSZ: %s\n", strerror(errno));
		exit(1);
	}
	return ws.ws_col;
}

static std::string textify (std::string str)
{
	str = format_string::replace(str, "\\A\\s+|<[^>]*>|\\s+\\z", "");
	str = format_string::replace(str, "\\s+", " ");
	str = decode::entities(str);
	return str;
}

extern char* optarg;
extern int optind;

static void usage (FILE* io = stdout)
{
	fprintf(io, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
	fprintf(io, "Usage: %s list [Cius] [«bundle» ..]\n", getprogname());
	fprintf(io, "       %s install [Cs] «bundle» ..\n", getprogname());
	fprintf(io, "       %s uninstall [Cs] «bundle» ..\n", getprogname());
	fprintf(io, "       %s show [C] «bundle» ..\n", getprogname());
	fprintf(io, "       %s dependencies [Cs] [«bundle» ..]\n", getprogname());
	fprintf(io, "       %s dependents [Ci] [«bundle» ..]\n", getprogname());
	fprintf(io, "       %s update [C]\n", getprogname());
	fprintf(io, "       %s help\n", getprogname());
	fprintf(io, "\n");
	fprintf(io, "Options:\n");
	fprintf(io, " -h/--help              Show this help.\n");
	fprintf(io, " -v/--version           Show version number.\n");
	fprintf(io, " -C/--directory «path»  Use «path» for local bundles.\n");
	fprintf(io, " -i/--installed         Only include installed bundles.\n");
	fprintf(io, " -u/--updated           Only include bundles which have pending updates.\n");
	fprintf(io, " -s/--source «name»     Restrict bundles to the given source. Multiple sources are allowed.\n");
	fprintf(io, "\n");
	fprintf(io, "Globs: Both source and bundle names can contain wild cards:\n");
	fprintf(io, " ?               Match any single character\n");
	fprintf(io, " *               Match any characters\n");
	fprintf(io, " {«a»,«b»,«c»}   Match «a», «b», or «c».\n");
}

static bool matches (std::string const& str, std::vector<std::string> const& globs)
{
	if(globs.empty())
		return true;

	for(auto const& glob : globs)
	{
		if(path::glob_t(glob).does_match(str))
			return true;
	}

	return false;
}

static std::vector<bundles_db::bundle_ptr> filtered_bundles (std::vector<bundles_db::bundle_ptr> const& index, std::vector<std::string> const& sourceNames, std::vector<std::string> const& bundleNames)
{
	std::vector<bundles_db::bundle_ptr> res;
	std::set<oak::uuid_t> seen;
	for(auto const& bundle : index)
	{
		if(matches(bundle->source() ? bundle->source()->identifier() : NULL_STR, sourceNames) && matches(text::lowercase(bundle->name()), bundleNames))
		{
			if(seen.find(bundle->uuid()) == seen.end())
			{
				seen.insert(bundle->uuid());
				res.push_back(bundle);
			}
		}
	}
	return res;
}

static std::string short_bundle_info (bundles_db::bundle_ptr bundle, int width)
{
	int descWidth = std::max(10, width - 23);
	char status = bundle->installed() ? (bundle->has_update() ? 'U' : 'I') : ' ';
	std::string desc = bundle->description();
	desc = desc == NULL_STR ? "(no description)" : textify(desc);
	if(desc.size() > descWidth)
		desc.resize(descWidth);
	return text::format("%c %-20.20s %s", status, bundle->name().c_str(), desc.c_str());
}

static void version ()
{
	fprintf(stdout, "%1$s %2$.1f (" COMPILE_DATE " revision %3$zu)\n", getprogname(), AppVersion, AppRevision);
}

int main (int argc, char const* argv[])
{
	oak::application_t::set_support(path::join(path::home(), "Library/Application Support/TextMate"));
	oak::application_t app(argc, argv);

	static struct option const longopts[] = {
		{ "directory",        required_argument,   0,      'C'   },
		{ "source",           required_argument,   0,      's'   },
		{ "updated",          no_argument,         0,      'u'   },
		{ "installed",        no_argument,         0,      'i'   },
		{ "help",             no_argument,         0,      'h'   },
		{ "version",          no_argument,         0,      'v'   },
		{ 0,                  0,                   0,      0     }
	};

	std::vector<std::string> sourceNames;
	std::string installDir = NULL_STR;
	bool onlyUpdated       = false;
	bool onlyInstalled     = false;

	unsigned int ch;
	while((ch = getopt_long(argc, (char* const*)argv, "s:uiC:hv", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'u': onlyUpdated = true;                     break;
			case 'i': onlyInstalled = true;                   break;
			case 'C': installDir = optarg;                    break;
			case 's': sourceNames.push_back(optarg);          break;
			case 'v': version();                              return 0;
			case 'h': usage();                                return 0;
			case '?': /* unknown option */                    return 1;
			case ':': /* missing option */                    return 1;
			default:  usage(stderr);                          return 1;
		}
	}

	if(optind == argc)
		return usage(stderr), 1;

	std::string command = argv[optind];
	if(command == "help")
		return usage(stdout), 0;

	std::vector<std::string> bundleNames;
	for(int i = optind + 1; i < argc; ++i)
		bundleNames.push_back(text::lowercase(argv[i]));

	static std::set<std::string> const CommandsNeedingBundleList = { "install", "uninstall", "show", "dependents" };
	if(bundleNames.empty() && CommandsNeedingBundleList.find(command) != CommandsNeedingBundleList.end())
	{
		fprintf(stderr, "no bundles specified\n");
		return 1;
	}

	static std::set<std::string> const CommandsNeedingUpdatedSources = { "update", "install", "list", "show" };
	if(CommandsNeedingUpdatedSources.find(command) != CommandsNeedingUpdatedSources.end())
	{
		std::vector<bundles_db::source_ptr> toUpdate;
		for(auto const& source : bundles_db::sources(installDir))
		{
			if(!source->disabled() && source->needs_update())
				toUpdate.push_back(source);
		}

		__block std::vector<bundles_db::source_ptr> failedUpdate;
		dispatch_apply(toUpdate.size(), dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^(size_t i){
			fprintf(stderr, "Downloading ‘%s’…\n", toUpdate[i]->url().c_str());
			if(!update(toUpdate[i]))
				failedUpdate.push_back(toUpdate[i]);
		});

		for(auto source : failedUpdate)
			fprintf(stderr, "*** failed to update source: ‘%s’ (%s)\n", source->name().c_str(), source->url().c_str());

		if(!failedUpdate.empty())
			exit(1);
	}

	std::vector<bundles_db::bundle_ptr> index = bundles_db::index(installDir);
	if(command == "list")
	{
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(!((onlyInstalled && !bundle->installed()) || (onlyUpdated && !onlyInstalled && !bundle->has_update())))
				fprintf(stdout, "%s\n", short_bundle_info(bundle, get_width()).c_str());
		}
	}
	else if(command == "install")
	{
		std::vector<bundles_db::bundle_ptr> toInstall;
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(!bundle->installed())
					toInstall.push_back(bundle);
			else	fprintf(stderr, "skip ‘%s’ (%s) -- already installed\n", bundle->name().c_str(), bundle->origin().c_str());
		}

		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			size_t n = toInstall.size();
			__block std::vector<bundles_db::bundle_ptr> failed;
			__block std::vector<double> progress(n);

			if(dispatch_source_t timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, dispatch_get_main_queue()))
			{
				dispatch_source_set_timer(timer, DISPATCH_TIME_NOW, NSEC_PER_SEC / 10, NSEC_PER_SEC / 5);
				dispatch_source_set_event_handler(timer, ^{
					double current = 0, total = 0;
					for(size_t i = 0; i < n; ++i)
					{
						current += toInstall[i]->size() * progress[i];
						total   += toInstall[i]->size();
					}
					fprintf(stderr, "\rDownloading... %4.1f%%", 100 * current / total);
				});
				dispatch_resume(timer);
			}

			dispatch_apply(n, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^(size_t i){
				if(!install(toInstall[i], installDir, &progress[i]))
					failed.push_back(toInstall[i]);
			});

			dispatch_async(dispatch_get_main_queue(), ^{
				fprintf(stderr, "\rDownloading... Done!   \n");
				if(!failed.empty())
				{
					std::vector<std::string> names;
					std::transform(failed.begin(), failed.end(), back_inserter(names), [](bundles_db::bundle_ptr const& bundle){ return bundle->name(); });
					fprintf(stderr, "*** failed to install %s.\n", text::join(names, ", ").c_str());
				}
				save_index(index, installDir);
				if(!failed.empty())
					exit(1);
				exit(0);
			});
		});
		dispatch_main();
	}
	else if(command == "uninstall")
	{
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(bundle->installed())
			{
				fprintf(stderr, "Uninstalling ‘%s’...", bundle->name().c_str());
				if(uninstall(bundle, installDir))
						fprintf(stderr, "ok!\n");
				else	fprintf(stderr, " *** failed!\n");
			}
			else
			{
				fprintf(stderr, "skip ‘%s’ (%s) -- not installed\n", bundle->name().c_str(), bundle->origin().c_str());
			}
		}
		save_index(index, installDir);
	}
	else if(command == "show")
	{
		bool first = true;
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(first)
					first = false;
			else	fprintf(stdout, "\n");

			fprintf(stdout, "name:         %s\n", bundle->name().c_str());
			fprintf(stdout, "source:       %s\n", bundle->source() ? bundle->source()->identifier().c_str() : "«no remote source»");
			fprintf(stdout, "uuid:         %s\n", to_s(bundle->uuid()).c_str());

			bool hasName  = bundle->contact_name() != NULL_STR;
			bool hasEmail = bundle->contact_email() != NULL_STR;
			char const* fmt[] = { "", "contact:      %1$s\n", "contact:      <%2$s>\n\0%1$s", "contact:      %1$s <%2$s>\n" };
			fprintf(stdout, fmt[(hasName ? 1 : 0) + (hasEmail ? 2 : 0)], bundle->contact_name().c_str(), bundle->contact_email().c_str());

			if(bundle->url_updated())
				fprintf(stdout, "date:         %s\n", to_s(bundle->url_updated()).c_str());
			if(bundle->url() != NULL_STR)
				fprintf(stdout, "url:          %s (%d bytes)\n", bundle->url().c_str(), bundle->size());
			if(bundle->path() != NULL_STR)
				fprintf(stdout, "path:         %s\n", path::with_tilde(bundle->path()).c_str());
			if(bundle->origin() != NULL_STR)
				fprintf(stdout, "origin:       %s\n", bundle->origin().c_str());

			for(auto const& grammar : bundle->grammars())
			{
				std::vector<std::string> fileTypes;
				for(auto const& ext : grammar->file_types())
					fileTypes.push_back(ext);
				if(grammar->mode_line() != NULL_STR)
					fileTypes.push_back(text::format("/%s/", grammar->mode_line().c_str()));
				if(fileTypes.empty())
					fileTypes.push_back(grammar->scope());
				fprintf(stdout, "grammar:      %s (%s)\n", grammar->name().c_str(), text::join(fileTypes, ", ").c_str());
			}

			std::vector<std::string> dependencies;
			for(auto const& dependency : bundle->dependencies(index))
				dependencies.push_back(dependency->name());
			if(!dependencies.empty())
				fprintf(stderr, "dependencies: %s\n", text::join(dependencies, ", ").c_str());

			if(bundle->description() != NULL_STR)
				fprintf(stdout, "description:  %s\n", textify(bundle->description()).c_str());
		}
	}
	else if(command == "update")
	{
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(bundle->has_update())
			{
				fprintf(stderr, "Updating ‘%s’...", bundle->name().c_str());
				if(update(bundle, installDir))
						fprintf(stderr, "ok!\n");
				else	fprintf(stderr, " *** failed!\n");
			}
		}
		save_index(index, installDir);
	}
	else if(command == "dependencies")
	{
		std::vector<bundles_db::bundle_ptr> bundles;
		for(auto const& bundle : filtered_bundles(index, sourceNames, bundleNames))
		{
			if(!bundleNames.empty() || bundle->installed())
				bundles.push_back(bundle);
		}

		for(auto const& bundle : dependencies(index, bundles, false))
			fprintf(stdout, "%s\n", short_bundle_info(bundle, get_width()).c_str());
	}
	else if(command == "dependents")
	{
		for(auto const& bundle : dependents(index, filtered_bundles(index, sourceNames, bundleNames), onlyInstalled))
			fprintf(stdout, "%s\n", short_bundle_info(bundle, get_width()).c_str());
	}
	else
	{
		fprintf(stderr, "unknown command: %s\n", command.c_str());
		return 1;
	}
	return 0;
}
