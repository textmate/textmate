#include "api.h"
#include <oak/oak.h>
#include <text/src/format.h>
#include <text/src/parse.h>
#include <text/src/trim.h>
#include <text/src/tokenize.h>
#include <regexp/src/format_string.h>
#include <io/src/io.h>
#include <cf/src/cf.h>
#include <oak/debug.h>

static scm::status::type parse_status_string (std::string const& status)
{
	// Based on subversion/svn/status.c (generate_status_desc)
	static auto const StatusMap = new std::map<std::string, scm::status::type>
	{
		{ "none",        scm::status::none        },
		{ "normal",      scm::status::none        },
		{ "added",       scm::status::added       },
		{ "missing",     scm::status::deleted     },
		{ "incomplete",  scm::status::deleted     },
		{ "deleted",     scm::status::deleted     },
		{ "replaced",    scm::status::modified    },
		{ "modified",    scm::status::modified    },
		{ "conflicted",  scm::status::conflicted  },
		{ "obstructed",  scm::status::conflicted  },
		{ "ignored",     scm::status::ignored     },
		{ "external",    scm::status::none        }, // No good way to represent external status so default to 'none'
		{ "unversioned", scm::status::unversioned },
	};

	auto it = StatusMap->find(status);
	return it != StatusMap->end() ? it->second : scm::status::unknown;
}

static void parse_status_output (scm::status_map_t& entries, std::string const& output, std::string const& dir)
{
	for(auto const& line : text::tokenize(output.begin(), output.end(), '\n'))
	{
		// Massaged Subversion output is as follows: 'FILE_STATUS    FILE_PROPS_STATUS    FILE_PATH'
		std::vector<std::string> cols = text::split(line, "    ");
		if(cols.size() == 3)
		{
			std::string const file_status       = cols[0];
			std::string const file_props_status = cols[1];
			std::string const file_path         = path::join(dir, cols[2]);

			// If the file's status is not normal/none, use the file's status, otherwise use the file's property status
			if(file_status != "normal" || file_status != "none")
					entries[file_path] = parse_status_string(file_status);
			else	entries[file_path] = parse_status_string(file_props_status);
		}
		else if(line.size())
		{
			os_log_error(OS_LOG_DEFAULT, "TextMate/svn: Unexpected line: ‘%{public}s’", line.c_str());
		}
	}
}

static std::map<std::string, std::string> parse_info_output (std::string const& str)
{
	std::map<std::string, std::string> res;
	for(auto const& line : text::tokenize(str.begin(), str.end(), '\n'))
	{
		std::string::size_type n = line.find(':');
		if(n != std::string::npos)
			res.emplace(line.substr(0, n), line.substr(n+2));
	}
	return res;
}

static void collect_all_paths (std::string const& svn, std::string const& xsltPath, scm::status_map_t& entries, std::string const& dir)
{
	ASSERT_NE(svn, NULL_STR); ASSERT_NE(xsltPath, NULL_STR);
	std::string const cmd = text::format("cd %s && %s status --no-ignore --xml|/usr/bin/xsltproc %s -", path::escape(dir).c_str(), path::escape(svn).c_str(), path::escape(xsltPath).c_str());
	parse_status_output(entries, io::exec("/bin/sh", "-c", cmd.c_str(), nullptr), dir);
}

namespace scm
{
	struct svn_driver_t : driver_t
	{
		svn_driver_t () : driver_t("svn", "%s/.svn", "svn")
		{
			if(CFBundleRef bundle = CFBundleGetBundleWithIdentifier(CFSTR("com.macromates.TextMate.scm")) ?: CFBundleGetMainBundle())
			{
				if(CFURLRef xsltURL = CFBundleCopyResourceURL(bundle, CFSTR("svn_status"), CFSTR("xslt"), nullptr))
				{
					if(CFStringRef path = CFURLCopyFileSystemPath(xsltURL, kCFURLPOSIXPathStyle))
					{
						_xslt_path = cf::to_s(path);
						CFRelease(path);
					}
					CFRelease(xsltURL);
				}
			}

			// TODO Tests should be linked against the full framework bundle.
			static std::string const SourceTreePath = path::join(path::cwd(), path::join(__FILE__, "../../../resources/svn_status.xslt"));
			if(_xslt_path == NULL_STR && path::exists(SourceTreePath))
				_xslt_path = SourceTreePath;

			if(_xslt_path == NULL_STR)
				os_log_error(OS_LOG_DEFAULT, "Unable to locate ‘svn_status.xslt’.");
		}

		std::map<std::string, std::string> variables (std::string const& wcPath) const
		{
			std::map<std::string, std::string> res = { { "TM_SCM_NAME", name() } };
			if(executable() != NULL_STR)
			{
				auto info = parse_info_output(io::exec(executable(), "info", wcPath.c_str(), nullptr));
				auto urlInfo = info.find("URL");
				if(urlInfo != info.end())
					res.emplace("TM_SCM_BRANCH", urlInfo->second);
			}
			return res;
		}

		status_map_t status (std::string const& wcPath) const
		{
			if(executable() == NULL_STR || _xslt_path == NULL_STR)
				return status_map_t();

			status_map_t relativePaths, res;
			collect_all_paths(executable(), _xslt_path, relativePaths, wcPath);
			for(auto const& pair : relativePaths)
				res.emplace(path::join(wcPath, pair.first), pair.second);
			return res;
		}

		bool tracks_directories () const { return true; }

	private:
		std::string _xslt_path = NULL_STR;
	};

	driver_t* svn_driver () { return new svn_driver_t; }
}
