#include "api.h"
#include <oak/oak.h>
#include <text/format.h>
#include <text/parse.h>
#include <text/trim.h>
#include <text/tokenize.h>
#include <io/io.h>
#include <cf/cf.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Svn);

static scm::status::type parse_status_string (std::string const& status)
{
	// Based on subversion/svn/status.c (generate_status_desc)
	static std::map<std::string, scm::status::type> const StatusMap
	{
		{ "none",        scm::status::versioned   },
		{ "normal",      scm::status::versioned   },
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

	auto it = StatusMap.find(status);
	return it != StatusMap.end() ? it->second : scm::status::none;
}

static void parse_status_output (scm::status_map_t& entries, std::string const& output)
{
	citerate(line, text::tokenize(output.begin(), output.end(), '\n'))
	{
		// Massaged Subversion output is as follows: 'FILE_PATH\0FILE_STATUS\0FILE_PROPS_STATUS'
		std::vector<std::string> cols = text::split((*line), std::string(1, '\0'));
		if(cols.size() == 3)
		{
			std::string const& file_path         = cols[0];
			std::string const& file_status       = cols[1];
			std::string const& file_props_status = cols[2];

			// If the file's status is not normal/none, use the file's status, otherwise use the file's property status
			if(file_status != "normal" || file_status != "none")
					entries[file_path] = parse_status_string(file_status);
			else	entries[file_path] = parse_status_string(file_props_status);
		}
	}
}

static std::map<std::string, std::string> parse_info_output (std::string const& str)
{
	std::map<std::string, std::string> res;
	citerate(line, text::tokenize(str.begin(), str.end(), '\n'))
	{
		std::string::size_type n = (*line).find(':');
		if(n != std::string::npos)
			res.insert(std::make_pair((*line).substr(0, n), (*line).substr(n+2)));
	}
	return res;
}

static void collect_all_paths (std::string const& svn, std::string const& xsltPath, scm::status_map_t& entries, std::string const& dir)
{
	ASSERT_NE(svn, NULL_STR); ASSERT_NE(xsltPath, NULL_STR);

	std::map<std::string, std::string> env = oak::basic_environment();
	// Parses Subversion's response XML and outputs 'FILE_PATH\0FILE_STATUS\0FILE_PROPS_STATUS'
	std::string python_cmd = "import sys, xml.dom.minidom\n"
		"for node in xml.dom.minidom.parse(sys.stdin).getElementsByTagName('entry'):\n"
		"  path = node.getAttribute('path')\n"
		"  wc_status = node.getElementsByTagName('wc-status')[0]\n"
		"  print('%s\\0%s\\0%s' % (path, wc_status.getAttribute('item'), wc_status.getAttribute('props')))";
	// Subversion status command that is verbose (necessary to get status for unchanged paths) and as XML
	std::string svn_status_cmd = text::format("%s status --no-ignore --xml -v", svn.c_str());

	env["PWD"] = dir;

	parse_status_output(entries, io::exec(env, "/bin/bash", "-c", text::format("%s | python -c \"%s\"", svn_status_cmd.c_str(), python_cmd.c_str()).c_str(), NULL));
}

namespace scm
{
	struct svn_driver_t : driver_t
	{
		svn_driver_t () : driver_t("svn", "%s/.svn", "svn")
		{
			if(CFBundleRef bundle = CFBundleGetBundleWithIdentifier(CFSTR("com.macromates.TextMate.scm")))
			{
				if(CFURLRef xsltURL = CFBundleCopyResourceURL(bundle, CFSTR("svn_status"), CFSTR("xslt"), NULL))
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
				fprintf(stderr, "TextMate/svn: Unable to locate ‘svn_status.xslt’.\n");
		}

		std::string branch_name (std::string const& wcPath) const
		{
			if(executable() == NULL_STR)
				return NULL_STR;

			std::map<std::string, std::string> env = oak::basic_environment();
			env["PWD"] = wcPath;

			auto info = parse_info_output(io::exec(env, executable(), "info", NULL));
			auto urlInfo = info.find("URL");
			return urlInfo != info.end() ? urlInfo->second : NULL_STR;
		}

		status_map_t status (std::string const& wcPath) const
		{
			D(DBF_SCM_Svn, bug("%s\n", wcPath.c_str()););
			if(executable() == NULL_STR || _xslt_path == NULL_STR)
				return status_map_t();

			status_map_t relativePaths, res;
			collect_all_paths(executable(), _xslt_path, relativePaths, wcPath);
			iterate(pair, relativePaths)
				res.insert(std::make_pair(path::join(wcPath, pair->first), pair->second));
			return res;
		}

	private:
		std::string _xslt_path = NULL_STR;
	};

	driver_t* svn_driver () { return new svn_driver_t; }
}
