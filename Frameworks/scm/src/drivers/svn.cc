#include "api.h"
#include <oak/oak.h>
#include <text/format.h>
#include <text/parse.h>
#include <text/trim.h>
#include <text/tokenize.h>
#include <io/io.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Svn);

static scm::status::type parse_status_string (std::string status)
{
	// Based on subversion/svn/status.c (generate_status_desc)
	if(status == "none" || status == "normal")
		return scm::status::versioned;
	else if(status == "added")
		return scm::status::added;
	else if(status == "missing" || status == "incomplete" || status == "deleted")
		return scm::status::deleted;
	else if(status == "replaced" || status == "modified")
		return scm::status::modified;
	else if(status == "conflicted" || status == "obstructed")
		return scm::status::conflicted;
	else if(status == "ignored")
		return scm::status::ignored;
	else if(status == "external") // No good way to represent external status so default to 'none'
		return scm::status::none;
	else if(status == "unversioned")
		return scm::status::unversioned;
	else
		return scm::status::none;
}

static void parse_status_output (scm::status_map_t& entries, std::string const& output)
{
	if(output == NULL_STR)
		return;

	std::vector<std::string> v = text::split(output, "\n");
	ASSERT_EQ(v.back(), "");
	v.pop_back();
	iterate(line, v)
	{
		// Massaged Subversion output is as follows: 'FILE_PATH    FILE_STATUS    FILE_PROPS_STATUS'
		std::vector<std::string> v2 = text::split((*line), "    ");
		ASSERT(v2.size() > 3);
		std::string file_path = v2[0];
		std::string file_status = v2[1];
		std::string file_props_status = v2[2];

		// If the file's status is not normal/none, use the file's status, otherwise use the file's property status
		if(file_status != "normal" || file_status != "none")
			entries[file_path] = parse_status_string(file_status);
		else
			entries[file_path] = parse_status_string(file_props_status);
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

static void collect_all_paths (std::string const& svn, scm::status_map_t& entries, std::string const& dir)
{
	ASSERT_NE(svn, NULL_STR);

	std::map<std::string, std::string> env = oak::basic_environment();
	// Parses Subversion's response XML and outputs 'FILE_PATH    FILE_STATUS    FILE_PROPS_STATUS'
	std::string python_cmd = "import sys, xml.dom.minidom\n"
		"for node in xml.dom.minidom.parse(sys.stdin).getElementsByTagName('entry'):\n"
		"  path = node.getAttribute('path')\n"
		"  wc_status = node.getElementsByTagName('wc-status')[0]\n"
		"  print('%s    %s    %s' % (path, wc_status.getAttribute('item'), wc_status.getAttribute('props')))";
	// Subversion status command that is verbose (necessary to get status for unchanged paths) and as XML
	std::string svn_status_cmd = text::format("%s status --no-ignore --xml -v", svn.c_str());

	env["PWD"] = dir;

	parse_status_output(entries, io::exec(env, "/bin/bash", "-c", text::format("%s | python -c \"%s\"", svn_status_cmd.c_str(), python_cmd.c_str()).c_str(), NULL));
}

namespace scm
{
	struct svn_driver_t : driver_t
	{
		svn_driver_t () : driver_t("svn", "%s/.svn", "svn") { }

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
			if(executable() == NULL_STR)
				return status_map_t();

			status_map_t relativePaths, res;
			collect_all_paths(executable(), relativePaths, wcPath);
			iterate(pair, relativePaths)
				res.insert(std::make_pair(path::join(wcPath, pair->first), pair->second));
			return res;
		}
	};

	driver_t* svn_driver () { return new svn_driver_t; }
}
