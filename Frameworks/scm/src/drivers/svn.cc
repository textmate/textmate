#include "api.h"
#include <oak/oak.h>
#include <text/parse.h>
#include <text/trim.h>
#include <io/io.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Svn);

static scm::status::type parse_status_flag (char flag)
{
	static struct { scm::status::type constant; char flag; } const StatusLetterConversionMap[] =
	{
		{ scm::status::added,       'A' },
		{ scm::status::conflicted,  'C' },
		{ scm::status::deleted,     'D' },
		{ scm::status::ignored,     'I' },
		{ scm::status::modified,    'M' },
		{ scm::status::modified,    'R' }, // replaced with other version on server (uncommitted)
		{ scm::status::unversioned, '?' },
		{ scm::status::versioned,   ' ' }, // really means none but it implies it's an unchanged, versioned file
		{ scm::status::deleted,     '!' }, // missing on disk
	};

	for(size_t i = 0; i < sizeofA(StatusLetterConversionMap); ++i)
	{
		if(flag == StatusLetterConversionMap[i].flag)
			return StatusLetterConversionMap[i].constant;
	}

	ASSERT_EQ(flag, '\0'); // we use ‘flag’ in the assertion to output the unrecognized status flag
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
		// Subversion's status output uses the first 7 characters for status
		ASSERT((*line).length() > 7);

		std::string rel_path = text::trim(text::split((*line), "   ").back(), " \t\n");

		if((*line)[1] != ' ')
			entries[rel_path] = parse_status_flag((*line)[1]);
		else
			entries[rel_path] = parse_status_flag((*line)[0]);
	}
}

static void collect_all_paths (std::string const& svn, scm::status_map_t& entries, std::string const& dir)
{
	ASSERT_NE(svn, NULL_STR);

	std::map<std::string, std::string> env = oak::basic_environment();
	env["PWD"] = dir;

	// This does not return unmodified, versioned paths
	parse_status_output(entries, io::exec(env, svn, "status", "--no-ignore", "-v", NULL));
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

			std::string info_output = io::exec(env, executable(), "info", NULL);
			std::vector<std::string> v = text::split(info_output, "\n");

			return text::split(v[2], ": ")[1];
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
