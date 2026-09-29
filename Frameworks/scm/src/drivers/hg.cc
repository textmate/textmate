#include "api.h"
#include <oak/oak.h>
#include <text/src/parse.h>
#include <io/src/io.h>
#include <oak/debug.h>

static scm::status::type parse_status_flag (char flag)
{
	static struct { scm::status::type constant; char flag; } const StatusLetterConversionMap[] =
	{
		{ scm::status::unversioned, '?' },
		{ scm::status::ignored,     'I' },
		{ scm::status::none,        'C' },
		{ scm::status::modified,    'M' },
		{ scm::status::added,       'A' },
		{ scm::status::deleted,     'R' },
		{ scm::status::deleted,     '!' }, // missing on disk
	};

	for(auto const& status : StatusLetterConversionMap)
	{
		if(flag == status.flag)
			return status.constant;
	}

	ASSERT_EQ(flag, '\0'); // we use ‘flag’ in the assertion to output the unrecognized status flag
	return scm::status::unknown;
}

static void parse_status_output (scm::status_map_t& entries, std::string const& output)
{
	if(output == NULL_STR)
		return;

	std::vector<std::string> v = text::split(output, std::string(1, '\0'));
	ASSERT_EQ(v.back(), "");
	v.pop_back();
	for(auto const& line : v)
	{
		ASSERT_EQ(line[1], ' ');
		entries[line.substr(2)] = parse_status_flag(line[0]);
	}
}

static void collect_all_paths (std::string const& hg, scm::status_map_t& entries, std::string const& dir)
{
	ASSERT_NE(hg, NULL_STR);
	parse_status_output(entries, io::exec(hg, "status", "--cwd", dir.c_str(), "--all", "-0", nullptr));
}

namespace scm
{
	struct hg_driver_t : driver_t
	{
		hg_driver_t () : driver_t("hg", "%s/.hg", "hg") { }

		bool may_touch_filesystem () const { return true; }

		std::map<std::string, std::string> variables (std::string const& wcPath) const
		{
			std::map<std::string, std::string> res = { { "TM_SCM_NAME", name() } };
			if(executable() != NULL_STR)
			{
				std::string branchName = io::exec(executable(), "branch", "--cwd", wcPath.c_str(), nullptr);
				res.emplace("TM_SCM_BRANCH", branchName.substr(0, branchName.find("\n")));
			}
			return res;
		}

		status_map_t status (std::string const& wcPath) const
		{
			if(executable() == NULL_STR)
				return status_map_t();

			status_map_t relativePaths, res;
			collect_all_paths(executable(), relativePaths, wcPath);
			for(auto const& pair : relativePaths)
				res.emplace(path::join(wcPath, pair.first), pair.second);
			return res;
		}
	};

	driver_t* hg_driver () { return new hg_driver_t; }
}
