#include "git_parse.h"
#include <text/tokenize.h>
#include <oak/debug.h>

namespace
{
	bool is_unmerged (char x, char y)
	{
		return x == 'U' || y == 'U' || (x == 'A' && y == 'A') || (x == 'D' && y == 'D');
	}

	scm::status::type status_for (char ch)
	{
		switch(ch)
		{
			case ' ': return scm::status::none;
			case '?': return scm::status::unversioned;
			case '!': return scm::status::ignored;
			case 'M': return scm::status::modified;
			case 'A': return scm::status::added;
			case 'D': return scm::status::deleted;
			case 'T': return scm::status::modified;
			case 'R': return scm::status::modified;
			case 'C': return scm::status::added;
			case 'U': return scm::status::conflicted;
		}

		os_log_error(OS_LOG_DEFAULT, "Unrecognized git porcelain status flag: ‘%{public}c’", ch);
		return scm::status::none;
	}
}

namespace scm::git
{
	scm::status::type resolve_porcelain_xy (char indexStatus, char workTreeStatus)
	{
		if(is_unmerged(indexStatus, workTreeStatus))
			return scm::status::conflicted;

		if(indexStatus != ' ')
			return status_for(indexStatus);

		return status_for(workTreeStatus);
	}

	void parse_porcelain (std::map<std::string, scm::status::type>& entries, std::string const& output)
	{
		if(output == NULL_STR)
			return;

		auto fields = text::tokenize(output.begin(), output.end(), '\0');
		for(auto it = fields.begin(); it != fields.end() && !(*it).empty(); ++it)
		{
			std::string const& field = *it;
			if(field.size() < 4 || field[2] != ' ')
			{
				os_log_error(OS_LOG_DEFAULT, "Unrecognized git porcelain record: ‘%{public}s’", field.c_str());
				continue;
			}

			char const x = field[0];
			char const y = field[1];
			std::string const path = field.substr(3);

			entries[path] = resolve_porcelain_xy(x, y);

			// With porcelain v1 -z, rename/copy records are "XY new-path\0old-path\0".
			if((x == 'R' || x == 'C') && std::next(it) != fields.end())
				++it;
		}
	}
}
