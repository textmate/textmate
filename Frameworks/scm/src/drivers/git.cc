#include "api.h"
#include <text/tokenize.h>
#include <text/format.h>
#include <io/io.h>
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Git);

static scm::status::type parse_status_flag (std::string const& str)
{
	static auto const StatusLetterConversionMap = new std::map<std::string, scm::status::type>
	{
		{ "?", scm::status::unversioned },
		{ "I", scm::status::ignored     },
		{ "H", scm::status::none        },
		{ "M", scm::status::modified    },
		{ "A", scm::status::added       },
		{ "D", scm::status::deleted     },
		{ "U", scm::status::conflicted  },
		{ "T", scm::status::modified    }  // type change, e.g. symbolic link → regular file
	};

	auto it = StatusLetterConversionMap->find(str);
	if(it != StatusLetterConversionMap->end())
		return it->second;

	ASSERT_EQ(str, NULL_STR); // we use ‘str’ in the assertion to output the unrecognized status flag
	return scm::status::unknown;
}

static void parse_diff (std::map<std::string, scm::status::type>& entries, std::string const& output)
{
	if(output == NULL_STR)
		return;

	auto v = text::tokenize(output.begin(), output.end(), '\0');
	for(auto it = v.begin(); it != v.end() && !(*it).empty(); ++it)
	{
		scm::status::type flag = parse_status_flag(*it);
		if(++it != v.end())
			entries[*it] = flag;
	}
}

static void parse_ls (std::map<std::string, scm::status::type>& entries, std::string const& output, scm::status::type state = scm::status::unknown)
{
	if(output == NULL_STR)
		return;

	for(auto str : text::tokenize(output.begin(), output.end(), '\0'))
	{
		if(str.size() > 1 && str[1] == ' ')
			entries[str.substr(2)] = state != scm::status::unknown ? state : parse_status_flag(str.substr(0, 1));
	}
}

static std::string copy_git_index (std::string const& dir)
{
	std::string gitDir = path::join(dir, ".git");
	struct stat buf;
	if(stat(gitDir.c_str(), &buf) != 0)
		return perror(text::format("*** stat(‘%s’)", dir.c_str()).c_str()), NULL_STR;

	if(S_ISREG(buf.st_mode))
	{
		// Submodules have their git dir in the super project (starting with 1.7.6) — if we know the user has 1.7.6 we should ask git for the git dir using: git rev-parse --resolve-git-dir /path/to/repository/.git
		std::string const setting = path::content(gitDir);
		if(setting.find("gitdir: ") == 0 && setting[setting.size()-1] == '\n')
			gitDir = path::join(dir, setting.substr(8, setting.size()-9));
	}

	std::string res = NULL_STR;

	std::string indexPath = path::join(gitDir, "index");
	int src = open(indexPath.c_str(), O_RDONLY|O_CLOEXEC);
	if(src != -1)
	{
		std::string const tmpIndex = path::temp("git-index");
		int dst = open(tmpIndex.c_str(), O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR);
		if(dst != -1)
		{
			if(fcopyfile(src, dst, NULL, COPYFILE_ALL | COPYFILE_NOFOLLOW_SRC) != -1)
			{
				res = tmpIndex;
			}
			else
			{
				perror(text::format("copyfile(\"%s\", \"%s\")", indexPath.c_str(), tmpIndex.c_str()).c_str());
				if(unlink(tmpIndex.c_str()) == -1)
					perror(text::format("unlink(\"%s\")", tmpIndex.c_str()).c_str());
			}
			close(dst);
		}
		else
		{
			perror(text::format("open(\"%s\", O_CREAT|O_TRUNC|O_WRONLY|O_CLOEXEC, S_IRUSR|S_IWUSR)", tmpIndex.c_str()).c_str());
		}
		close(src);
	}
	else
	{
		perror(text::format("open(\"%s\", O_RDONLY|O_CLOEXEC)", indexPath.c_str()).c_str());
	}
	return res;
}

static void collect_all_paths (std::string const& git, std::map<std::string, scm::status::type>& entries, std::string const& dir)
{
	ASSERT_NE(git, NULL_STR);

	std::map<std::string, std::string> env = oak::basic_environment();
	env["GIT_WORK_TREE"] = dir;
	env["GIT_DIR"]       = path::join(dir, ".git");

	bool haveHead = io::exec(env, git, "show-ref", "-qh", NULL) != NULL_STR;

	std::string const tmpIndex = copy_git_index(dir);
	if(tmpIndex != NULL_STR)
	{
		env["GIT_INDEX_FILE"] = tmpIndex;
		io::exec(env, git, "update-index", "-q", "--unmerged", "--ignore-missing", "--refresh", NULL);

		// All files part of the repository (index)
		if(haveHead)
				parse_ls(entries, io::exec(env, git, "ls-files", "--exclude-standard", "-zt", NULL));
		else	parse_ls(entries, io::exec(env, git, "ls-files", "--exclude-standard", "-zt", NULL), scm::status::added);

		// Modified, Deleted (on disk, not staged)
		parse_diff(entries, io::exec(env, git, "diff-files", "--name-status", "--ignore-submodules=dirty", "-z", NULL));

		// Added (to index), Deleted (from index)
		if(haveHead)
			parse_diff(entries, io::exec(env, git, "diff-index", "--name-status", "--ignore-submodules=dirty", "-z", "--cached", "HEAD", NULL));
	}

	// All files with ‘other’ status
	parse_ls(entries, io::exec(env, git, "ls-files", "--exclude-standard", "-zto", NULL));

	path::remove(tmpIndex);
}

namespace
{
	struct entry_t
	{
	private:
		struct helper_t
		{
			helper_t (std::map<std::string, scm::status::type> const& entries) : _entries(entries) { }
			std::map<std::string, scm::status::type> _entries;
		};
		typedef std::shared_ptr<helper_t> helper_ptr;
		helper_ptr _helper;
		std::string _key;
		bool _is_dir;

		entry_t (helper_ptr helper, std::string const& key, bool is_dir) : _helper(helper), _key(key), _is_dir(is_dir) { }

	public:
		entry_t (std::map<std::string, scm::status::type> const& entries) : _key(""), _is_dir(true) { _helper = std::make_shared<helper_t>(entries); }

		entry_t operator[] (std::string const& path) { return entry_t(_helper, path, _helper->_entries.find(path) == _helper->_entries.end()); }
		bool is_dir () const                         { return _is_dir; }
		scm::status::type& status ()                 { return _helper->_entries[_key]; }
		scm::status::type const& status () const     { ASSERT(!_is_dir); ASSERT(_helper->_entries.find(_key) != _helper->_entries.end()); return _helper->_entries.find(_key)->second; }
		std::string path () const                    { return _key; }

		std::vector<entry_t> entries () const
		{
			std::map<std::string, bool> tmp;
			std::string const base = _key == "" ? _key : _key + "/";
			foreach(it, _helper->_entries.lower_bound(base), _helper->_entries.lower_bound(base + '\xFF'))
			{
				std::string const path = it->first.substr(base.length());
				std::string::size_type const sep = path.find('/');
				tmp.emplace(path.substr(0, sep), sep != std::string::npos);
			}

			std::vector<entry_t> res;
			iterate(pair, tmp)
				res.push_back(entry_t(_helper, base + pair->first, pair->second));
			return res;
		}
	};
}

static scm::status::type status_for (entry_t const& root)
{
	if(!root.is_dir())
		return root.status();

	size_t untracked = 0, ignored = 0, tracked = 0, modified = 0, added = 0, deleted = 0, mixed = 0, conflicted = 0;
	citerate(entry, root.entries())
	{
		switch(status_for(*entry))
		{
			case scm::status::conflicted:   ++conflicted;break;
			case scm::status::unversioned:  ++untracked; break;
			case scm::status::ignored:      ++ignored;   break;
			case scm::status::none:         ++tracked;   break;
			case scm::status::modified:     ++modified;  break;
			case scm::status::added:        ++added;     break;
			case scm::status::deleted:      ++deleted;   break;
			case scm::status::mixed:        ++mixed;     break;
		}
	}
	
	if(conflicted > 0) return scm::status::conflicted;
	if(mixed > 0) return scm::status::mixed;
	
	size_t total = untracked + ignored + tracked + modified + added + deleted + conflicted;
	
	if(total == conflicted)return scm::status::conflicted;
	if(total == untracked) return scm::status::unversioned;
	if(total == ignored)   return scm::status::none;
	if(total == tracked)   return scm::status::none;
	if(total == modified)  return scm::status::modified;
	if(total == added)     return scm::status::added;
	if(total == deleted)   return scm::status::deleted;
	
	if(total > 0) return scm::status::mixed;
	
	return scm::status::none;
}

static void filter (scm::status_map_t& statusMap, entry_t const& root, std::string const& base)
{
	citerate(entry, root.entries())
	{
		scm::status::type status = status_for(*entry);
		statusMap.emplace(path::join(base, entry->path()), status);
		if(entry->is_dir() && status != scm::status::ignored)
			filter(statusMap, (*entry)[entry->path()], base);
	}
}

namespace scm
{
	struct git_driver_t : driver_t
	{
		git_driver_t () : driver_t("git", "%s/.git", "git") { }

		std::map<std::string, std::string> variables (std::string const& wcPath) const
		{
			D(DBF_SCM_Git, bug("%s\n", wcPath.c_str()););
			std::map<std::string, std::string> res = { { "TM_SCM_NAME", name() } };
			if(executable() != NULL_STR)
			{
				std::map<std::string, std::string> env = oak::basic_environment();
				env["GIT_WORK_TREE"] = wcPath;
				env["GIT_DIR"]       = path::join(wcPath, ".git");

				bool haveHead = io::exec(env, executable(), "show-ref", "-qh", NULL) != NULL_STR;
				if(haveHead)
				{
					std::string branchName = io::exec(env, executable(), "symbolic-ref", "HEAD", NULL);
					if(branchName.find("refs/heads/") == 0)
					{
						branchName = branchName.substr(11);
						branchName = branchName.substr(0, branchName.find("\n"));
						res.emplace("TM_SCM_BRANCH", branchName);
					}
				}
			}
			return res;
		}

		status_map_t status (std::string const& wcPath) const
		{
			D(DBF_SCM_Git, bug("%s\n", wcPath.c_str()););
			if(executable() == NULL_STR)
				return status_map_t();

			std::map<std::string, scm::status::type> tmp;
			collect_all_paths(executable(), tmp, wcPath);

			scm::status_map_t statusMap;
			filter(statusMap, entry_t(tmp), wcPath);

			return statusMap;
		}
	};

	driver_t* git_driver () { return new git_driver_t; }
}
