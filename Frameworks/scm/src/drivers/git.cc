#include "api.h"
#include <text/parse.h>
#include <text/format.h>
#include <io/io.h>
#include <oak/oak.h>
#include <oak/debug.h>

OAK_DEBUG_VAR(SCM_Git);

static scm::status::type parse_status_flag (std::string const& str)
{
	static struct { scm::status::type flag; std::string string; } const StatusLetterConversionMap[] =
	{
		{ scm::status::unversioned, "?" },
		{ scm::status::ignored,     "I" },
		{ scm::status::versioned,   "H" },
		{ scm::status::modified,    "M" },
		{ scm::status::added,       "A" },
		{ scm::status::deleted,     "D" },
		{ scm::status::conflicted,  "U" },
		{ scm::status::modified,    "T" }, // type change, e.g. symbolic link → regular file
	};

	for(size_t i = 0; i < sizeofA(StatusLetterConversionMap); ++i)
	{
		if(str == StatusLetterConversionMap[i].string)
			return StatusLetterConversionMap[i].flag;
	}

	ASSERT_EQ(str, NULL_STR); // we use ‘str’ in the assertion to output the unrecognized status flag
	return scm::status::none;
}

static void parse_diff (std::map<std::string, scm::status::type>& entries, std::string const& output)
{
	if(output == NULL_STR)
		return;

	std::vector<std::string> v = text::split(output, std::string(1, 0));
	ASSERT(v.size() % 2 == 1); ASSERT_EQ(v.back(), "");
	v.pop_back();
	for(size_t i = 0; i < v.size(); i += 2)
		entries[v[i+1]] = parse_status_flag(v[i]);
}

static void parse_ls (std::map<std::string, scm::status::type>& entries, std::string const& output, scm::status::type state = scm::status::none)
{
	if(output == NULL_STR)
		return;

	std::vector<std::string> v = text::split(output, std::string(1, 0));
	ASSERT(!v.empty()); ASSERT_EQ(v.back(), "");
	v.pop_back();
	iterate(str, v)
	{
		ASSERT_EQ((*str)[1], ' ');
		entries[str->substr(2)] = state != scm::status::none ? state : parse_status_flag(str->substr(0, 1));
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

	std::string indexPath = path::join(gitDir, "index");
	if(path::exists(indexPath))
	{
		std::string const tmpIndex = path::temp("git-index");
		if(path::copy(indexPath, tmpIndex))
			return tmpIndex;
		path::remove(tmpIndex);
	}
	else
	{
		fprintf(stderr, "*** missing git index: %s\n", indexPath.c_str());
	}
	return NULL_STR;
}

static void collect_all_paths (std::string const& git, std::map<std::string, scm::status::type>& entries, std::string const& dir)
{
	ASSERT_NE(git, NULL_STR);

	std::map<std::string, std::string> env = oak::basic_environment();
	env["PWD"] = dir;

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
		typedef std::tr1::shared_ptr<helper_t> helper_ptr;
		helper_ptr _helper;
		std::string _key;
		bool _is_dir;

		entry_t (helper_ptr helper, std::string const& key, bool is_dir) : _helper(helper), _key(key), _is_dir(is_dir) { }

	public:
		entry_t (std::map<std::string, scm::status::type> const& entries) : _key(""), _is_dir(true) { _helper.reset(new helper_t(entries)); }

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
				tmp.insert(std::make_pair(path.substr(0, sep), sep != std::string::npos));
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

	size_t unknown = 0, ignored = 0, tracked = 0, modified = 0, added = 0, deleted = 0, mixed = 0;
	citerate(entry, root.entries())
	{
		switch(status_for(*entry))
		{
			case scm::status::unversioned:  ++unknown;  break;
			case scm::status::ignored:      ++ignored;  break;
			case scm::status::versioned:    ++tracked;  break;
			case scm::status::modified:     ++modified; break;
			case scm::status::added:        ++added;    break;
			case scm::status::deleted:      ++deleted;  break;
			case scm::status::mixed:        ++mixed;    break;
		}
	}
	
	if(mixed > 0) return scm::status::mixed;
	
	size_t	total=unknown + ignored + tracked + modified + added + deleted;
	
	if(total == unknown)  return scm::status::unversioned;
	if(total == ignored)  return scm::status::none;
	if(total == tracked)  return scm::status::versioned;
	if(total == modified) return scm::status::modified;
	if(total == added)    return scm::status::added;
	if(total == deleted)  return scm::status::deleted;
	
	if(total > 0) return scm::status::mixed;
	
	return scm::status::none;
}

static void filter (scm::status_map_t& statusMap, entry_t const& root, std::string const& base)
{
	citerate(entry, root.entries())
	{
		scm::status::type status = status_for(*entry);
		statusMap.insert(std::make_pair(path::join(base, entry->path()), status));
		if(entry->is_dir() && status != scm::status::ignored)
			filter(statusMap, (*entry)[entry->path()], base);
	}
}

namespace scm
{
	struct git_driver_t : driver_t
	{
		git_driver_t () : driver_t("git", "%s/.git", "git") { }

		std::string branch_name (std::string const& wcPath) const
		{
			if(executable() == NULL_STR)
				return NULL_STR;

			std::map<std::string, std::string> env = oak::basic_environment();
			env["PWD"] = wcPath;

			bool haveHead = io::exec(env, executable(), "show-ref", "-qh", NULL) != NULL_STR;
			if(!haveHead)
				return NULL_STR;

			std::string branchName = io::exec(env, executable(), "symbolic-ref", "HEAD");
			branchName = branchName.substr(0, branchName.find("\n"));
			if(branchName.find("refs/heads/") == 0)
				branchName = branchName.substr(11);
			return branchName;
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
