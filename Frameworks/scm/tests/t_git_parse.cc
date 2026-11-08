#include "../src/drivers/git_parse.h"

void test_porcelain_worktree_status ()
{
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy(' ', 'M'), scm::status::modified);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy(' ', 'D'), scm::status::deleted);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('?', '?'), scm::status::unversioned);
}

void test_porcelain_index_status_takes_precedence ()
{
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('A', ' '), scm::status::added);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('M', 'D'), scm::status::modified);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('D', ' '), scm::status::deleted);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('R', ' '), scm::status::modified);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('C', ' '), scm::status::added);
}

void test_porcelain_conflicts ()
{
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('U', 'U'), scm::status::conflicted);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('A', 'A'), scm::status::conflicted);
	OAK_ASSERT_EQ(scm::git::resolve_porcelain_xy('D', 'D'), scm::status::conflicted);
}

void test_parse_porcelain_records ()
{
	std::map<std::string, scm::status::type> entries;
	std::string output;
	for(std::string const& record : { " M modified.txt", "A  added.txt", "?? untracked.txt", "R  renamed.txt", "old.txt" })
	{
		output += record;
		output.push_back('\0');
	}
	scm::git::parse_porcelain(entries, output);

	OAK_ASSERT_EQ(entries["modified.txt"], scm::status::modified);
	OAK_ASSERT_EQ(entries["added.txt"], scm::status::added);
	OAK_ASSERT_EQ(entries["untracked.txt"], scm::status::unversioned);
	OAK_ASSERT_EQ(entries["renamed.txt"], scm::status::modified);
	OAK_ASSERT(entries.find("old.txt") == entries.end());
}
