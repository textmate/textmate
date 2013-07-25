#include <test/jail.h>
#include "../src/scan_path.h"
using namespace find;

static void run_scanner (scan_path_t& scanner)
{
	scanner.start();
	while(scanner.is_running())
		sleep(0);
}

void test_simple ()
{
	test::jail_t jail;
	jail.set_content("matches", "text");
	jail.touch("dummy");

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(folder_scan_settings_t(jail.path()));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 2);
	OAK_ASSERT_EQ(matches.begin()->first->path(), jail.path("matches"));
}

void test_globs ()
{
	test::jail_t jail;
	jail.set_content("text.x", "text");
	jail.set_content("text.y", "text");
	jail.set_content("text.z", "dsalkdalsjas");

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(folder_scan_settings_t(jail.path(), "*.{x,z}"));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 2);
	OAK_ASSERT_EQ(matches.begin()->first->path(), jail.path("text.x"));
}

void test_exclude_globs ()
{
	test::jail_t jail;
	jail.set_content("text.x", "text");
	jail.set_content("text.y", "text");
	jail.set_content("text.z", "text");

	path::glob_list_t globs;
	globs.add_exclude_glob("*.y");
	globs.add_include_glob("*");

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(folder_scan_settings_t(jail.path(), globs));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 2);
	OAK_ASSERT_EQ(matches.size(), 2);
	OAK_ASSERT_EQ(matches[0].first->path(), jail.path("text.x"));
	OAK_ASSERT_EQ(matches[1].first->path(), jail.path("text.z"));
}

void test_ignore_hidden ()
{
	test::jail_t jail;
	jail.set_content("visible", "text");
	jail.set_content(".hidden/hidden", "text");

	scan_path_matches_t matches;
	folder_scan_settings_t search(jail.path());

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(search);
	run_scanner(scanner);

	matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 1);

	scan_path_t hidden_scanner;
	hidden_scanner.set_string("text");
	search.globs.add_include_glob(".*", path::kPathItemDirectory);
	hidden_scanner.set_folder_options(search);
	run_scanner(hidden_scanner);

	matches = hidden_scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 2);
	OAK_ASSERT_EQ(hidden_scanner.get_scanned_file_count(), 2);
}

void test_follow_links ()
{
	test::jail_t jail;
	jail.touch("start/foo.txt");
	jail.set_content("linked/match.txt", "text");
	jail.ln("start/link", "linked");

	scan_path_matches_t matches;
	folder_scan_settings_t search(jail.path("start"));

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(search);
	run_scanner(scanner);

	matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 0);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 1);

	scan_path_t follow_scanner;
	search.follow_links = true;
	follow_scanner.set_string("text");
	follow_scanner.set_folder_options(search);
	run_scanner(follow_scanner);

	matches = follow_scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(follow_scanner.get_scanned_file_count(), 2);
}

void test_file_links_are_skipped ()
{
	test::jail_t jail;
	jail.set_content("match", "text");
	jail.ln("link", "match");

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(folder_scan_settings_t(jail.path()));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 1);
}

void test_duplicate_links ()
{
	test::jail_t jail;
	jail.set_content("dir/match.txt", "text");
	jail.ln("link", "dir");

	folder_scan_settings_t search(jail.path());
	search.follow_links = true;

	scan_path_t scanner;
	scanner.set_string("text");
	scanner.set_folder_options(search);
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 1);
	OAK_ASSERT_EQ(scanner.get_scanned_file_count(), 1);
}

void test_file_lf ()
{
	test::jail_t jail;
	jail.set_content("match", "line 1\nline 2\nline 3\nline 4\n");

	scan_path_t scanner;
	scanner.set_string("line ");
	scanner.set_folder_options(folder_scan_settings_t(jail.path()));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 4);

	OAK_ASSERT_EQ(matches[0].second.range.min().line, 0);
	OAK_ASSERT_EQ(matches[1].second.range.min().line, 1);
	OAK_ASSERT_EQ(matches[2].second.range.min().line, 2);
	OAK_ASSERT_EQ(matches[3].second.range.min().line, 3);
}

void test_file_cr ()
{
	test::jail_t jail;
	jail.set_content("match", "line 1\rline 2\rline 3\rline 4\r");

	scan_path_t scanner;
	scanner.set_string("line ");
	scanner.set_folder_options(folder_scan_settings_t(jail.path()));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 4);

	OAK_ASSERT_EQ(matches[0].second.range.min().line, 0);
	OAK_ASSERT_EQ(matches[1].second.range.min().line, 1);
	OAK_ASSERT_EQ(matches[2].second.range.min().line, 2);
	OAK_ASSERT_EQ(matches[3].second.range.min().line, 3);
}

void test_file_crlf ()
{
	test::jail_t jail;
	jail.set_content("match", "line 1\r\nline 2\r\nline 3\r\nline 4\r\n");

	scan_path_t scanner;
	scanner.set_string("line ");
	scanner.set_folder_options(folder_scan_settings_t(jail.path()));
	run_scanner(scanner);

	scan_path_matches_t matches = scanner.accept_matches();
	OAK_ASSERT_EQ(matches.size(), 4);

	OAK_ASSERT_EQ(matches[0].second.range.min().line, 0);
	OAK_ASSERT_EQ(matches[1].second.range.min().line, 1);
	OAK_ASSERT_EQ(matches[2].second.range.min().line, 2);
	OAK_ASSERT_EQ(matches[3].second.range.min().line, 3);
}
