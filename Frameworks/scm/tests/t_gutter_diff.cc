#include "../src/gutter_diff.h"

using scm::gutter_diff::change;
using scm::gutter_diff::diff_bytes;

void test_diff_no_change ()
{
	auto r = diff_bytes("a\nb\nc\n", "a\nb\nc\n");
	OAK_ASSERT(r.empty());
}

void test_diff_pure_addition ()
{
	auto r = diff_bytes("a\nb\n", "a\nNEW\nb\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[2], change::added);
}

void test_diff_pure_modification ()
{
	auto r = diff_bytes("a\nb\n", "a\nB\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[2], change::modified);
}

void test_diff_pure_deletion ()
{
	auto r = diff_bytes("a\nb\nc\n", "a\nc\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[1], change::deleted);
}

void test_diff_mixed_hunk_balanced_replace ()
{
	// Balanced: -x +X -y +Y → both new lines are modifications.
	auto r = diff_bytes("a\nx\ny\nz\n", "a\nX\nY\nz\n");
	OAK_ASSERT_EQ(r.size(), 2);
	OAK_ASSERT_EQ(r[2], change::modified);
	OAK_ASSERT_EQ(r[3], change::modified);
}

void test_diff_mixed_hunk_more_added_than_deleted ()
{
	// Replace one line with two: first new line modifies the deleted
	// one (paired), the extra one is a pure addition.
	auto r = diff_bytes("a\nb\nz\n", "a\nB1\nB2\nz\n");
	OAK_ASSERT_EQ(r.size(), 2);
	OAK_ASSERT_EQ(r[2], change::modified);
	OAK_ASSERT_EQ(r[3], change::added);
}

void test_diff_mixed_hunk_more_deleted_than_added ()
{
	// Replace two lines with one: first new line modifies the first
	// deleted (paired), the second deletion is unpaired and emits a
	// `deleted` mark on the line before the deletion site.
	auto r = diff_bytes("a\nb\nc\nz\n", "a\nB\nz\n");
	OAK_ASSERT_EQ(r.size(), 2);
	OAK_ASSERT_EQ(r[2], change::modified);
	OAK_ASSERT_EQ(r[2], change::modified);  // line 2 is the modification site
	// Unpaired deletion appears at the next gutter line — line before
	// the surviving `z`. With one trailing `-c`, that's line 2 already
	// taken by the modification, so the deleted mark falls at line 3
	// (the line where deletion landed in the new file's frame).
	OAK_ASSERT_EQ(r[3], change::deleted);
}

void test_diff_addition_at_start ()
{
	auto r = diff_bytes("a\nb\n", "FIRST\na\nb\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[1], change::added);
}

void test_diff_addition_at_end ()
{
	auto r = diff_bytes("a\nb\n", "a\nb\nLAST\n");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[3], change::added);
}

void test_diff_empty_old_all_added ()
{
	auto r = diff_bytes("", "a\nb\nc\n");
	OAK_ASSERT_EQ(r.size(), 3);
	OAK_ASSERT_EQ(r[1], change::added);
	OAK_ASSERT_EQ(r[2], change::added);
	OAK_ASSERT_EQ(r[3], change::added);
}

void test_diff_empty_old_indented_lines_are_consecutive ()
{
	auto r = diff_bytes("", "if(true)\n{\n    a();\n    b();\n}\n");
	OAK_ASSERT_EQ(r.size(), 5);
	OAK_ASSERT_EQ(r[1], change::added);
	OAK_ASSERT_EQ(r[2], change::added);
	OAK_ASSERT_EQ(r[3], change::added);
	OAK_ASSERT_EQ(r[4], change::added);
	OAK_ASSERT_EQ(r[5], change::added);
}

void test_diff_empty_new_all_deleted ()
{
	// Every line gone. Mark line 1 as deleted (best we can do with
	// no surviving lines).
	auto r = diff_bytes("a\nb\nc\n", "");
	OAK_ASSERT_EQ(r.size(), 1);
	OAK_ASSERT_EQ(r[1], change::deleted);
}

void test_diff_trailing_newline_added ()
{
	// Adding a final newline to a file that lacked one is not a
	// content change worth flagging in the gutter.
	auto r = diff_bytes("a\nb", "a\nb\n");
	OAK_ASSERT(r.empty());
}

void test_diff_multiple_hunks ()
{
	auto r = diff_bytes(
		"a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\n",
		"a\nB\nc\nd\ne\nf\ng\nH\ni\nj\nk\n");
	OAK_ASSERT_EQ(r.size(), 2);
	OAK_ASSERT_EQ(r[2], change::modified);
	OAK_ASSERT_EQ(r[8], change::modified);
}
